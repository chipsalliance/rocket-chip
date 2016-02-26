package rocket

import Chisel._
import uncore._
import uncore.DmaRequest._
import junctions.ParameterizedBundle
import cde.Parameters

trait HasClientDmaParameters extends HasCoreParameters with HasDmaParameters {
  val dmaAddrBits = coreMaxAddrBits
  val dmaSegmentSizeBits = coreMaxAddrBits
  val dmaSegmentBits = 24
}

abstract class ClientDmaBundle(implicit val p: Parameters)
  extends ParameterizedBundle()(p) with HasClientDmaParameters
abstract class ClientDmaModule(implicit val p: Parameters)
  extends Module with HasClientDmaParameters

class ClientDmaRequest(implicit p: Parameters) extends ClientDmaBundle()(p) {
  val cmd = UInt(width = DMA_CMD_SZ)
  val src_start  = UInt(width = dmaAddrBits)
  val dst_start  = UInt(width = dmaAddrBits)
  val src_stride = UInt(width = dmaSegmentSizeBits)
  val dst_stride = UInt(width = dmaSegmentSizeBits)
  val segment_size = UInt(width = dmaSegmentSizeBits)
  val nsegments  = UInt(width = dmaSegmentBits)
  val word_size  = UInt(width = dmaWordSizeBits)
}

object ClientDmaRequest {
  def apply(cmd: UInt,
            src_start: UInt,
            dst_start: UInt,
            segment_size: UInt,
            nsegments: UInt = UInt(1),
            src_stride: UInt = UInt(0),
            dst_stride: UInt = UInt(0),
            word_size: UInt = UInt(0))
      (implicit p: Parameters) = {
    val req = Wire(new ClientDmaRequest)
    req.cmd := cmd
    req.src_start := src_start
    req.dst_start := dst_start
    req.src_stride := src_stride
    req.dst_stride := dst_stride
    req.segment_size := segment_size
    req.nsegments := nsegments
    req.word_size := word_size
    req
  }
}

object ClientDmaResponse {
  val pagefault = UInt("b01")
  val invalid_region = UInt("b10")

  def apply(status: UInt = UInt(0))(implicit p: Parameters) = {
    val resp = Wire(new ClientDmaResponse)
    resp.status := status
    resp
  }
}

class ClientDmaResponse(implicit p: Parameters) extends ClientDmaBundle {
  val status = UInt(width = dmaStatusBits)
}

class ClientDmaIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val req = Decoupled(new ClientDmaRequest)
  val resp = Valid(new ClientDmaResponse).flip
}

class DmaFrontend(implicit p: Parameters) extends CoreModule()(p)
    with HasClientDmaParameters with HasTileLinkParameters {
  val io = new Bundle {
    val cpu = (new ClientDmaIO).flip
    val mem = new ClientUncachedTileLinkIO
    val ptw = new TLBPTWIO
    val busy = Bool(OUTPUT)
    val incr_outstanding = Bool(OUTPUT)
    val host_id = UInt(INPUT, log2Up(nCores))
  }

  val tlb = Module(new DecoupledTLB()(p.alterPartial({
    case CacheName => "L1D"
  })))
  io.ptw <> tlb.io.ptw

  private val pgSize = 1 << pgIdxBits

  val cmd = Reg(UInt(width = DMA_CMD_SZ))
  val adv_ptr = MuxLookup(cmd, UInt("b11"), Seq(
    DMA_CMD_PFR -> UInt("b10"),
    DMA_CMD_PFW -> UInt("b10"),
    DMA_CMD_SIN -> UInt("b10"),
    DMA_CMD_SOUT -> UInt("b01")))

  val segment_size = Reg(UInt(width = dmaSegmentSizeBits))
  val bytes_left = Reg(UInt(width = dmaSegmentSizeBits))
  val segments_left = Reg(UInt(width = dmaSegmentBits))
  val word_size = Reg(UInt(width = dmaWordSizeBits))

  val src_vaddr = Reg(UInt(width = dmaAddrBits))
  val dst_vaddr = Reg(UInt(width = dmaAddrBits))
  val src_vpn = src_vaddr(dmaAddrBits - 1, pgIdxBits)
  val dst_vpn = dst_vaddr(dmaAddrBits - 1, pgIdxBits)
  val src_idx = src_vaddr(pgIdxBits - 1, 0)
  val dst_idx = dst_vaddr(pgIdxBits - 1, 0)
  val src_pglen = UInt(pgSize) - src_idx
  val dst_pglen = UInt(pgSize) - dst_idx

  val src_stride = Reg(UInt(width = dmaSegmentSizeBits))
  val dst_stride = Reg(UInt(width = dmaSegmentSizeBits))

  val src_ppn = Reg(UInt(width = ppnBits))
  val dst_ppn = Reg(UInt(width = ppnBits))

  val src_paddr = Cat(src_ppn, src_idx)
  val dst_paddr = Cat(dst_ppn, dst_idx)

  val last_src_vpn = Reg(UInt(width = vpnBits))
  val last_dst_vpn = Reg(UInt(width = vpnBits))

  val tx_len = Util.minUInt(src_pglen, dst_pglen, bytes_left)

  val dma_busy = Reg(init = UInt(0, tlMaxClientXacts))
  val dma_xact_id = PriorityEncoder(~dma_busy)
  val (dma_req_beat, dma_req_done) = Counter(io.mem.acquire.fire(), tlDataBeats)

  val (s_idle :: s_translate :: s_dma_req :: s_dma_update ::
       s_prepare :: s_finish :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_idle)

  // lower bit is for src, higher bit is for dst
  val to_translate = Reg(init = UInt(0, 2))
  val tlb_sent = Reg(init = UInt(0, 2))
  val tlb_to_send = to_translate & ~tlb_sent
  val resp_status = Reg(UInt(width = dmaStatusBits))

  def make_acquire(
      addr_beat: UInt, client_xact_id: UInt, client_id: UInt,
      cmd: UInt, source: UInt, dest: UInt,
      length: UInt, size: UInt): Acquire = {

    val data_blob = Wire(UInt(width = tlDataBeats * tlDataBits))
    data_blob := DmaRequest(
      xact_id = UInt(0),
      client_id = client_id,
      cmd = cmd,
      source = source,
      dest = dest,
      length = length,
      size = size).toBits
    val data_beats = Vec(tlDataBeats, UInt(width = tlDataBits)).fromBits(data_blob)
    val base_addr = addrMap("devices:dma").start
    val addr_block = UInt(base_addr >> (tlBeatAddrBits + tlByteAddrBits))

    PutBlock(
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      data = data_beats(addr_beat),
      alloc = Bool(false))
  }

  def check_region(cmd: UInt, src: UInt, dst: UInt): Bool = {
    val dst_ok = Mux(cmd === DMA_CMD_SOUT, dst >= UInt(mmioBase), dst < UInt(mmioBase))
    val src_ok = Mux(cmd === DMA_CMD_SIN,  src >= UInt(mmioBase), Bool(true))
    dst_ok && src_ok
  }

  tlb.io.req.valid := tlb_to_send.orR
  tlb.io.req.bits.vpn := Mux(tlb_to_send(0), src_vpn, dst_vpn)
  tlb.io.req.bits.passthrough := Bool(false)
  tlb.io.req.bits.instruction := Bool(false)
  tlb.io.req.bits.store := !tlb_to_send(0)
  tlb.io.resp.ready := tlb_sent.orR

  when (tlb.io.req.fire()) {
    tlb_sent := tlb_sent | PriorityEncoderOH(tlb_to_send)
  }

  when (tlb.io.resp.fire()) {
    val recv_choice = PriorityEncoderOH(to_translate)
    val error = Mux(recv_choice(0),
      tlb.io.resp.bits.xcpt_ld, tlb.io.resp.bits.xcpt_st)

    when (error) {
      resp_status := ClientDmaResponse.pagefault
      state := s_finish
    }

    // getting the src translation
    when (recv_choice(0)) {
      src_ppn := tlb.io.resp.bits.ppn
    } .otherwise {
      dst_ppn := tlb.io.resp.bits.ppn
    }

    to_translate := to_translate & ~recv_choice
  }

  io.cpu.req.ready := state === s_idle
  io.cpu.resp.valid := state === s_finish
  io.cpu.resp.bits := ClientDmaResponse(resp_status)

  io.mem.acquire.valid := (state === s_dma_req) && !dma_busy.andR
  io.mem.acquire.bits := make_acquire(
    addr_beat = dma_req_beat,
    client_id = io.host_id,
    client_xact_id = dma_xact_id,
    cmd = cmd, source = src_paddr, dest = dst_paddr,
    length = tx_len, size = word_size)

  io.mem.grant.ready := (state =/= s_dma_req)

  when (io.cpu.req.fire()) {
    val req = io.cpu.req.bits
    val is_prefetch = req.cmd(2, 1) === UInt("b01")
    cmd := req.cmd
    src_vaddr := req.src_start
    dst_vaddr := req.dst_start
    src_stride := req.src_stride
    dst_stride := req.dst_stride
    segment_size := req.segment_size
    segments_left := req.nsegments - UInt(1)
    bytes_left := req.segment_size
    word_size := req.word_size
    to_translate := Mux(is_prefetch, UInt("b10"), UInt("b11"))
    tlb_sent := UInt(0)
    state := s_translate
  }

  when (state === s_translate && !to_translate.orR) {
    when (check_region(cmd, src_paddr, dst_paddr)) {
      state := s_dma_req
    } .otherwise {
      resp_status := ClientDmaResponse.invalid_region
      state := s_finish
    }
  }

  def setBusy(set: Bool, xact_id: UInt): UInt =
    Mux(set, UIntToOH(xact_id), UInt(0))

  dma_busy := (dma_busy |
                setBusy(dma_req_done, dma_xact_id)) &
                ~setBusy(io.mem.grant.fire(), io.mem.grant.bits.client_xact_id)


  when (dma_req_done) {
    src_vaddr := src_vaddr + Mux(adv_ptr(0), tx_len, UInt(0))
    dst_vaddr := dst_vaddr + Mux(adv_ptr(1), tx_len, UInt(0))
    bytes_left := bytes_left - tx_len
    state := s_dma_update
  }

  when (state === s_dma_update) {
    when (bytes_left === UInt(0)) {
      when (segments_left === UInt(0)) {
        resp_status := UInt(0)
        state := s_finish
      } .otherwise {
        last_src_vpn := src_vpn
        last_dst_vpn := dst_vpn
        src_vaddr := src_vaddr + src_stride
        dst_vaddr := dst_vaddr + dst_stride
        bytes_left := segment_size
        segments_left := segments_left - UInt(1)
        state := s_prepare
      }
    } .otherwise {
      to_translate := adv_ptr & Cat(dst_idx === UInt(0), src_idx === UInt(0))
      tlb_sent := UInt(0)
      state := s_translate
    }
  }

  when (state === s_prepare) {
    to_translate := adv_ptr & Cat(
      dst_vpn =/= last_dst_vpn,
      src_vpn =/= last_src_vpn)
    tlb_sent := UInt(0)
    state := s_translate
  }

  when (state === s_finish) { state := s_idle }

  io.busy := (state =/= s_idle) || dma_busy.orR
  io.incr_outstanding := dma_req_done
}

object DmaCtrlRegNumbers {
  val SRC_STRIDE = 0
  val DST_STRIDE = 1
  val SEGMENT_SIZE = 2
  val NSEGMENTS = 3
  val WORD_SIZE = 4
  val RESP_STATUS = 5
  val OUTSTANDING = 6
  val NCSRS = 7
  val CSR_BASE = 0x800
  val CSR_END  = CSR_BASE + NCSRS
}
import DmaCtrlRegNumbers._

class DmaCtrlRegFile(implicit val p: Parameters) extends Module
    with HasClientDmaParameters with HasTileLinkParameters {

  private val nWriteRegs = 5
  private val nRegs = nWriteRegs + 2

  val io = new Bundle {
    val wen = Bool(INPUT)
    val waddr = UInt(INPUT, log2Up(nRegs))
    val wdata = UInt(INPUT, dmaSegmentSizeBits)

    val src_stride = UInt(OUTPUT, dmaSegmentSizeBits)
    val dst_stride = UInt(OUTPUT, dmaSegmentSizeBits)
    val segment_size = UInt(OUTPUT, dmaSegmentSizeBits)
    val nsegments  = UInt(OUTPUT, dmaSegmentBits)
    val word_size = UInt(OUTPUT, dmaWordSizeBits)

    val incr_outstanding = Bool(INPUT)
    val xact_outstanding = Bool(OUTPUT)
  }

  val regs = Reg(Vec(nWriteRegs, UInt(width = dmaSegmentSizeBits)))
  val waddr = io.waddr(log2Up(NCSRS) - 1, 0)

  io.src_stride := regs(SRC_STRIDE)
  io.dst_stride := regs(DST_STRIDE)
  io.segment_size := regs(SEGMENT_SIZE)
  io.nsegments := regs(NSEGMENTS)
  io.word_size := regs(WORD_SIZE)

  when (io.wen && waddr < UInt(nWriteRegs)) {
    regs.write(waddr, io.wdata)
  }

  val outstanding_cnt = TwoWayCounter(
    io.incr_outstanding,
    io.wen && io.waddr === UInt(OUTSTANDING),
    tlMaxClientXacts)

  io.xact_outstanding := outstanding_cnt > UInt(0)
}

class DmaController(implicit p: Parameters) extends RoCC()(p)
    with HasClientDmaParameters {
  io.mem.req.valid := Bool(false)
  io.resp.valid := Bool(false)
  io.interrupt := Bool(false)

  val cmd = Queue(io.cmd)
  val inst = cmd.bits.inst
  val is_transfer = inst.funct < UInt(8)

  val reg_status = Reg(UInt(width = dmaStatusBits))
  val crfile = Module(new DmaCtrlRegFile)
  crfile.io.waddr := io.csr.waddr
  crfile.io.wdata := io.csr.wdata
  crfile.io.wen := io.csr.wen

  io.csr.rdata(SRC_STRIDE) := crfile.io.src_stride
  io.csr.rdata(DST_STRIDE) := crfile.io.dst_stride
  io.csr.rdata(SEGMENT_SIZE) := crfile.io.segment_size
  io.csr.rdata(NSEGMENTS) := crfile.io.nsegments
  io.csr.rdata(WORD_SIZE) := crfile.io.word_size
  io.csr.rdata(RESP_STATUS) := reg_status

  val frontend = Module(new DmaFrontend)
  io.ptw(0) <> frontend.io.ptw
  io.autl <> frontend.io.mem
  crfile.io.incr_outstanding := frontend.io.incr_outstanding
  frontend.io.host_id := io.host_id
  frontend.io.cpu.req.valid := cmd.valid && is_transfer
  frontend.io.cpu.req.bits := ClientDmaRequest(
    cmd = cmd.bits.inst.funct,
    src_start = cmd.bits.rs2,
    dst_start = cmd.bits.rs1,
    src_stride = crfile.io.src_stride,
    dst_stride = crfile.io.dst_stride,
    segment_size = crfile.io.segment_size,
    nsegments = crfile.io.nsegments,
    word_size = crfile.io.word_size)
  cmd.ready := is_transfer && frontend.io.cpu.req.ready

  when (frontend.io.cpu.resp.valid) {
    reg_status := frontend.io.cpu.resp.bits.status
  }

  io.busy := cmd.valid || frontend.io.busy || crfile.io.xact_outstanding
}
