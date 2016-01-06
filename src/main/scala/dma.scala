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
  val outer_err = UInt("b10")

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

class DmaFrontend(implicit val p: Parameters)
    extends Module with HasClientDmaParameters {
  val io = new Bundle {
    val cpu = (new ClientDmaIO).flip
    val dma = new DmaIO
    val ptw = new TLBPTWIO
    val busy = Bool(OUTPUT)
  }

  private val pgSize = 1 << pgIdxBits

  val priv = Mux(io.ptw.status.mprv, io.ptw.status.prv1, io.ptw.status.prv)
  val vm_enabled = io.ptw.status.vm(3) && priv <= UInt(PRV_S)

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

  val src_paddr = Mux(vm_enabled, Cat(src_ppn, src_idx), src_vaddr)
  val dst_paddr = Mux(vm_enabled, Cat(dst_ppn, dst_idx), dst_vaddr)

  val last_src_vpn = Reg(UInt(width = vpnBits))
  val last_dst_vpn = Reg(UInt(width = vpnBits))

  val tx_len = Mux(!vm_enabled, bytes_left,
    Util.minUInt(src_pglen, dst_pglen, bytes_left))

  val (dma_xact_id, _) = Counter(io.dma.req.fire(), nDmaXactsPerClient)
  val dma_busy = Reg(init = UInt(0, nDmaXactsPerClient))

  val (s_idle :: s_translate :: s_dma_req :: s_dma_update ::
       s_prepare :: s_finish :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_idle)

  // lower bit is for src, higher bit is for dst
  val to_translate = Reg(init = UInt(0, 2))
  val ptw_sent = Reg(init = UInt(0, 2))
  val ptw_to_send = to_translate & ~ptw_sent
  val ptw_resp_id = Reg(init = UInt(0, 1))
  val resp_status = Reg(UInt(width = dmaStatusBits))

  io.ptw.req.valid := ptw_to_send.orR && vm_enabled
  io.ptw.req.bits.addr := Mux(ptw_to_send(0), src_vpn, dst_vpn)
  io.ptw.req.bits.prv := io.ptw.status.prv
  io.ptw.req.bits.store := !ptw_to_send(0) // storing to destination
  io.ptw.req.bits.fetch := Bool(true)

  when (io.ptw.req.fire()) {
    ptw_sent := ptw_sent | PriorityEncoderOH(ptw_to_send)
  }

  when (io.ptw.resp.valid) {
    when (io.ptw.resp.bits.error) {
      resp_status := ClientDmaResponse.pagefault
      state := s_finish
    }
    val recv_choice = PriorityEncoderOH(to_translate)
    to_translate := to_translate & ~recv_choice

    // getting the src translation
    when (recv_choice(0)) {
      src_ppn := io.ptw.resp.bits.pte.ppn
    } .otherwise {
      dst_ppn := io.ptw.resp.bits.pte.ppn
    }
  }

  io.cpu.req.ready := state === s_idle
  io.cpu.resp.valid := state === s_finish
  io.cpu.resp.bits := ClientDmaResponse(resp_status)
  io.dma.req.valid := state === s_dma_req && !dma_busy(dma_xact_id)
  io.dma.req.bits := DmaRequest(
    client_xact_id = dma_xact_id,
    cmd = cmd,
    source = src_paddr,
    dest = dst_paddr,
    length = tx_len,
    size = word_size)
  io.dma.resp.ready := Bool(true)

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
    ptw_sent := UInt(0)
    state := Mux(vm_enabled, s_translate, s_dma_req)
  }

  when (state === s_translate && !to_translate.orR) {
    state := s_dma_req
  }

  def setBusyOnSend(req: DecoupledIO[DmaRequest]): UInt =
    Mux(req.fire(), UIntToOH(req.bits.client_xact_id), UInt(0))

  def clearBusyOnRecv(resp: DecoupledIO[DmaResponse]): UInt =
    ~Mux(resp.fire(), UIntToOH(resp.bits.client_xact_id), UInt(0))

  dma_busy := (dma_busy | setBusyOnSend(io.dma.req)) &
                          clearBusyOnRecv(io.dma.resp)

  when (io.dma.req.fire()) {
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
        state := Mux(vm_enabled, s_prepare, s_dma_req)
      }
    } .otherwise {
      to_translate := adv_ptr & Cat(dst_idx === UInt(0), src_idx === UInt(0))
      ptw_sent := UInt(0)
      state := s_translate
    }
  }

  when (state === s_prepare) {
    to_translate := adv_ptr & Cat(
      dst_vpn =/= last_dst_vpn,
      src_vpn =/= last_src_vpn)
    ptw_sent := UInt(0)
    state := s_translate
  }

  when (state === s_finish) { state := s_idle }

  io.busy := (state =/= s_idle) || dma_busy.orR
}

object DmaCtrlRegNumbers {
  val SRC_STRIDE = 0
  val DST_STRIDE = 1
  val SEGMENT_SIZE = 2
  val NSEGMENTS = 3
  val WORD_SIZE = 4
  val RESP_STATUS = 5
}
import DmaCtrlRegNumbers._

class DmaCtrlRegFile(implicit p: Parameters) extends ClientDmaModule()(p) {
  private val nWriteRegs = 5
  private val nReadRegs = 1
  private val nRegs = nWriteRegs + nReadRegs

  val io = new Bundle {
    val wen = Bool(INPUT)
    val addr = UInt(INPUT, log2Up(nRegs))
    val wdata = UInt(INPUT, dmaSegmentSizeBits)
    val rdata = UInt(OUTPUT, dmaSegmentSizeBits)

    val src_stride = UInt(OUTPUT, dmaSegmentSizeBits)
    val dst_stride = UInt(OUTPUT, dmaSegmentSizeBits)
    val segment_size = UInt(OUTPUT, dmaSegmentSizeBits)
    val nsegments  = UInt(OUTPUT, dmaSegmentBits)
    val word_size = UInt(OUTPUT, dmaWordSizeBits)

    val status = UInt(INPUT, dmaStatusBits)
  }

  val regs = Reg(Vec(nWriteRegs, UInt(width = dmaSegmentSizeBits)))

  io.src_stride := regs(SRC_STRIDE)
  io.dst_stride := regs(DST_STRIDE)
  io.segment_size := regs(SEGMENT_SIZE)
  io.nsegments := regs(NSEGMENTS)
  io.word_size := regs(WORD_SIZE)

  when (io.wen && io.addr < UInt(nWriteRegs)) {
    regs.write(io.addr, io.wdata)
  }

  io.rdata := MuxLookup(io.addr, regs(io.addr), Seq(
    UInt(RESP_STATUS) -> io.status))
}

class DmaController(implicit p: Parameters) extends RoCC()(p)
    with HasClientDmaParameters {
  io.mem.req.valid := Bool(false)
  io.autl.acquire.valid := Bool(false)
  io.autl.grant.ready := Bool(false)
  io.iptw.req.valid := Bool(false)
  io.pptw.req.valid := Bool(false)

  val cmd = Queue(io.cmd)
  val inst = cmd.bits.inst
  val is_transfer = inst.funct < UInt(8)
  val is_cr_write = inst.funct === UInt(8)
  val is_cr_read  = inst.funct === UInt(9)
  val is_cr_access = is_cr_write || is_cr_read

  val resp_rd = Reg(io.resp.bits.rd)
  val resp_data = Reg(io.resp.bits.data)

  val s_idle :: s_resp :: Nil = Enum(Bits(), 2)
  val state = Reg(init = s_idle)

  val reg_status = Reg(UInt(width = dmaStatusBits))
  val crfile = Module(new DmaCtrlRegFile)
  crfile.io.addr := cmd.bits.rs1
  crfile.io.wdata := cmd.bits.rs2
  crfile.io.wen := cmd.fire() && is_cr_write

  val frontend = Module(new DmaFrontend)
  io.dma <> frontend.io.dma
  io.dptw <> frontend.io.ptw
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

  cmd.ready := state === s_idle && (!is_transfer || frontend.io.cpu.req.ready)
  io.resp.valid := state === s_resp
  io.resp.bits.rd := resp_rd
  io.resp.bits.data := resp_data

  when (cmd.fire()) {
    when (is_cr_read) {
      resp_rd := inst.rd
      resp_data := crfile.io.rdata
      state := s_resp
    }
  }

  when (io.resp.fire()) { state := s_idle }

  when (frontend.io.cpu.resp.valid) {
    reg_status := frontend.io.cpu.resp.bits.status
  }

  io.busy := (state =/= s_idle) || cmd.valid || frontend.io.busy
  io.interrupt := Bool(false)
}
