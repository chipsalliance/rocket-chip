package uncore

import Chisel._
import cde.{Parameters, Field}
import junctions._
import junctions.NastiConstants._

case object NDmaTransactors extends Field[Int]
case object NDmaXacts extends Field[Int]
case object NDmaClients extends Field[Int]

trait HasDmaParameters {
  implicit val p: Parameters
  val nDmaTransactors = p(NDmaTransactors)
  val nDmaXacts = p(NDmaXacts)
  val nDmaClients = p(NDmaClients)
  val dmaXactIdBits = log2Up(nDmaXacts)
  val dmaClientIdBits = log2Up(nDmaClients)
  val addrBits = p(PAddrBits)
  val dmaStatusBits = 2
  val dmaWordSizeBits = 2
}

abstract class DmaModule(implicit val p: Parameters) extends Module with HasDmaParameters
abstract class DmaBundle(implicit val p: Parameters) extends ParameterizedBundle()(p) with HasDmaParameters

class DmaRequest(implicit p: Parameters) extends DmaBundle()(p) {
  val xact_id = UInt(width = dmaXactIdBits)
  val client_id = UInt(width = dmaClientIdBits)
  val cmd = UInt(width = DmaRequest.DMA_CMD_SZ)
  val source = UInt(width = addrBits)
  val dest = UInt(width = addrBits)
  val length = UInt(width = addrBits)
  val size = UInt(width = dmaWordSizeBits)
}

class DmaResponse(implicit p: Parameters) extends DmaBundle()(p) {
  val xact_id = UInt(width = dmaXactIdBits)
  val client_id = UInt(width = dmaClientIdBits)
  val status = UInt(width = dmaStatusBits)
}

object DmaRequest {
  val DMA_CMD_SZ = 3

  val DMA_CMD_COPY = UInt("b000")
  val DMA_CMD_PFR  = UInt("b010")
  val DMA_CMD_PFW  = UInt("b011")
  val DMA_CMD_SIN  = UInt("b100")
  val DMA_CMD_SOUT = UInt("b101")

  def apply(xact_id: UInt = UInt(0),
            client_id: UInt,
            cmd: UInt,
            source: UInt,
            dest: UInt,
            length: UInt,
            size: UInt = UInt(0))(implicit p: Parameters): DmaRequest = {
    val req = Wire(new DmaRequest)
    req.xact_id := xact_id
    req.client_id := client_id
    req.cmd := cmd
    req.source := source
    req.dest := dest
    req.length := length
    req.size := size
    req
  }
}
import DmaRequest._

class DmaIO(implicit p: Parameters) extends DmaBundle()(p) {
  val req = Decoupled(new DmaRequest)
  val resp = Decoupled(new DmaResponse).flip
}

class DmaTrackerIO(implicit p: Parameters) extends DmaBundle()(p) {
  val dma = (new DmaIO).flip
  val mem = new ClientUncachedTileLinkIO
  val mmio = new NastiIO
}

class DmaManager(outstandingCSR: Int)(implicit p: Parameters)
    extends DmaModule()(p)
    with HasNastiParameters
    with HasAddrMapParameters
    with HasHtifParameters {

  val io = new Bundle {
    val ctrl = (new NastiIO).flip
    val mmio = new NastiIO
    val dma = new DmaIO
  }

  private val wordBits = 1 << log2Up(addrBits)
  private val wordBytes = wordBits / 8
  private val wordOff = log2Up(wordBytes)
  private val wordMSB = wordOff + 2

  val s_idle :: s_wdata :: s_dma_req :: s_wresp :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  val nCtrlWords = (addrBits * 4) / nastiXDataBits
  val ctrl_regs = Reg(Vec(nCtrlWords, UInt(width = nastiXDataBits)))
  val ctrl_idx = Reg(UInt(width = log2Up(nCtrlWords)))
  val ctrl_done = Reg(Bool())
  val ctrl_blob = ctrl_regs.toBits
  val ctrl_id = Reg(UInt(width = nastiXIdBits))

  val sizeOffset = 3 * addrBits
  val cmdOffset = sizeOffset + dmaWordSizeBits

  val dma_req = new DmaRequest().fromBits(ctrl_blob)
  val dma_busy = Reg(init = UInt(0, nDmaXacts))
  val dma_xact_id = PriorityEncoder(~dma_busy)

  when (io.ctrl.aw.fire()) {
    ctrl_id := io.ctrl.aw.bits.id
    ctrl_idx := UInt(0)
    ctrl_done := Bool(false)
    state := s_wdata
  }

  when (io.ctrl.w.fire()) {
    when (!ctrl_done) {
      ctrl_regs(ctrl_idx) := io.ctrl.w.bits.data
      ctrl_idx := ctrl_idx + UInt(1)
    }
    when (ctrl_idx === UInt(nCtrlWords - 1)) { ctrl_done := Bool(true) }
    when (io.ctrl.w.bits.last) { state := s_dma_req }
  }

  dma_busy := (dma_busy |
    Mux(io.dma.req.fire(), UIntToOH(dma_xact_id), UInt(0))) &
    ~Mux(io.dma.resp.fire(), UIntToOH(io.dma.resp.bits.xact_id), UInt(0))

  when (io.dma.req.fire()) { state := s_wresp }
  when (io.ctrl.b.fire()) { state := s_idle }

  io.ctrl.ar.ready := Bool(false)
  io.ctrl.aw.ready := (state === s_idle)
  io.ctrl.w.ready := (state === s_wdata)

  io.ctrl.r.valid := Bool(false)
  io.ctrl.b.valid := (state === s_wresp)
  io.ctrl.b.bits := NastiWriteResponseChannel(id = ctrl_id)

  io.dma.req.valid := (state === s_dma_req) && !dma_busy.andR
  io.dma.req.bits := dma_req
  io.dma.req.bits.xact_id := dma_xact_id

  val resp_waddr_pending = Reg(init = Bool(false))
  val resp_wdata_pending = Reg(init = Bool(false))
  val resp_wresp_pending = Reg(init = Bool(false))
  val resp_pending = resp_waddr_pending || resp_wdata_pending || resp_wresp_pending

  val resp_client_id = Reg(UInt(width = dmaClientIdBits))
  val resp_status = Reg(UInt(width = dmaStatusBits))

  io.dma.resp.ready := !resp_pending

  when (io.dma.resp.fire()) {
    resp_client_id := io.dma.resp.bits.client_id
    resp_status := io.dma.resp.bits.status
    resp_waddr_pending := Bool(true)
    resp_wdata_pending := Bool(true)
    resp_wresp_pending := Bool(true)
  }

  val addrTable = Vec.tabulate(nDmaClients) { i =>
    UInt(addrMap(s"conf:csr$i").start + outstandingCSR * csrDataBytes)
  }

  io.mmio.ar.valid := Bool(false)
  io.mmio.aw.valid := resp_waddr_pending
  io.mmio.aw.bits := NastiWriteAddressChannel(
    id = UInt(0),
    addr = addrTable(resp_client_id),
    size = UInt(log2Up(csrDataBytes)))
  io.mmio.w.valid := resp_wdata_pending
  io.mmio.w.bits := NastiWriteDataChannel(data = resp_status)
  io.mmio.b.ready := resp_wresp_pending
  io.mmio.r.ready := Bool(false)

  when (io.mmio.aw.fire()) { resp_waddr_pending := Bool(false) }
  when (io.mmio.w.fire()) { resp_wdata_pending := Bool(false) }
  when (io.mmio.b.fire()) { resp_wresp_pending := Bool(false) }
}

class DmaEngine(outstandingCSR: Int)(implicit p: Parameters) extends DmaModule()(p) {
  val io = new Bundle {
    val ctrl = (new NastiIO).flip
    val mem = new ClientUncachedTileLinkIO
    val mmio = new NastiIO
  }

  val manager = Module(new DmaManager(outstandingCSR))
  val trackers = Module(new DmaTrackerFile)

  manager.io.ctrl <> io.ctrl
  trackers.io.dma <> manager.io.dma

  val innerIOs = trackers.io.mem
  val outerIOs = trackers.io.mmio :+ manager.io.mmio

  val innerArb = Module(new ClientUncachedTileLinkIOArbiter(innerIOs.size))
  innerArb.io.in <> innerIOs
  io.mem <> innerArb.io.out

  val outerArb = Module(new NastiArbiter(outerIOs.size))
  outerArb.io.master <> outerIOs
  io.mmio <> outerArb.io.slave

  assert(!io.mmio.b.valid || io.mmio.b.bits.resp === UInt(0),
    "DmaEngine: NASTI write response error")

  assert(!io.mmio.r.valid || io.mmio.r.bits.resp === UInt(0),
    "DmaEngine: NASTI read response error")
}

class DmaTrackerFile(implicit p: Parameters) extends DmaModule()(p) {
  val io = new Bundle {
    val dma = (new DmaIO).flip
    val mem = Vec(nDmaTransactors, new ClientUncachedTileLinkIO)
    val mmio = Vec(nDmaTransactors, new NastiIO)
  }

  val trackers = List.fill(nDmaTransactors) { Module(new DmaTracker) }
  val reqReadys = Vec(trackers.map(_.io.dma.req.ready)).toBits

  io.mem <> trackers.map(_.io.mem)
  io.mmio <> trackers.map(_.io.mmio)

  if (nDmaTransactors > 1) {
    val resp_arb = Module(new RRArbiter(new DmaResponse, nDmaTransactors))
    resp_arb.io.in <> trackers.map(_.io.dma.resp)
    io.dma.resp <> resp_arb.io.out

    val selection = PriorityEncoder(reqReadys)
    trackers.zipWithIndex.foreach { case (tracker, i) =>
      tracker.io.dma.req.valid := io.dma.req.valid && selection === UInt(i)
      tracker.io.dma.req.bits := io.dma.req.bits
    }
    io.dma.req.ready := reqReadys.orR
  } else {
    io.dma <> trackers.head.io.dma
  }
}

class DmaTracker(implicit p: Parameters) extends DmaModule()(p)
    with HasTileLinkParameters with HasNastiParameters {
  val io = new DmaTrackerIO

  private val blockOffset = tlBeatAddrBits + tlByteAddrBits
  private val blockBytes = tlDataBeats * tlDataBytes

  val data_buffer = Reg(Vec(2 * tlDataBeats, Bits(width = tlDataBits)))
  val get_inflight = Reg(UInt(2 * tlDataBeats))
  val put_inflight = Reg(Bool())
  val put_half = Reg(UInt(width = 1))
  val get_half = Reg(UInt(width = 1))
  val prefetch_put = Reg(Bool())
  val get_done = !get_inflight.orR

  val src_block = Reg(UInt(width = tlBlockAddrBits))
  val dst_block = Reg(UInt(width = tlBlockAddrBits))
  val offset    = Reg(UInt(width = blockOffset))
  val alignment = Reg(UInt(width = blockOffset))
  val shift_dir = Reg(Bool())

  val bytes_left = Reg(UInt(width = addrBits))
  val streaming = Reg(Bool())
  val stream_addr = Reg(UInt(width = nastiXAddrBits))
  val stream_len = Reg(UInt(width = nastiXLenBits))
  val stream_size = Reg(UInt(width = nastiXSizeBits))
  val stream_idx = Reg(UInt(width = blockOffset))
  val stream_bytesel = MuxLookup(stream_size, UInt("b11111111"), Seq(
    UInt("b00") -> UInt("b00000001"),
    UInt("b01") -> UInt("b00000011"),
    UInt("b10") -> UInt("b00001111")))
  val stream_mask = FillInterleaved(8, stream_bytesel)
  val stream_last = Reg(Bool())

  val stream_word_bytes = UInt(1) << stream_size
  val stream_beat_idx = stream_idx(blockOffset - 1, tlByteAddrBits)
  val stream_byte_idx = stream_idx(tlByteAddrBits - 1, 0)
  val stream_bitshift = Cat(stream_byte_idx, UInt(0, 3))
  val stream_in_beat =
    (((io.mmio.r.bits.data & stream_mask) << stream_bitshift)) |
    (data_buffer(stream_beat_idx) & ~(stream_mask << stream_bitshift))
  val stream_out_word = data_buffer(stream_beat_idx) >> stream_bitshift
  val stream_out_last = bytes_left === stream_word_bytes

  val acq = io.mem.acquire.bits
  val gnt = io.mem.grant.bits

  val (s_idle :: s_get :: s_put :: s_prefetch ::
       s_stream_read_req :: s_stream_read_resp ::
       s_stream_write_req :: s_stream_write_data :: s_stream_write_resp ::
       s_wait :: s_resp :: Nil) = Enum(Bits(), 11)
  val state = Reg(init = s_idle)

  val (put_beat, put_done) = Counter(
    io.mem.acquire.fire() && acq.hasData(), tlDataBeats)

  val put_mask = Vec.tabulate(tlDataBytes) { i =>
    val byte_index = Cat(put_beat, UInt(i, tlByteAddrBits))
    byte_index >= offset && byte_index < bytes_left
  }.toBits

  val prefetch_sent = io.mem.acquire.fire() && io.mem.acquire.bits.isPrefetch()
  val prefetch_busy = Reg(init = UInt(0, tlMaxClientXacts))
  val (prefetch_id, _) = Counter(prefetch_sent, tlMaxClientXacts)

  val base_index = Cat(put_half, put_beat)
  val put_data = Wire(init = Bits(0, tlDataBits))
  val beat_align = alignment(blockOffset - 1, tlByteAddrBits)
  val bit_align = Cat(alignment(tlByteAddrBits - 1, 0), UInt(0, 3))
  val rev_align = UInt(tlDataBits) - bit_align

  def getBit(value: UInt, sel: UInt): Bool =
    (value >> sel)(0)

  when (alignment === UInt(0)) {
    put_data := data_buffer.read(base_index)
  } .elsewhen (shift_dir) {
    val shift_index = base_index - beat_align
    when (bit_align === UInt(0)) {
      put_data := data_buffer.read(shift_index)
    } .otherwise {
      val upper_bits = data_buffer.read(shift_index)
      val lower_bits = data_buffer.read(shift_index - UInt(1))
      val upper_shifted = upper_bits << bit_align
      val lower_shifted = lower_bits >> rev_align
      put_data := upper_shifted | lower_shifted
    }
  } .otherwise {
    val shift_index = base_index + beat_align
    when (bit_align === UInt(0)) {
      put_data := data_buffer.read(shift_index)
    } .otherwise {
      val upper_bits = data_buffer.read(shift_index + UInt(1))
      val lower_bits = data_buffer.read(shift_index)
      val upper_shifted = upper_bits << rev_align
      val lower_shifted = lower_bits >> bit_align
      put_data := upper_shifted | lower_shifted
    }
  }

  val put_acquire = PutBlock(
    client_xact_id = UInt(2),
    addr_block = dst_block,
    addr_beat = put_beat,
    data = put_data,
    wmask = put_mask)

  val get_acquire = GetBlock(
    client_xact_id = get_half,
    addr_block = src_block,
    alloc = Bool(false))

  val prefetch_acquire = Mux(prefetch_put,
    PutPrefetch(client_xact_id = prefetch_id, addr_block = dst_block),
    GetPrefetch(client_xact_id = prefetch_id, addr_block = dst_block))

  val resp_xact_id = Reg(UInt(width = dmaXactIdBits))
  val resp_client_id = Reg(UInt(width = dmaClientIdBits))

  io.mem.acquire.valid := (state === s_get) ||
                          (state === s_put && get_done) ||
                          (state === s_prefetch && !prefetch_busy(prefetch_id))
  io.mem.acquire.bits := MuxBundle(
    state, prefetch_acquire, Seq(
      s_get -> get_acquire,
      s_put -> put_acquire))
  io.mem.grant.ready := Bool(true)
  io.dma.req.ready := state === s_idle
  io.dma.resp.valid := state === s_resp
  io.dma.resp.bits.xact_id := resp_xact_id
  io.dma.resp.bits.client_id := resp_client_id
  io.dma.resp.bits.status := UInt(0)
  io.mmio.ar.valid := (state === s_stream_read_req)
  io.mmio.ar.bits := NastiReadAddressChannel(
    id = UInt(0),
    addr = stream_addr,
    size = stream_size,
    len  = stream_len,
    burst = BURST_FIXED)
  io.mmio.r.ready := (state === s_stream_read_resp)

  io.mmio.aw.valid := (state === s_stream_write_req)
  io.mmio.aw.bits := NastiWriteAddressChannel(
    id = UInt(0),
    addr = stream_addr,
    size = stream_size,
    len  = stream_len,
    burst = BURST_FIXED)
  io.mmio.w.valid := (state === s_stream_write_data) && get_done
  io.mmio.w.bits := NastiWriteDataChannel(
    data = stream_out_word,
    last = stream_out_last)
  io.mmio.b.ready := (state === s_stream_write_resp)

  when (io.dma.req.fire()) {
    val src_off = io.dma.req.bits.source(blockOffset - 1, 0)
    val dst_off = io.dma.req.bits.dest(blockOffset - 1, 0)
    val direction = src_off < dst_off

    resp_xact_id := io.dma.req.bits.xact_id
    resp_client_id := io.dma.req.bits.client_id
    src_block := io.dma.req.bits.source(addrBits - 1, blockOffset)
    dst_block := io.dma.req.bits.dest(addrBits - 1, blockOffset)
    alignment := Mux(direction, dst_off - src_off, src_off - dst_off)
    shift_dir := direction
    offset := dst_off
    bytes_left := io.dma.req.bits.length + dst_off
    get_inflight := UInt(0)
    put_inflight := Bool(false)
    get_half := UInt(0)
    put_half := UInt(0)
    streaming := Bool(false)
    stream_len := (io.dma.req.bits.length >> io.dma.req.bits.size) - UInt(1)
    stream_size := io.dma.req.bits.size
    stream_last := Bool(false)

    when (io.dma.req.bits.cmd === DMA_CMD_COPY) {
      state := s_get
    } .elsewhen (io.dma.req.bits.cmd(2, 1) === UInt("b01")) {
      prefetch_put := io.dma.req.bits.cmd(0)
      state := s_prefetch
    } .elsewhen (io.dma.req.bits.cmd === DMA_CMD_SIN) {
      stream_addr := io.dma.req.bits.source
      stream_idx := dst_off
      streaming := Bool(true)
      alignment := UInt(0)
      state := s_stream_read_req
    } .elsewhen (io.dma.req.bits.cmd === DMA_CMD_SOUT) {
      stream_addr := io.dma.req.bits.dest
      stream_idx := src_off
      streaming := Bool(true)
      bytes_left := io.dma.req.bits.length
      state := s_stream_write_req
    }
  }

  when (io.mmio.ar.fire()) { state := s_stream_read_resp }

  when (io.mmio.r.fire()) {
    data_buffer(stream_beat_idx) := stream_in_beat
    stream_idx := stream_idx + stream_word_bytes
    val block_finished = stream_idx === UInt(blockBytes) - stream_word_bytes
    when (block_finished || io.mmio.r.bits.last) { state := s_put }
  }

  when (io.mmio.aw.fire()) { state := s_get }

  when (io.mmio.w.fire()) {
    stream_idx := stream_idx + stream_word_bytes
    bytes_left := bytes_left - stream_word_bytes
    val block_finished = stream_idx === UInt(blockBytes) - stream_word_bytes
    when (stream_out_last) {
      state := s_stream_write_resp
    } .elsewhen (block_finished) {
      state := s_get
    }
  }

  when (io.mmio.b.fire()) { state := s_resp }

  when (state === s_get && io.mem.acquire.ready) {
    get_inflight := get_inflight | FillInterleaved(tlDataBeats, UIntToOH(get_half))
    src_block := src_block + UInt(1)
    when (streaming) {
      state := s_stream_write_data
    } .otherwise {
      val bytes_in_buffer = UInt(blockBytes) - alignment
      val extra_read = alignment > UInt(0) && !shift_dir && // dst_off < src_off
                       get_half === UInt(0) && // this is the first block
                       bytes_in_buffer < bytes_left // there is still more data left to fetch
      get_half := get_half + UInt(1)
      when (!extra_read) { state := s_put }
    }
  }

  when (prefetch_sent) {
    prefetch_busy := prefetch_busy | UIntToOH(prefetch_id)
    when (bytes_left < UInt(blockBytes)) {
      bytes_left := UInt(0)
      state := s_resp
    } .otherwise {
      bytes_left := bytes_left - UInt(blockBytes)
      dst_block := dst_block + UInt(1)
    }
  }

  when (io.mem.grant.fire()) {
    when (gnt.g_type === Grant.prefetchAckType) {
      prefetch_busy := prefetch_busy & ~UIntToOH(gnt.client_xact_id)
    } .elsewhen (gnt.hasData()) {
      val write_half = gnt.client_xact_id(0)
      val write_idx = Cat(write_half, gnt.addr_beat)
      get_inflight := get_inflight & ~UIntToOH(write_idx)
      data_buffer.write(write_idx, gnt.data)
    } .otherwise {
      put_inflight := Bool(false)
    }
  }

  when (put_done) { // state === s_put
    when (!streaming) {
      put_half := put_half + UInt(1)
    }
    offset := UInt(0)
    stream_idx := UInt(0)
    when (bytes_left < UInt(blockBytes)) {
      bytes_left := UInt(0)
    } .otherwise {
      bytes_left := bytes_left - UInt(blockBytes)
    }
    put_inflight := Bool(true)
    dst_block := dst_block + UInt(1)
    state := s_wait
  }

  when (state === s_wait && get_done && !put_inflight) {
    state := MuxCase(s_get, Seq(
      (bytes_left === UInt(0)) -> s_resp,
      streaming -> s_stream_read_resp))
  }

  when (io.dma.resp.fire()) { state := s_idle }
}
