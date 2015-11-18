package uncore

import Chisel._
import cde.{Parameters, Field}
import junctions._

case object NDmaTransactors extends Field[Int]
case object NDmaClients extends Field[Int]
case object NDmaXactsPerClient extends Field[Int]

trait HasDmaParameters {
  implicit val p: Parameters
  val nDmaTransactors = p(NDmaTransactors)
  val nDmaClients = p(NDmaClients)
  val nDmaXactsPerClient = p(NDmaXactsPerClient)
  val dmaClientXactIdBits = log2Up(nDmaClients * nDmaXactsPerClient)
  val addrBits = p(PAddrBits)
  val dmaStatusBits = 2
}

abstract class DmaModule(implicit val p: Parameters) extends Module with HasDmaParameters
abstract class DmaBundle(implicit val p: Parameters) extends ParameterizedBundle()(p) with HasDmaParameters

class DmaRequest(implicit p: Parameters) extends DmaBundle()(p) {
  val client_xact_id = UInt(width = dmaClientXactIdBits)
  val cmd = UInt(width = DmaRequest.DMA_CMD_SZ)
  val source = UInt(width = addrBits)
  val dest = UInt(width = addrBits)
  val length = UInt(width = addrBits)
}

class DmaResponse(implicit p: Parameters) extends DmaBundle()(p) {
  val client_xact_id = UInt(width = dmaClientXactIdBits)
  val status = UInt(width = dmaStatusBits)
}

object DmaRequest {
  val DMA_CMD_SZ = 2

  val DMA_CMD_COPY = UInt(0, DMA_CMD_SZ)
  val DMA_CMD_PFR = UInt(2, DMA_CMD_SZ)
  val DMA_CMD_PFW = UInt(3, DMA_CMD_SZ)

  def apply(client_xact_id: UInt = UInt(0),
            cmd: UInt,
            source: UInt,
            dest: UInt,
            length: UInt)(implicit p: Parameters): DmaRequest = {
    val req = Wire(new DmaRequest)
    req.client_xact_id := client_xact_id
    req.cmd := cmd
    req.source := source
    req.dest := dest
    req.length := length
    req
  }
}
import DmaRequest._

class DmaIO(implicit p: Parameters) extends DmaBundle()(p) {
  val req = Decoupled(new DmaRequest)
  val resp = Decoupled(new DmaResponse).flip
}

class DmaEngine(implicit p: Parameters) extends DmaModule()(p) {
  val io = new Bundle {
    val dma = (new DmaIO).flip
    val mem = new ClientUncachedTileLinkIO
  }

  val trackers = List.fill(nDmaTransactors) { Module(new DmaTracker) }
  val reqReadys = Vec(trackers.map(_.io.dma.req.ready)).toBits

  if (nDmaTransactors > 1) {
    val mem_arb = Module(new ClientUncachedTileLinkIOArbiter(nDmaTransactors))
    mem_arb.io.in <> trackers.map(_.io.mem)
    io.mem <> mem_arb.io.out

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
    io.mem <> trackers.head.io.mem
    io.dma <> trackers.head.io.dma
  }
}

class DmaTracker(implicit p: Parameters) extends DmaModule()(p)
    with HasTileLinkParameters {
  val io = new Bundle {
    val dma = (new DmaIO).flip
    val mem = new ClientUncachedTileLinkIO
  }

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

  val acq = io.mem.acquire.bits
  val gnt = io.mem.grant.bits

  val (s_idle :: s_get :: s_put :: s_prefetch ::
       s_wait :: s_resp :: Nil) = Enum(Bits(), 6)
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

  val resp_id = Reg(UInt(width = dmaClientXactIdBits))

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
  io.dma.resp.bits.client_xact_id := resp_id
  io.dma.resp.bits.status := UInt(0)

  when (io.dma.req.fire()) {
    val src_off = io.dma.req.bits.source(blockOffset - 1, 0)
    val dst_off = io.dma.req.bits.dest(blockOffset - 1, 0)
    val direction = src_off < dst_off

    resp_id := io.dma.req.bits.client_xact_id
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

    when (io.dma.req.bits.cmd === DMA_CMD_COPY) {
      state := s_get
    } .otherwise {
      prefetch_put := io.dma.req.bits.cmd(0)
      state := s_prefetch
    }
  }

  when (state === s_get && io.mem.acquire.ready) {
    val bytes_in_buffer = UInt(blockBytes) - alignment
    val extra_read = alignment > UInt(0) && !shift_dir && // dst_off < src_off
                     get_half === UInt(0) && // this is the first block
                     bytes_in_buffer < bytes_left // there is still more data left to fetch
    get_inflight := get_inflight | FillInterleaved(tlDataBeats, UIntToOH(get_half))
    get_half := get_half + UInt(1)
    src_block := src_block + UInt(1)
    when (!extra_read) {
      state := s_put
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
    put_half := put_half + UInt(1)
    offset := UInt(0)
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
    state := Mux(bytes_left === UInt(0), s_resp, s_get)
  }

  when (io.dma.resp.fire()) { state := s_idle }
}

class DmaArbiter(arbN: Int)(implicit p: Parameters) extends DmaModule()(p) {
  val io = new Bundle {
    val in = Vec(arbN, new DmaIO).flip
    val out = new DmaIO
  }

  if (arbN > 1) {
    val idBits = log2Up(arbN)
    val req_arb = Module(new RRArbiter(new DmaRequest, arbN))
    val out_resp_client_id = io.out.resp.bits.client_xact_id(idBits - 1, 0)

    for (i <- 0 until arbN) {
      req_arb.io.in(i) <> io.in(i).req
      req_arb.io.in(i).bits.client_xact_id := Cat(
        io.in(i).req.bits.client_xact_id,
        UInt(i, idBits))

      io.in(i).resp.valid := io.out.resp.valid && out_resp_client_id === UInt(i)
      io.in(i).resp.bits := io.out.resp.bits
    }

    val respReadys = Vec(io.in.map(_.resp.ready))

    io.out.req <> req_arb.io.out
    io.out.resp.ready := respReadys(out_resp_client_id)
  } else {
    io.out <> io.in.head
  }
}
