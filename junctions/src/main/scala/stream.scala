package junctions

import Chisel._
import NastiConstants._
import cde.Parameters

class StreamChannel(w: Int) extends Bundle {
  val data = UInt(width = w)
  val last = Bool()

  override def cloneType = new StreamChannel(w).asInstanceOf[this.type]
}

class StreamIO(w: Int) extends Bundle {
  val out = Decoupled(new StreamChannel(w))
  val in = Decoupled(new StreamChannel(w)).flip

  override def cloneType = new StreamIO(w).asInstanceOf[this.type]
}

class NastiIOStreamIOConverter(w: Int)(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val nasti = (new NastiIO).flip
    val stream = new StreamIO(w)
  }

  val streamSize = UInt(log2Up(w / 8))
  assert(!io.nasti.ar.valid || io.nasti.ar.bits.size === streamSize,
         "read channel wrong size on stream")
  assert(!io.nasti.ar.valid || io.nasti.ar.bits.len === UInt(0) ||
         io.nasti.ar.bits.burst === BURST_FIXED,
         "read channel wrong burst type on stream")
  assert(!io.nasti.aw.valid || io.nasti.aw.bits.size === streamSize,
         "write channel wrong size on stream")
  assert(!io.nasti.aw.valid || io.nasti.aw.bits.len === UInt(0) ||
         io.nasti.aw.bits.burst === BURST_FIXED,
         "write channel wrong burst type on stream")
  assert(!io.nasti.w.valid || io.nasti.w.bits.strb.andR,
         "write channel cannot take partial writes")

  val read_id = Reg(io.nasti.ar.bits.id)
  val read_cnt = Reg(io.nasti.ar.bits.len)
  val reading = Reg(init = Bool(false))

  io.nasti.ar.ready := !reading
  io.nasti.r.valid := reading && io.stream.in.valid
  io.nasti.r.bits := io.stream.in.bits
  io.nasti.r.bits.resp := UInt(0)
  io.nasti.r.bits.id := read_id
  io.stream.in.ready := reading && io.nasti.r.ready

  when (io.nasti.ar.fire()) {
    read_id := io.nasti.ar.bits.id
    read_cnt := io.nasti.ar.bits.len
    reading := Bool(true)
  }

  when (io.nasti.r.fire()) {
    when (read_cnt === UInt(0)) {
      reading := Bool(false)
    } .otherwise {
      read_cnt := read_cnt - UInt(1)
    }
  }

  val write_id = Reg(io.nasti.aw.bits.id)
  val writing = Reg(init = Bool(false))
  val write_resp = Reg(init = Bool(false))

  io.nasti.aw.ready := !writing && !write_resp
  io.nasti.w.ready := writing && io.stream.out.ready
  io.stream.out.valid := writing && io.nasti.w.valid
  io.stream.out.bits := io.nasti.w.bits
  io.nasti.b.valid := write_resp
  io.nasti.b.bits.resp := UInt(0)
  io.nasti.b.bits.id := write_id

  when (io.nasti.aw.fire()) {
    write_id := io.nasti.aw.bits.id
    writing := Bool(true)
  }

  when (io.nasti.w.fire() && io.nasti.w.bits.last) {
    writing := Bool(false)
    write_resp := Bool(true)
  }

  when (io.nasti.b.fire()) { write_resp := Bool(false) }
}

class StreamNarrower(win: Int, wout: Int) extends Module {
  require(win > wout, "Stream narrower input width must be larger than input width")
  require(win % wout == 0, "Stream narrower input width must be multiple of output width")

  val io = new Bundle {
    val in = Decoupled(new StreamChannel(win)).flip
    val out = Decoupled(new StreamChannel(wout))
  }

  val n_pieces = win / wout
  val buffer = Reg(Bits(width = win))
  val (piece_idx, pkt_done) = Counter(io.out.fire(), n_pieces)
  val pieces = Vec.tabulate(n_pieces) { i => buffer(wout * (i + 1) - 1, wout * i) }
  val last_piece = (piece_idx === UInt(n_pieces - 1))
  val sending = Reg(init = Bool(false))
  val in_last = Reg(Bool())

  when (io.in.fire()) {
    buffer := io.in.bits.data
    in_last := io.in.bits.last
    sending := Bool(true)
  }
  when (pkt_done) { sending := Bool(false) }

  io.out.valid := sending
  io.out.bits.data := pieces(piece_idx)
  io.out.bits.last := in_last && last_piece
  io.in.ready := !sending
}

class StreamExpander(win: Int, wout: Int) extends Module {
  require(win < wout, "Stream expander input width must be smaller than input width")
  require(wout % win == 0, "Stream narrower output width must be multiple of input width")

  val io = new Bundle {
    val in = Decoupled(new StreamChannel(win)).flip
    val out = Decoupled(new StreamChannel(wout))
  }

  val n_pieces = wout / win
  val buffer = Reg(Vec(n_pieces, UInt(width = win)))
  val last = Reg(Bool())
  val collecting = Reg(init = Bool(true))
  val (piece_idx, pkt_done) = Counter(io.in.fire(), n_pieces)

  when (io.in.fire()) { buffer(piece_idx) := io.in.bits.data }
  when (pkt_done) { last := io.in.bits.last; collecting := Bool(false) }
  when (io.out.fire()) { collecting := Bool(true) }

  io.in.ready := collecting
  io.out.valid := !collecting
  io.out.bits.data := buffer.toBits
  io.out.bits.last := last
}

object StreamUtils {
  def connectStreams(a: StreamIO, b: StreamIO) {
    a.in <> b.out
    b.in <> a.out
  }
}

trait Serializable {
  def nbits: Int
}

class Serializer[T <: Data with Serializable](w: Int, typ: T) extends Module {
  val io = new Bundle {
    val in = Decoupled(typ).flip
    val out = Decoupled(Bits(width = w))
  }

  val narrower = Module(new StreamNarrower(typ.nbits, w))
  narrower.io.in.bits.data := io.in.bits.toBits
  narrower.io.in.bits.last := Bool(true)
  narrower.io.in.valid := io.in.valid
  io.in.ready := narrower.io.in.ready
  io.out.valid := narrower.io.out.valid
  io.out.bits := narrower.io.out.bits.data
  narrower.io.out.ready := io.out.ready
}

class Deserializer[T <: Data with Serializable](w: Int, typ: T) extends Module {
  val io = new Bundle {
    val in = Decoupled(Bits(width = w)).flip
    val out = Decoupled(typ)
  }

  val expander = Module(new StreamExpander(w, typ.nbits))
  expander.io.in.valid := io.in.valid
  expander.io.in.bits.data := io.in.bits
  expander.io.in.bits.last := Bool(true)
  io.in.ready := expander.io.in.ready
  io.out.valid := expander.io.out.valid
  io.out.bits := typ.cloneType.fromBits(expander.io.out.bits.data)
  expander.io.out.ready := io.out.ready
}
