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
  assert(!io.nasti.ar.valid || io.nasti.ar.bits.burst === BURST_FIXED,
         "read channel wrong burst type on stream")
  assert(!io.nasti.aw.valid || io.nasti.aw.bits.size === streamSize,
         "write channel wrong size on stream")
  assert(!io.nasti.aw.valid || io.nasti.aw.bits.burst === BURST_FIXED,
         "write channel wrong burst type on stream")

  val read_id = Reg(io.nasti.ar.bits.id)
  val reading = Reg(init = Bool(false))

  io.nasti.ar.ready := !reading
  io.nasti.r.valid := reading && io.stream.in.valid
  io.nasti.r.bits := io.stream.in.bits
  io.nasti.r.bits.resp := UInt(0)
  io.nasti.r.bits.id := read_id
  io.stream.in.ready := reading && io.nasti.r.ready

  when (io.nasti.ar.fire()) {
    read_id := io.nasti.ar.bits.id
    reading := Bool(true)
  }

  when (io.nasti.r.fire() && io.nasti.r.bits.last) {
    reading := Bool(false)
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
