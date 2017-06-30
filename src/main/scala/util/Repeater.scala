// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._

// A Repeater passes it's input to it's output, unless repeat is asserted.
// When repeat is asserted, the Repeater copies the input and repeats it next cycle.
class Repeater[T <: Data](gen: T) extends Module
{
  val io = new Bundle {
    val repeat = Bool(INPUT)
    val full = Bool(OUTPUT)
    val enq = Decoupled(gen).flip
    val deq = Decoupled(gen)
  }

  val full = RegInit(Bool(false))
  val saved = Reg(gen)

  // When !full, a repeater is pass-through
  io.deq.valid := io.enq.valid || full
  io.enq.ready := io.deq.ready && !full
  io.deq.bits := Mux(full, saved, io.enq.bits)
  io.full := full

  when (io.enq.fire() &&  io.repeat) { full := Bool(true); saved := io.enq.bits }
  when (io.deq.fire() && !io.repeat) { full := Bool(false) }
}

object Repeater
{
  def apply[T <: Data](enq: DecoupledIO[T], repeat: Bool): DecoupledIO[T] = {
    val repeater = Module(new Repeater(enq.bits))
    repeater.io.repeat := repeat
    repeater.io.enq := enq
    repeater.io.deq
  }
}
