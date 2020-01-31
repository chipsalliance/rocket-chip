// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO}

// A Repeater passes its input to its output, unless repeat is asserted.
// When repeat is asserted, the Repeater copies the input and repeats it next cycle.
class Repeater[T <: Data](gen: T) extends Module
{
  val typ = chiselTypeOf(gen)

  val io = IO( new Bundle {
    val repeat = Input(Bool())
    val full = Output(Bool())
    val enq = Flipped(Decoupled(typ))
    val deq = Decoupled(typ)
  } )

  val full = RegInit(false.B)
  val saved = Reg(typ)

  // When !full, a repeater is pass-through
  io.deq.valid := io.enq.valid || full
  io.enq.ready := io.deq.ready && !full
  io.deq.bits := Mux(full, saved, io.enq.bits)
  io.full := full

  when (io.enq.fire() &&  io.repeat) { full := true.B; saved := io.enq.bits }
  when (io.deq.fire() && !io.repeat) { full := false.B }
}

object Repeater
{
  def apply[T <: Data](enq: DecoupledIO[T], repeat: Bool): DecoupledIO[T] = {
    val repeater = Module(new Repeater(chiselTypeOf(enq.bits)))
    repeater.io.repeat := repeat
    repeater.io.enq <> enq
    repeater.io.deq
  }
}
