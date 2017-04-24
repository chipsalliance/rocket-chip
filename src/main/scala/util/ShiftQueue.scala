// See LICENSE.SiFive for license details.

package util

import Chisel._

/** Implements the same interface as chisel3.util.Queue, but uses a shift
  * register internally.  It is less energy efficient whenever the queue
  * has more than one entry populated, but is faster on the dequeue side.
  * It is efficient for usually-empty flow-through queues. */
class ShiftQueue[T <: Data](gen: T,
                            val entries: Int,
                            pipe: Boolean = false,
                            flow: Boolean = false)
    extends Module {
  val io = IO(new QueueIO(gen, entries) {
    val mask = UInt(OUTPUT, entries)
  })

  private val ram = Mem(entries, gen)
  private val valid = RegInit(UInt(0, entries))
  private val elts = Reg(Vec(entries, gen))

  private val do_enq = Wire(init=io.enq.fire())
  private val do_deq = Wire(init=io.deq.fire())

  when (do_deq) {
    when (!do_enq) { valid := (valid >> 1) }
    for (i <- 1 until entries)
      when (valid(i)) { elts(i-1) := elts(i) }
  }
  when (do_enq && do_deq) {
    for (i <- 0 until entries)
      when (valid(i) && (if (i == entries-1) true.B else !valid(i+1))) { elts(i) := io.enq.bits }
  }
  when (do_enq && !do_deq) {
    valid := (valid << 1) | UInt(1)
    for (i <- 0 until entries)
      when (!valid(i) && (if (i == 0) true.B else valid(i-1))) { elts(i) := io.enq.bits }
  }

  io.enq.ready := !valid(entries-1)
  io.deq.valid := valid(0)
  io.deq.bits := elts.head

  if (flow) {
    when (io.enq.valid) { io.deq.valid := true.B }
    when (!valid(0)) {
      io.deq.bits := io.enq.bits
      do_deq := false.B
      when (io.deq.ready) { do_enq := false.B }
    }
  }

  if (pipe) {
    when (io.deq.ready) { io.enq.ready := true.B }
  }

  io.count := PopCount(valid)
  io.mask := valid
}
