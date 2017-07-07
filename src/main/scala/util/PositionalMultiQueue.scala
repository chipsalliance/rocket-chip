// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._

case class PositionalMultiQueueParameters[T <: Data](gen: T, positions: Int, ways: Int)

class PositionalMultiQueueEntry[T <: Data](params: PositionalMultiQueueParameters[T])
  extends GenericParameterizedBundle(params)
{
  val data = params.gen.asOutput
  val pos  = UInt(width = log2Up(params.positions))
}

class PositionalMultiQueuePush[T <: Data](params: PositionalMultiQueueParameters[T])
  extends PositionalMultiQueueEntry(params)
{
  val way = UInt(width = log2Up(params.ways))
}

/* A PositionalMultiQueue is like a normal Queue, except that it stores (position, value).
 * When you pop it, you get back the oldest (position, value) pushed into it.
 * >>>>> You must guarantee that you never enque to an occupied position. <<<<<
 * Unlike a normal Queue, a PositionalMultiQueue has multiple deque ports (ways).
 * You select which way will deque a given (position, value) when you enque it.
 * If combinational, deque ports become valid on the same cycle as the enque.
 */
class PositionalMultiQueue[T <: Data](params: PositionalMultiQueueParameters[T], combinational: Boolean) extends Module
{
  val io = new Bundle {
    val enq = Valid(new PositionalMultiQueuePush(params)).flip
    val deq = Vec(params.ways, Decoupled(new PositionalMultiQueueEntry(params)))
  }

  val empty = RegInit(Vec.fill(params.ways) { Bool(true) })
  val head  = Reg(Vec(params.ways, UInt(width = log2Up(params.positions))))
  val tail  = Reg(Vec(params.ways, UInt(width = log2Up(params.positions))))
  val next  = Mem(params.positions, UInt(width = log2Up(params.positions)))
  val data  = Mem(params.positions, params.gen)
  // optimized away for synthesis; used to confirm invariant
  val guard = RegInit(UInt(0, width = params.positions))

  when (io.enq.fire()) {
    data(io.enq.bits.pos) := io.enq.bits.data
    // ensure the user never stores to the same position twice
    assert (!guard(io.enq.bits.pos))

    when (!empty(io.enq.bits.way)) {
      next(tail(io.enq.bits.way)) := io.enq.bits.pos
    }
  }
  val setGuard = io.enq.fire() << io.enq.bits.pos

  val deq = Wire(io.deq)
  io.deq <> deq

  val waySelect = UIntToOH(io.enq.bits.way, params.ways)
  var clrGuard = UInt(0)
  for (i <- 0 until params.ways) {
    val enq = io.enq.fire() && waySelect(i)
    val last = head(i) === tail(i)

    when (enq) {
      tail(i) := io.enq.bits.pos
      when (empty(i)) {
        head(i) := io.enq.bits.pos
      }
    }

    if (combinational) {
      deq(i).valid := !empty(i) || enq
      deq(i).bits.pos  := Mux(empty(i), io.enq.bits.pos, head(i))
      deq(i).bits.data := Mux(empty(i), io.enq.bits.data, data(head(i)))
    } else {
      deq(i).valid := !empty(i)
      deq(i).bits.pos := head(i)
      deq(i).bits.data := data(head(i))
    }

    when (deq(i).fire()) {
      head(i) := Mux(last, io.enq.bits.pos, next(head(i)))
    }
    clrGuard = clrGuard | (deq(i).fire() << deq(i).bits.pos)

    when (enq =/= deq(i).fire()) {
      empty(i) := deq(i).fire() && last
    }
  }

  guard := (guard | setGuard) & ~clrGuard
}

object PositionalMultiQueue
{
  def apply[T <: Data](gen: T, positions: Int, ways: Int = 1, combinational: Boolean = true) = {
    Module(new PositionalMultiQueue(PositionalMultiQueueParameters(gen, positions, ways), combinational))
  }
}
