// See LICENSE for license details.

package util
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
  val next  = Reg(Vec(params.positions, UInt(width = log2Up(params.positions))))
  val data  = Reg(Vec(params.positions, params.gen))
  // optimized away for synthesis; used to confirm invariant
  val guard = RegInit(Vec.fill(params.positions) { Bool(false) })

  when (io.enq.fire()) {
    data(io.enq.bits.pos) := io.enq.bits.data
    // ensure the user never stores to the same position twice
    assert (!guard(io.enq.bits.pos))
    guard(io.enq.bits.pos) := Bool(true)
  }

  val waySelect = UIntToOH(io.enq.bits.way, params.ways)
  for (i <- 0 until params.ways) {
    val enq = io.enq.fire() && waySelect(i)
    val last = head(i) === tail(i)

    when (enq) {
      tail(i) := io.enq.bits.pos
      when (empty(i)) {
        head(i) := io.enq.bits.pos
      } .otherwise {
        next(tail(i)) := io.enq.bits.pos
      }
    }

    if (combinational) {
      io.deq(i).valid := !empty(i) || enq
      io.deq(i).bits.pos  := Mux(empty(i), io.enq.bits.pos, head(i))
      io.deq(i).bits.data := Mux(empty(i), io.enq.bits.data, data(head(i)))
    } else {
      io.deq(i).valid := !empty(i)
      io.deq(i).bits.pos := head(i)
      io.deq(i).bits.data := data(head(i))
    }

    when (io.deq(i).fire()) {
      head(i) := Mux(last, io.enq.bits.pos, next(head(i)))
      guard(io.deq(i).bits.pos) := Bool(false)
    }

    when (enq =/= io.deq(i).fire()) {
      empty(i) := io.deq(i).fire() && last
    }
  }
}

object PositionalMultiQueue
{
  def apply[T <: Data](gen: T, positions: Int, ways: Int = 1, combinational: Boolean = true) = {
    Module(new PositionalMultiQueue(PositionalMultiQueueParameters(gen, positions, ways), combinational))
  }
}
