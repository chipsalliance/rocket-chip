package junctions
import Chisel._

class CrossingIO[T <: Data](gen: T) extends Bundle {
  // Enqueue clock domain
  val enq_clock = Clock(INPUT)
  val enq_reset = Bool(INPUT) // synchronously deasserted wrt. enq_clock
  val enq = Decoupled(gen).flip()
  // Dequeue clock domain
  val deq_clock = Clock(INPUT)
  val deq_reset = Bool(INPUT) // synchronously deasserted wrt. deq_clock
  val deq = Decoupled(gen)
}

abstract class Crossing[T <: Data] extends Module {
  val io: CrossingIO[T]
}

class AsyncScope extends Module { val io = new Bundle }
object AsyncScope { def apply() = Module(new AsyncScope) }

object AsyncDecoupledCrossing
{
  // takes from_source from the 'from' clock domain and puts it into the 'to' clock domain
  def apply[T <: Data](from_clock: Clock, from_reset: Bool, from_source: DecoupledIO[T], to_clock: Clock, to_reset: Bool, depth: Int = 8, sync: Int = 3): DecoupledIO[T] = {
    val crossing = Module(new AsyncQueue(from_source.bits, depth, sync)).io
    crossing.enq_clock := from_clock
    crossing.enq_reset := from_reset
    crossing.enq       <> from_source
    crossing.deq_clock := to_clock
    crossing.deq_reset := to_reset
    crossing.deq
  }
}

object AsyncDecoupledTo
{
  // takes source from your clock domain and puts it into the 'to' clock domain
  def apply[T <: Data](to_clock: Clock, to_reset: Bool, source: DecoupledIO[T], depth: Int = 8, sync: Int = 3): DecoupledIO[T] = {
    val scope = AsyncScope()
    AsyncDecoupledCrossing(scope.clock, scope.reset, source, to_clock, to_reset, depth, sync)
  }
}

object AsyncDecoupledFrom
{
  // takes from_source from the 'from' clock domain and puts it into your clock domain
  def apply[T <: Data](from_clock: Clock, from_reset: Bool, from_source: DecoupledIO[T], depth: Int = 8, sync: Int = 3): DecoupledIO[T] = {
    val scope = AsyncScope()
    AsyncDecoupledCrossing(from_clock, from_reset, from_source, scope.clock, scope.reset, depth, sync)
  }
}
