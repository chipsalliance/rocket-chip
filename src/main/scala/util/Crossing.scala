// See LICENSE.SiFive for license details.

package util

import Chisel._
import chisel3.util.{DecoupledIO, Decoupled, Irrevocable, IrrevocableIO, ReadyValidIO}

class CrossingIO[T <: Data](gen: T) extends Bundle {
  // Enqueue clock domain
  val enq_clock = Clock(INPUT)
  val enq_reset = Bool(INPUT) // synchronously deasserted wrt. enq_clock
  val enq = Decoupled(gen).flip
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
  def apply[T <: Data](from_clock: Clock, from_reset: Bool, from_source: ReadyValidIO[T], to_clock: Clock, to_reset: Bool, depth: Int = 8, sync: Int = 3): DecoupledIO[T] = {
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
  def apply[T <: Data](to_clock: Clock, to_reset: Bool, source: ReadyValidIO[T], depth: Int = 8, sync: Int = 3): DecoupledIO[T] = {
    val scope = AsyncScope()
    AsyncDecoupledCrossing(scope.clock, scope.reset, source, to_clock, to_reset, depth, sync)
  }
}

object AsyncDecoupledFrom
{
  // takes from_source from the 'from' clock domain and puts it into your clock domain
  def apply[T <: Data](from_clock: Clock, from_reset: Bool, from_source: ReadyValidIO[T], depth: Int = 8, sync: Int = 3): DecoupledIO[T] = {
    val scope = AsyncScope()
    AsyncDecoupledCrossing(from_clock, from_reset, from_source, scope.clock, scope.reset, depth, sync)
  }
}

object PostQueueIrrevocablize
{
  def apply[T <: Data](deq: DecoupledIO[T]): IrrevocableIO[T] = {
    val irr = Wire(new IrrevocableIO(deq.bits))
    irr.bits := deq.bits
    irr.valid := deq.valid
    deq.ready := irr.ready
    irr
  }
}

object AsyncIrrevocableCrossing {
  def apply[T <: Data](from_clock: Clock, from_reset: Bool, from_source: ReadyValidIO[T], to_clock: Clock, to_reset: Bool, depth: Int = 8, sync: Int = 3): IrrevocableIO[T] = {
    PostQueueIrrevocablize(AsyncDecoupledCrossing(from_clock, from_reset, from_source, to_clock, to_reset, depth, sync))
  }
}

object AsyncIrrevocableTo
{
  // takes source from your clock domain and puts it into the 'to' clock domain
  def apply[T <: Data](to_clock: Clock, to_reset: Bool, source: ReadyValidIO[T], depth: Int = 8, sync: Int = 3): IrrevocableIO[T] = {
    PostQueueIrrevocablize(AsyncDecoupledTo(to_clock, to_reset, source, depth, sync))
  }
}

object AsyncIrrevocableFrom
{
  // takes from_source from the 'from' clock domain and puts it into your clock domain
  def apply[T <: Data](from_clock: Clock, from_reset: Bool, from_source: ReadyValidIO[T], depth: Int = 8, sync: Int = 3): IrrevocableIO[T] = {
    PostQueueIrrevocablize(AsyncDecoupledFrom(from_clock, from_reset, from_source, depth, sync))
  }
}

/**
 * This helper object synchronizes a level-sensitive signal from one
 * clock domain to another.
 */
object LevelSyncCrossing {
  class SynchronizerBackend(sync: Int, _clock: Clock) extends Module(Some(_clock)) {
    val io = new Bundle {
      val in = Bool(INPUT)
      val out = Bool(OUTPUT)
    }

    io.out := ShiftRegister(io.in, sync)
  }

  class SynchronizerFrontend(_clock: Clock) extends Module(Some(_clock)) {
    val io = new Bundle {
      val in = Bool(INPUT)
      val out = Bool(OUTPUT)
    }

    io.out := RegNext(io.in)
  }

  def apply(from_clock: Clock, to_clock: Clock, in: Bool, sync: Int = 2): Bool = {
    val front = Module(new SynchronizerFrontend(from_clock))
    val back = Module(new SynchronizerBackend(sync, to_clock))

    front.io.in := in
    back.io.in := front.io.out
    back.io.out
  }
}

object LevelSyncTo {
  def apply(to_clock: Clock, in: Bool, sync: Int = 2): Bool = {
    val scope = AsyncScope()
    LevelSyncCrossing(scope.clock, to_clock, in, sync)
  }
}

object LevelSyncFrom {
  def apply(from_clock: Clock, in: Bool, sync: Int = 2): Bool = {
    val scope = AsyncScope()
    LevelSyncCrossing(from_clock, scope.clock, in, sync)
  }
}
