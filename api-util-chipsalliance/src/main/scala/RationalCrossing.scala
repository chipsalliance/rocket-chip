// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._

// If you know two clocks are related with an N:M relationship, you
// can cross the clock domains with lower latency than an AsyncQueue.
// This crossing adds 1 cycle in the target clock domain.

// A rational crossing must put registers on the slow side.
// This trait covers the options of how/where to put the registers.
// BEWARE: the source+sink must agree on the direction!
sealed trait RationalDirection {
  def flip: RationalDirection
}

// If it's unclear which side will be slow (or it is variable),
// place registers on both sides of the crossing, by splitting
// a Queue into flow and pipe parts on either side. This is safe
// for all possible clock ratios, but has the downside that the
// timing must be met for the least-common-multiple of the clocks.
case object Symmetric extends RationalDirection {
  def flip = Symmetric
}

// Like Symmetric, this crossing works for all ratios N:M.
// However, unlike the other crossing options, this varient adds
// a full flow+pipe buffer on both sides of the crossing. This
// ends up costing potentially two cycles of delay, but gives
// both clock domains a full clock period to close timing.
case object Flexible extends RationalDirection {
  def flip = Flexible
}

// If the source is N:1 of the sink, place the registers at the sink.
// This imposes only a single clock cycle of delay and both side of
// the crossing have a full clock period to close timing.
case object FastToSlow extends RationalDirection {
  def flip = SlowToFast
}

// If the source is 1:N of the sink, place the registers at the source.
// This imposes only a single clock cycle of delay and both side of
// the crossing have a full clock period to close timing.
case object SlowToFast extends RationalDirection {
  def flip = FastToSlow
}

final class RationalIO[T <: Data](gen: T) extends Bundle
{
  val bits0  = gen.asOutput
  val bits1  = gen.asOutput
  val valid  = Bool(OUTPUT)
  val source = UInt(OUTPUT, width = 2)
  val ready  = Bool(INPUT)
  val sink   = UInt(INPUT, width = 2)

  override def cloneType: this.type = new RationalIO(gen).asInstanceOf[this.type]
}

object RationalIO
{
  def apply[T <: Data](gen: T) = new RationalIO(gen)
}

class RationalCrossingSource[T <: Data](gen: T, direction: RationalDirection = Symmetric) extends Module
{
  val io = new Bundle {
    val enq = DecoupledIO(gen).flip
    val deq = RationalIO(gen)
  }

  val deq = io.deq
  val enq = direction match {
    case Symmetric  => ShiftQueue(io.enq, 1, flow=true)
    case Flexible => ShiftQueue(io.enq, 2)
    case FastToSlow => io.enq
    case SlowToFast => ShiftQueue(io.enq, 2)
  }

  val count = RegInit(UInt(0, width = 2))
  val equal = count === deq.sink

  deq.valid  := enq.valid
  deq.source := count
  deq.bits0  := enq.bits
  deq.bits1  := RegEnable(enq.bits, equal)
  enq.ready  := Mux(equal, deq.ready, count(1) =/= deq.sink(0))

  when (enq.fire()) { count := Cat(count(0), !count(1)) }

  // Ensure the clocking is setup correctly
  direction match {
    case Symmetric  => () // always safe
    case Flexible   => ()
    case FastToSlow => assert (equal || count(1) === deq.sink(0))
    case SlowToFast => assert (equal || count(1) =/= deq.sink(0))
  }
}

class RationalCrossingSink[T <: Data](gen: T, direction: RationalDirection = Symmetric) extends Module
{
  val io = new Bundle {
    val enq = RationalIO(gen).flip
    val deq = DecoupledIO(gen)
  }

  val enq = io.enq
  val deq = Wire(io.deq)
  direction match {
    case Symmetric  => io.deq <> ShiftQueue(deq, 1, pipe=true)
    case Flexible   => io.deq <> ShiftQueue(deq, 2)
    case FastToSlow => io.deq <> ShiftQueue(deq, 2)
    case SlowToFast => io.deq <> deq
  }

  val count = RegInit(UInt(0, width = 2))
  val equal = count === enq.source

  enq.ready := deq.ready
  enq.sink  := count
  deq.bits  := Mux(equal, enq.bits0, enq.bits1)
  deq.valid := Mux(equal, enq.valid, count(1) =/= enq.source(0))

  when (deq.fire()) { count := Cat(count(0), !count(1)) }

  // Ensure the clocking is setup correctly
  direction match {
    case Symmetric  => () // always safe
    case Flexible   => ()
    case FastToSlow => assert (equal || count(1) =/= enq.source(0))
    case SlowToFast => assert (equal || count(1) === enq.source(0))
  }
}

class RationalCrossingFull[T <: Data](gen: T, direction: RationalDirection = Symmetric) extends Module
{
  val io = new CrossingIO(gen)

  val source = Module(new RationalCrossingSource(gen, direction))
  val sink   = Module(new RationalCrossingSink(gen, direction))

  source.clock := io.enq_clock
  source.reset := io.enq_reset
  sink  .clock := io.deq_clock
  sink  .reset := io.deq_reset

  source.io.enq <> io.enq
  io.deq <> sink.io.deq
}

object ToRational
{
  def apply[T <: Data](x: DecoupledIO[T], direction: RationalDirection = Symmetric): RationalIO[T] = {
    val source = Module(new RationalCrossingSource(x.bits, direction))
    source.io.enq <> x
    source.io.deq
  }
}

object FromRational
{
  def apply[T <: Data](x: RationalIO[T], direction: RationalDirection = Symmetric): DecoupledIO[T] = {
    val sink = Module(new RationalCrossingSink(x.bits0, direction))
    sink.io.enq <> x
    sink.io.deq
  }
}
