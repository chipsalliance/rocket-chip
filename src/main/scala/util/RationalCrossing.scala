// See LICENSE.SiFive for license details.

// If you know two clocks are related with a N:1 or 1:N relationship, you
// can cross the clock domains with lower latency than an AsyncQueue. This
// crossing adds 1 cycle in the target clock domain.

package util
import Chisel._

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
// path from the slow domain must close timing in the fast domain.
case object Symmetric extends RationalDirection {
  def flip = Symmetric
}

// If the source is fast, place the registers at the sink.
case object FastToSlow extends RationalDirection {
  def flip = SlowToFast
}

// If the source is slow, place the registers at the source.
case object SlowToFast extends RationalDirection {
  def flip = FastToSlow
}

final class RationalIO[T <: Data](gen: T) extends Bundle
{
  val bits   = gen.chiselCloneType
  val valid  = Bool()
  val source = UInt(width = 2)
  val ready  = Bool().flip
  val sink   = UInt(width = 2).flip

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
    case Symmetric  => Queue(io.enq, 1, flow=true)
    case FastToSlow => io.enq
    case SlowToFast => Queue(io.enq, 2)
  }

  val count = RegInit(UInt(0, width = 2))
  val equal = count === deq.sink

  deq.valid  := enq.valid
  deq.source := count
  deq.bits   := Mux(equal, enq.bits, RegEnable(enq.bits, equal))
  enq.ready  := Mux(equal, deq.ready, count(1) =/= deq.sink(0))

  when (enq.fire()) { count := Cat(count(0), !count(1)) }

  // Ensure the clocking is setup correctly
  direction match {
    case Symmetric  => () // always safe
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
    case Symmetric  => io.deq <> Queue(deq, 1, pipe=true)
    case FastToSlow => io.deq <> Queue(deq, 2)
    case SlowToFast => io.deq <> deq
  }

  val count = RegInit(UInt(0, width = 2))
  val equal = count === enq.source

  enq.ready := deq.ready
  enq.sink  := count
  deq.bits  := enq.bits
  deq.valid := Mux(equal, enq.valid, count(1) =/= enq.source(0))

  when (deq.fire()) { count := Cat(count(0), !count(1)) }

  // Ensure the clocking is setup correctly
  direction match {
    case Symmetric  => () // always safe
    case FastToSlow => assert (equal || count(1) =/= enq.source(0))
    case SlowToFast => assert (equal || count(1) === enq.source(0))
  }
}

class RationalCrossing[T <: Data](gen: T, direction: RationalDirection = Symmetric) extends Module
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
    val sink = Module(new RationalCrossingSink(x.bits, direction))
    sink.io.enq <> x
    sink.io.deq
  }
}
