// See LICENSE.SiFive for license details.

package util
import Chisel._

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

class RationalCrossingSource[T <: Data](gen: T) extends Module
{
  val io = new Bundle {
    val enq = DecoupledIO(gen).flip
    val deq = RationalIO(gen)
  }

  val enq = Queue(io.enq, 1, flow=true)
  val deq = io.deq

  val count = RegInit(UInt(0, width = 2))
  val equal = count === deq.sink

  deq.valid  := enq.valid
  deq.source := count
  deq.bits   := Mux(equal, enq.bits, RegEnable(enq.bits, equal && enq.valid))
  enq.ready  := Mux(equal, deq.ready, count(1) =/= deq.sink(0))

  when (enq.fire()) { count := Cat(count(0), !count(1)) }
}

class RationalCrossingSink[T <: Data](gen: T) extends Module
{
  val io = new Bundle {
    val enq = RationalIO(gen).flip
    val deq = DecoupledIO(gen)
  }

  val enq = io.enq
  val deq = Wire(io.deq)
  io.deq <> Queue(deq, 1, pipe=true)

  val count = RegInit(UInt(0, width = 2))
  val equal = count === enq.source

  enq.ready := deq.ready
  enq.sink  := count
  deq.bits  := enq.bits
  deq.valid := Mux(equal, enq.valid, count(1) =/= enq.source(0))

  when (deq.fire()) { count := Cat(count(0), !count(1)) }
}

class RationalCrossing[T <: Data](gen: T) extends Module
{
  val io = new CrossingIO(gen)

  val source = Module(new RationalCrossingSource(gen))
  val sink   = Module(new RationalCrossingSink(gen))

  source.clock := io.enq_clock
  source.reset := io.enq_reset
  sink  .clock := io.deq_clock
  sink  .reset := io.deq_reset

  source.io.enq <> io.enq
  io.deq <> sink.io.deq
}

object ToRational
{
  def apply[T <: Data](x: DecoupledIO[T]): RationalIO[T] = {
    val source = Module(new RationalCrossingSource(x.bits))
    source.io.enq <> x
    source.io.deq
  }
}

object FromRational
{
  def apply[T <: Data](x: RationalIO[T]): DecoupledIO[T] = {
    val sink = Module(new RationalCrossingSink(x.bits))
    sink.io.enq <> x
    sink.io.deq
  }
}
