// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import chisel3.util.{ReadyValidIO}

final class AsyncBundle[T <: Data](val depth: Int, gen: T) extends Bundle
{
  require (isPow2(depth))
  val mem  = Vec(depth, gen)
  val ridx = UInt(width = log2Ceil(depth)+1).flip
  val widx = UInt(width = log2Ceil(depth)+1)
  val ridx_valid = Bool().flip
  val widx_valid = Bool()
  val source_reset_n = Bool()
  val sink_reset_n = Bool().flip

  override def cloneType: this.type = new AsyncBundle(depth, gen).asInstanceOf[this.type]
}

object FromAsyncBundle
{
  def apply[T <: Data](x: AsyncBundle[T], sync: Int = 3): DecoupledIO[T] = {
    val sink = Module(new AsyncQueueSink(x.mem(0), x.depth, sync))
    x.ridx := sink.io.ridx
    x.ridx_valid := sink.io.ridx_valid
    sink.io.widx := x.widx
    sink.io.widx_valid := x.widx_valid
    sink.io.mem  := x.mem
    sink.io.source_reset_n := x.source_reset_n
    x.sink_reset_n := !sink.reset
    val out = Wire(Decoupled(x.mem(0)))
    out.valid := sink.io.deq.valid
    out.bits := sink.io.deq.bits
    sink.io.deq.ready := out.ready
    out
  }
}

object ToAsyncBundle
{
  def apply[T <: Data](x: ReadyValidIO[T], depth: Int = 8, sync: Int = 3): AsyncBundle[T] = {
    val source = Module(new AsyncQueueSource(x.bits, depth, sync))
    source.io.enq.valid := x.valid
    source.io.enq.bits := x.bits
    x.ready := source.io.enq.ready
    val out = Wire(new AsyncBundle(depth, x.bits))
    source.io.ridx := out.ridx
    source.io.ridx_valid := out.ridx_valid
    out.mem := source.io.mem
    out.widx := source.io.widx
    out.widx_valid := source.io.widx_valid
    source.io.sink_reset_n := out.sink_reset_n
    out.source_reset_n := !source.reset
    out
  }
}

