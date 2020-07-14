// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.CrossingWrapper
import freechips.rocketchip.util._

class AXI4CreditedBuffer(delay: AXI4CreditedDelay)(implicit p: Parameters) extends LazyModule
{
  val node = AXI4CreditedAdapterNode(
    masterFn = p => p.copy(delay = delay + p.delay),
    slaveFn  = p => p.copy(delay = delay + p.delay))

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.aw :<> in.aw.pipeline(delay.aw)
      out.w :<> in.w.pipeline(delay.w)
      in.b :<> out.b.pipeline(delay.b)
      out.ar :<> in.ar.pipeline(delay.ar)
      in.r :<> out.r.pipeline(delay.r)
    }
  }
}

object AXI4CreditedBuffer {
  def apply(delay: AXI4CreditedDelay)(implicit p: Parameters): AXI4CreditedAdapterNode = {
    val buffer = LazyModule(new AXI4CreditedBuffer(delay))
    buffer.node
  }
  def apply(delay: CreditedDelay)(implicit p: Parameters): AXI4CreditedAdapterNode = apply(AXI4CreditedDelay(delay))
  def apply()(implicit p: Parameters): AXI4CreditedAdapterNode = apply(CreditedDelay(1, 1))
}

class AXI4CreditedSource(delay: AXI4CreditedDelay)(implicit p: Parameters) extends LazyModule
{
  val node = AXI4CreditedSourceNode(delay)
  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val tld = edgeOut.delay
      out.aw :<> CreditedIO.fromSender(in.aw, tld.aw.total).pipeline(delay.aw)
      out.w :<> CreditedIO.fromSender(in.w, tld.w.total).pipeline(delay.w)
      in.b :<> out.b.pipeline(delay.b).toReceiver(tld.b.total)
      out.ar :<> CreditedIO.fromSender(in.ar, tld.ar.total).pipeline(delay.ar)
      in.r :<> out.r.pipeline(delay.r).toReceiver(tld.r.total)
    }
  }
}

object AXI4CreditedSource {
  def apply(delay: AXI4CreditedDelay)(implicit p: Parameters): AXI4CreditedSourceNode = {
    val source = LazyModule(new AXI4CreditedSource(delay))
    source.node
  }
  def apply(delay: CreditedDelay)(implicit p: Parameters): AXI4CreditedSourceNode = apply(AXI4CreditedDelay(delay))
  def apply()(implicit p: Parameters): AXI4CreditedSourceNode = apply(CreditedDelay(1, 1))
}

class AXI4CreditedSink(delay: AXI4CreditedDelay)(implicit p: Parameters) extends LazyModule
{
  val node = AXI4CreditedSinkNode(delay)
  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val tld = edgeIn.delay
      out.aw :<> in.aw.pipeline(delay.aw).toReceiver(tld.aw.total)
      out.w :<> in.w.pipeline(delay.w).toReceiver(tld.w.total)
      in.b :<> CreditedIO.fromSender(out.b, tld.b.total).pipeline(delay.b)
      out.ar :<> in.ar.pipeline(delay.ar).toReceiver(tld.ar.total)
      in.r :<> CreditedIO.fromSender(out.r, tld.r.total).pipeline(delay.r)
    }
  }
}

object AXI4CreditedSink {
  def apply(delay: AXI4CreditedDelay)(implicit p: Parameters): AXI4CreditedSinkNode = {
    val sink = LazyModule(new AXI4CreditedSink(delay))
    sink.node
  }
  def apply(delay: CreditedDelay)(implicit p: Parameters): AXI4CreditedSinkNode = apply(AXI4CreditedDelay(delay))
  def apply()(implicit p: Parameters): AXI4CreditedSinkNode = apply(CreditedDelay(1, 1))
}
