// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.{Parameters,Field}

case class BundleBridgeParams[T <: Data](gen: () => T)
case class BundleBridgeNull()

class BundleBridgeImp[T <: Data]() extends SimpleNodeImp[BundleBridgeParams[T], BundleBridgeNull, BundleBridgeParams[T], T]
{
  def edge(pd: BundleBridgeParams[T], pu: BundleBridgeNull, p: Parameters, sourceInfo: SourceInfo) = pd
  def bundle(e: BundleBridgeParams[T]) = e.gen()
  def render(e: BundleBridgeParams[T]) = RenderedEdge(colour = "#cccc00" /* yellow */)
}

case class BundleBridgeSink[T <: Data]()(implicit valName: ValName) extends SinkNode(new BundleBridgeImp[T])(Seq(BundleBridgeNull()))
{
  def io: T = in(0)._1
}

case class BundleBridgeSource[T <: Data](gen: () => T)(implicit valName: ValName) extends SourceNode(new BundleBridgeImp[T])(Seq(BundleBridgeParams(gen)))
{
  def sink(implicit p: Parameters) = {
    val sink = BundleBridgeSink[T]()
    sink := this
    sink
  }
}

class BundleBridge[D <: Data, T <: LazyModule](lm: => T { val module: { val io: D }})(implicit p: Parameters) extends LazyModule
{
  val child = LazyModule(lm)
  val ioNode = BundleBridgeSource(() => child.module.io.cloneType)
  override lazy val desiredName = s"BundleBridge_${child.desiredName}"

  lazy val module = new LazyModuleImp(this) {
    val (io, _) = ioNode.out(0)
    io <> child.module.io
  }
}

/* Usage:
 *   // Wrap up SomeDevice's module.io Bundle into an ioNode
 *   val deviceBridge = BundleBridge(new SomeDevice) // BundleBridge() replaces LazyModule()
 *   // Somewhere else in the design in LazyModule scope:
 *   val sink = deviceBridge.ioNode.sink // creates a sink node of matching type
 *   // In LazyModuleImp scope:
 *   val io = sink.io // io is a Wire () connected to device.module.io
 */
object BundleBridge
{
  def apply[D <: Data, T <: LazyModule](lm: => T { val module: { val io: D }})(implicit p: Parameters, valName: ValName) =
    LazyModule(new BundleBridge(lm))
}
