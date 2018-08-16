// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket.fpucp

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object FPUCPImp extends SimpleNodeImp[FPUCPNullParameters, FPUCPSinkParameters, FPUCPSinkParameters, FPUCPBundle] {
  def edge(pd: FPUCPNullParameters, pu: FPUCPSinkParameters, p: Parameters, sourceInfo: SourceInfo) = pu
  def bundle(e: FPUCPSinkParameters) = FPUCPBundle(e)
  def render(e: FPUCPSinkParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label = (e.fLen).toString)
}

// Nodes implemented inside modules
case class FPUCPSourceNode()(implicit valName: ValName) extends SourceNode(FPUCPImp)(Seq(FPUCPNullParameters()))
case class FPUCPSinkNode(portParams: FPUCPSinkParameters)(implicit valName: ValName) extends SinkNode(FPUCPImp)(Seq(portParams))
case class FPUCPNexusNode(
  sinkFn:        Seq[FPUCPSinkParameters]  => FPUCPSinkParameters)(
  implicit valName: ValName)
  extends NexusNode(FPUCPImp)(
    { _: Seq[FPUCPNullParameters] => FPUCPNullParameters()},
    sinkFn,
    outputRequiresInput = false
  )

