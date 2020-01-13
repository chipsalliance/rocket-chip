// See LICENSE.SiFive for license details.
package freechips.rocketchip.prci

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object ClockImp extends SimpleNodeImp[ClockSourceParameters, ClockSinkParameters, ClockEdgeParameters, ClockBundle]
{
  def edge(pd: ClockSourceParameters, pu: ClockSinkParameters, p: Parameters, sourceInfo: SourceInfo) = ClockEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: ClockEdgeParameters) = new ClockBundle(e.bundle)
  def render(e: ClockEdgeParameters) = RenderedEdge(colour = "#00cc00" /* green */)
}

case class ClockSourceNode(val portParams: Seq[ClockSourceParameters])(implicit valName: ValName) extends SourceNode(ClockImp)(portParams)
{
  def fixedClockResources(name: String, prefix: String = "soc/"): Seq[Option[FixedClockResource]] = portParams.map { p =>
    p.give.map(g => new FixedClockResource(name, g.freqMHz, prefix))
  }
}

case class ClockSinkNode(val portParams: Seq[ClockSinkParameters])(implicit valName: ValName) extends SinkNode(ClockImp)(portParams)
{
  def fixedClockResources(name: String, prefix: String = "soc/"): Seq[Option[FixedClockResource]] = portParams.map { p =>
    p.take.map(t => new FixedClockResource(name, t.freqMHz, prefix))
  }
}

case class ClockAdapterNode(
  sourceFn: ClockSourceParameters => ClockSourceParameters = { m => m },
  sinkFn:   ClockSinkParameters   => ClockSinkParameters   = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(ClockImp)(sourceFn, sinkFn)
case class ClockIdentityNode()(implicit valName: ValName) extends IdentityNode(ClockImp)()

object ClockSinkNode
{
  def apply(
    freqMHz:       Double,
    dutyCycle:     Double = 50,
    phaseDeg:      Double = 0,
    // Create SDC/TCL constraints that the clock matches these requirements:
    phaseErrorDeg: Double = 5,
    freqErrorPPM:  Double = 10000,
    jitterPS:      Double = 300)(implicit valName: ValName): ClockSinkNode =
    ClockSinkNode(Seq(ClockSinkParameters(
      phaseDeg      = phaseDeg,
      phaseErrorDeg = phaseErrorDeg,
      freqErrorPPM  = freqErrorPPM,
      jitterPS      = jitterPS,
      take          = Some(ClockParameters(
        freqMHz     = freqMHz,
        dutyCycle   = dutyCycle)))))
}

object ClockSourceNode
{
  def apply(
    freqMHz:   Double,
    dutyCycle: Double = 50,
    jitterPS:  Double = 300)(implicit valName: ValName): ClockSourceNode =
    ClockSourceNode(Seq(ClockSourceParameters(
      jitterPS    = Some(jitterPS),
      give        = Some(ClockParameters(
        freqMHz   = freqMHz,
        dutyCycle = dutyCycle)))))
}

object ClockGroupImp extends SimpleNodeImp[ClockGroupSourceParameters, ClockGroupSinkParameters, ClockGroupEdgeParameters, ClockGroupBundle]
{
  def edge(pd: ClockGroupSourceParameters, pu: ClockGroupSinkParameters, p: Parameters, sourceInfo: SourceInfo) = ClockGroupEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: ClockGroupEdgeParameters) = new ClockGroupBundle(e.bundle)
  def render(e: ClockGroupEdgeParameters) = RenderedEdge(colour = "#00cc00" /* green */)
}

case class ClockGroupSourceNode(params: Seq[ClockGroupSourceParameters])(implicit valName: ValName) extends SourceNode(ClockGroupImp)(params)
case class ClockGroupSinkNode(params: Seq[ClockGroupSinkParameters])(implicit valName: ValName) extends SinkNode(ClockGroupImp)(params)

case class ClockGroupAdapterNode(
  sourceFn: ClockGroupSourceParameters => ClockGroupSourceParameters = { m => m },
  sinkFn:   ClockGroupSinkParameters   => ClockGroupSinkParameters   = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(ClockGroupImp)(sourceFn, sinkFn)
case class ClockGroupIdentityNode()(implicit valName: ValName) extends IdentityNode(ClockGroupImp)()
