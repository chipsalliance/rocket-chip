// See LICENSE.SiFive for license details.
package freechips.rocketchip.clocks

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import scala.math.max

// All Clock parameters specify only the PLL values required at power-on
// Dynamic control of the PLL from software can take the values out-of-range

case class ClockParameters(
  freqMHz:   Double,
  dutyCycle: Double = 50) //in percent 
{
  require (freqMHz > 0)
  require (0 < dutyCycle)
  require (dutyCycle < 100)
}

case class ClockSourceParameters(
  jitterPS: Option[Double] = None, // if known at chisel elaboration
  give:     Option[ClockParameters] = None)

case class ClockSinkParameters(
  phaseDeg:      Double = 0,
  // Create SDC/TCL constraints that the clock matches these requirements:
  phaseErrorDeg: Double = 5,
  freqErrorPPM:  Double = 10000,
  jitterPS:      Double = 200,
  take:          Option[ClockParameters] = None) 
{
  require (phaseErrorDeg >= 0)
  require (freqErrorPPM >= 0)
}

case class ClockBundleParameters()

case class ClockEdgeParameters(
  source:     ClockSourceParameters,
  sink:       ClockSinkParameters,
  params:     Parameters,
  sourceInfo: SourceInfo)
{
  // Unify the given+taken ClockParameters
  require (!source.give.isEmpty || !sink.take.isEmpty)
  val clock = source.give.orElse(sink.take).get
  source.give.foreach { x => require (clock == x) }
  sink.take.foreach   { x => require (clock == x) }

  val bundle = ClockBundleParameters()
}

// ClockGroups exist as the output of a PLL

case class ClockGroupSourceParameters()
case class ClockGroupSinkParameters(
  name: String,
  members: Seq[ClockSinkParameters])

case class ClockGroupBundleParameters(
  members: Seq[ClockBundleParameters])

case class ClockGroupEdgeParameters(
  source:     ClockGroupSourceParameters,
  sink:       ClockGroupSinkParameters,
  params:     Parameters,
  sourceInfo: SourceInfo)
{
  val sourceParameters = ClockSourceParameters()
  val members = sink.members.map { s =>
    ClockEdgeParameters(sourceParameters, s, params, sourceInfo)
  }

  val bundle = ClockGroupBundleParameters(members.map(_.bundle))
}
