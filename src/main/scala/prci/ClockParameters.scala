// See LICENSE.SiFive for license details.
package freechips.rocketchip.prci

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo

import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy.{InModuleBody, ModuleValue, ValName}
import freechips.rocketchip.subsystem.{Attachable, HierarchicalLocation}
import freechips.rocketchip.util._
import scala.math.max
import scala.collection.immutable.ListMap

class ClockSinkLocation(override val name: String) extends Location[ClockSinkNode](name)
class ClockSourceLocation(override val name: String) extends Location[FixedClockBroadcastNode](name)

case class ClockTopologyLocated(loc: HierarchicalLocation) extends Field[Seq[(Location[ClockSinkNode],Location[FixedClockBroadcastNode])]](Nil)

case class ClockTopology(val connections: Seq[(Location[ClockSinkNode], Location[FixedClockBroadcastNode])])

trait HasConfigurableClockTopology { this: Attachable =>
  def location: HierarchicalLocation

  p(ClockTopologyLocated(location)).foreach { case(sinkLocation, sourceLocation) =>
    println(s"${sinkLocation} := ${sourceLocation}")
    locateClockSink(sinkLocation) := locateClockSource(sourceLocation)
  }
}

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
  val clock = source.give.orElse(sink.take).map { clock =>
    source.give.foreach { x => require (clock == x) }
    sink.take.foreach   { x => require (clock == x) }
    clock
  }

  val bundle = ClockBundleParameters()
}

// ClockGroups exist as the output of a PLL

case class ClockGroupSourceParameters()
case class ClockGroupSinkParameters(
  name: String,
  members: Seq[ClockSinkParameters])

case class ClockGroupBundleParameters(
  members: ListMap[String, ClockBundleParameters])

case class ClockGroupEdgeParameters(
  source:     ClockGroupSourceParameters,
  sink:       ClockGroupSinkParameters,
  params:     Parameters,
  sourceInfo: SourceInfo)
{
  val sourceParameters = ClockSourceParameters()
  val members: ListMap[String, ClockEdgeParameters] = ListMap(
    sink.members.zipWithIndex.map { case (s, i) =>
      s"${sink.name}_${i}" -> ClockEdgeParameters(sourceParameters, s, params, sourceInfo)
  }:_*)

  val bundle = ClockGroupBundleParameters(members.map{ case (k, v) => k -> v.bundle})
}

// Used to create simple clock group drivers that just use the Chisel implicit clock
case class ClockGroupDriverParameters(
  num: Int = 1,
  driveFn: ClockGroupDriver.DriveFn = ClockGroupDriver.driveFromImplicitClock
) {
  def drive(node: ClockGroupEphemeralNode)(implicit p: Parameters, vn: ValName): ModuleValue[HeterogeneousBag[ClockGroupBundle]] = {
    driveFn(node, num, p, vn)
  }
}

object ClockGroupDriver {
  type DriveFn = (ClockGroupEphemeralNode, Int, Parameters, ValName) => ModuleValue[HeterogeneousBag[ClockGroupBundle]]

  def driveFromImplicitClock: DriveFn = { (groups, num, p, vn) =>
    implicit val pp = p
    val dummyClockGroupSourceNode: ClockGroupSourceNode = SimpleClockGroupSource(num)
    groups :*= dummyClockGroupSourceNode
    InModuleBody { HeterogeneousBag[ClockGroupBundle](Nil) }
  }

  def driveFromIOs: DriveFn = { (groups, num, p, vn) =>
    implicit val pp = p
    val ioClockGroupSourceNode = ClockGroupSourceNode(List.fill(num) { ClockGroupSourceParameters() })
    groups :*= ioClockGroupSourceNode
    InModuleBody { ioClockGroupSourceNode.makeIOs()(vn) }
  }
}
