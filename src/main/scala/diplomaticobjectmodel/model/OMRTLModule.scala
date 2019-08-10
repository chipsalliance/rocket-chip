// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

// Examples of ClockRelationships
//  {
//   "clock0": "core_clock_0",
//    "clock1": "clock",
//    "relationship": "$clock0 == m * $clock1 where m is a positive integer",
//  }

//  {
//    "clock1": "rtc_toggle",
//    "relationship": "$clock0 >= 2 * $clock1",
//  }

trait OMStatus extends OMSignal {
  name: String
  description: Option[String]
}

trait OMCoreStatus extends OMStatus {
  name: String
  description: Option[String]
  def hartId: Int
}

case class Halt(
  name: String,
  description: Option[String],
  hartId: Int
) extends OMCoreStatus

case class WFI(
  name: String,
  description: Option[String],
  hartId: Int
) extends OMCoreStatus

case class Trace(
  name: String,
  description: Option[String],
  hartId: Int
) extends OMCoreStatus

trait RTLComponent extends OMCompoundType

trait OMSignal extends RTLComponent {
  def name: String // This will always be the name of the signal on the top-level module
  def description: Option[String]
}

case class OMClock(
  name: String,
  description: Option[String]
) extends OMSignal

case class OMClockRelationship(
  clock0: String,
  clock1: String,
  relationship: String,
) extends RTLComponent

trait OMSignalAssertionLevel extends OMEnum
trait High extends OMSignalAssertionLevel
trait Low extends OMSignalAssertionLevel

trait Synchronicity extends OMEnum
trait Synchronous extends Synchronicity
trait Asynchronous extends Synchronicity

case class OMRTLReset(
  name: String,
  description: Option[String],
  activeEdge: Option[OMSignalAssertionLevel],
  clock: String, // This will always be the name of the clock signal on the to p-level module
  synchronicity: Option[Synchronicity]
) extends OMSignal

case class OMResetVector(
  name: String,
  description: Option[String],
  width: Int
) extends OMSignal

// Interrupts
case class InterruptSignal(
  name: String,
  description: Option[String],
  width: Int
) extends OMSignal

case class OMRTLInterface(
  clocks: Seq[OMClock],
  clockRelationships: Seq[OMClockRelationship],
  resets: Seq[OMRTLReset],
  statuses: Seq[OMStatus]
) extends RTLComponent

case class OMRTLModule(
  moduleName: String,
  instanceName: Option[String],  // TODO: This does not exist for the top-level module because the top-level module is the only one that is not instantiated
  hierarchicalId: Option[String],  // Full dotted path from the root, where the root is described as a module name while all other path components are instance names
  interface: OMRTLInterface
)