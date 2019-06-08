// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

trait OMRTLComponent extends OMCompoundType

trait OMSignal extends OMRTLComponent {
  def name: String // This will always be the name of the signal on the top-level module
  def description: Option[String]
  def uid: Int
}

case class OMClock(
  name: String,
  description: Option[String],
  uid: Int,
  _types: Seq[String] = Seq("OMClock", "OMSignal", "OMRTLComponent", "OMCompoundType")
) extends OMSignal

case class OMClockRelationship(
  clock0: String,
  clock1: String,
  relationship: String,
  _types: Seq[String] = Seq("OMClockRelationship", "OMRTLComponent", "OMCompoundType")
) extends OMRTLComponent

trait OMSignalAssertionLevel extends OMEnum
trait High extends OMSignalAssertionLevel
trait Low extends OMSignalAssertionLevel

trait Synchronicity extends OMEnum
trait Synchronous extends Synchronicity
trait Asynchronous extends Synchronicity

case class OMRTLReset(
  activeEdge: Option[OMSignalAssertionLevel],
  clock: String, // This will always be the name of the clock signal on the to p-level module
  synchronicity: Option[Synchronicity],
  _types: Seq[String] = Seq("OMClockReset", "OMRTLComponent", "OMCompoundType")
) extends OMRTLComponent

case class OMReset(
  description: Option[String],
  name: String,
  activeEdge: Option[OMSignalAssertionLevel],
  clockName: String, // This will always be the name of the clock signal on the top-level module
  synchronicity: Option[Synchronicity],
  uid: Int,
  _types: Seq[String] = Seq("OMReset", "OMSignal", "OMRTLComponent", "OMCompoundType")
) extends OMSignal

// Status
trait OMStatus extends OMSignal {
  def description: Option[String]
  def name: String
  uid: Int
}

trait OMCoreStatus extends OMStatus {
  def description: Option[String]
  def name: String
  def hartId: Int
}

case class OMHalt(
  description: Option[String],
  name: String,
  hartId: Int,
  uid: Int,
  _types: Seq[String] = Seq("OMHalt", "OMCoreStatus", "OMStatus", "OMSignal", "OMRTLComponent", "OMCompoundType")
) extends OMCoreStatus

case class OMWFI(
  description: Option[String],
  name: String,
  hartId: Int,
  uid: Int,
  _types: Seq[String] = Seq("OMWFI", "OMCoreStatus", "OMStatus", "OMSignal", "OMRTLComponent", "OMCompoundType")
) extends OMCoreStatus

case class OMTrace(
  description: Option[String],
  name: String,
  hartId: Int,
  uid: Int,
  _types: Seq[String] = Seq("OMTrace", "OMCoreStatus", "OMStatus", "OMSignal", "OMRTLComponent", "OMCompoundType")
) extends OMCoreStatus

// Reset Vector
case class OMResetVector(
  description: Option[String],
  name: String,
  width: Int,
  uid: Int,
  _types: Seq[String] = Seq("OMResetVector", "OMSignal", "OMRTLComponent", "OMCompoundType")
) extends OMSignal

// Interrupts
case class OMInterruptSignal(
  description: Option[String],
  name: String,
  width: Int,
  uid: Int,
  _types: Seq[String] = Seq("OMInterruptSignal", "OMSignal", "OMRTLComponent", "OMCompoundType")
) extends OMSignal

trait OMRTLInterface extends OMRTLComponent {
  def clocks: Seq[OMClock]
  def clockRelationships: Seq[OMClockRelationship]
  def resets: Seq[OMRTLReset]
  def statuses: Seq[OMStatus]
  def _types: Seq[String] = Seq("OMRTLInterface", "OMRTLComponent", "OMCompoundType")
}

trait OMRTLModule {
  def moduleName: String
  def instanceName: Option[String]
  def hierarchicalId: Option[String] // Full dotted path from the root, where the root is described as a module name while all other path components are instance names
  def interface: OMRTLInterface
  def _types: Seq[String] = Seq("OMRTLModule", "OMRTLComponent", "OMCompoundType")
}
