// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

trait RTLComponent extends OMCompoundType

trait OMSignal extends RTLComponent {
  def name: String // This will always be the name of the signal on the top-level module
  def description: Option[String]
  def uid: Int
}

case class OMClock(
  name: String,
  description: Option[String],
  uid: Int
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
  activeEdge: Option[OMSignalAssertionLevel],
  clock: String, // This will always be the name of the clock signal on the to p-level module
  synchronicity: Option[Synchronicity]
)

case class Reset(
  description: Option[String],
  name: String,
  activeEdge: Option[OMSignalAssertionLevel],
  clockName: String, // This will always be the name of the clock signal on the top-level module
  synchronicity: Option[Synchronicity],
  uid: Int
) extends OMSignal

// Status
trait Status extends OMSignal {
  def description: Option[String]
  def name: String
  uid: Int
}

trait OMCoreStatus extends Status {
  def description: Option[String]
  def name: String
  def hartId: Int
}

case class OMHalt(
  description: Option[String],
  name: String,
  hartId: Int,
  uid: Int
) extends OMCoreStatus

case class OMWFI(
  description: Option[String],
  name: String,
  hartId: Int,
  uid: Int
) extends OMCoreStatus

// Reset Vector
case class OMResetVector(
  description: Option[String],
  name: String,
  width: Int,
  uid: Int
) extends OMSignal

// Interrupts
case class OMInterruptSignal(
  description: Option[String],
  name: String,
  width: Int,
  uid: Int
) extends OMSignal

trait OMRTLInterface extends RTLComponent {
  def clocks: List[OMClock]
  def clockRelationships: List[OMClockRelationship]
  def resets: List[OMRTLReset]
  def statuses: List[Status]
}

trait OMRTLModule {
  def moduleName: String
  def instanceName: Option[String]
  def hierarchicalId: Option[String] // Full dotted path from the root, where the root is described as a module name while all other path components are instance names
  def interface: OMRTLInterface
}
