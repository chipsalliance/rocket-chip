// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

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
  activeEdge: Option[OMSignalAssertionLevel],
  clock: String, // This will always be the name of the clock signal on the to p-level module
  synchronicity: Option[Synchronicity]
)

case class OMResetVector(
  width: Int
)

case class OMRTLInterface(
  clocks: List[OMClock],
  clockRelationships: List[OMClockRelationship],
  resets: List[OMRTLReset]
) extends RTLComponent

case class  OMRTLModule(
  moduleName: String,
  instanceName: Option[String],  // TODO: This does not exist for the top-level module because the top-level module is the only one that is not instantiated
  hierarchicalId: Option[String],  // Full dotted path from the root, where the root is described as a module name while all other path components are instance names
  interface: OMRTLInterface
)