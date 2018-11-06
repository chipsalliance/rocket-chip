// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

trait RTLComponent extends OMCompoundType

trait OMSignal extends RTLComponent {
  def name: String // This will always be the name of the signal on the top-level module
  def description: String
}

trait OMClock extends OMSignal

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

case class RTLReset(
  activeEdge: OMSignalAssertionLevel,
  clock: String, // This will always be the name of the clock signal on the top-level module
  synchronicity: Synchronicity
)

case class OMResetVector(
  width: Int
)

case class OMRTLInterface(
  clocks: List[OMClock],
  clockRelationships: List[OMClockRelationship],
  resets: List[RTLReset]
) extends RTLComponent

case class OMRTLModule(
  moduleName: String,
  instanceName: String,
  hierarchicalId: String,  // Full dotted path from the root, where the root is described as a module name while all other path components are instance names
  interface: OMRTLInterface
)