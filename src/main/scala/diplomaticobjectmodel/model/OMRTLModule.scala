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
  resets: List[OMRTLReset],
  _types: Seq[String] = Seq("OMRTLInterface")
) extends RTLComponent


/*********************************************************
  * Describe an RTL module
  * RTL modules may be transformed, renamed or even removed by subsequent optimizations.
  * Thus, "moduleName" is only a suggested name which may not correspond to any of the final module names.
  * For convenience, there are default values for all fields. It is reasonable to use "OMRTLModule()"
  * as a placeholder when an rtl module is required but the information isn't known.
  */
case class  OMRTLModule(
  moduleName: String = "",
  instanceName: Option[String] = None,  // TODO: This does not exist for the top-level module because the top-level module is the only one that is not instantiated
  hierarchicalId: Option[String] = None,  // Full dotted path from the root, where the root is described as a module name while all other path components are instance names
  interface: OMRTLInterface = OMRTLInterface(List(), List(), List()),
  _types: Seq[String] = Seq("OMRTLModule", "OMRTLComponent")
)
