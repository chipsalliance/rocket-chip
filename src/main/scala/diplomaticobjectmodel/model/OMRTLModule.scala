// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

import firrtl.annotations.Named

trait RTLComponent extends OMCompoundType

trait OMSignal extends RTLComponent {
  def name: Either[Named, String] // This will always be the name of the signal on the top-level module
  def description: Option[String]
}

case class OMClock(
  name: Either[Named, String],
  description: Option[String] = None
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
  name: Either[Named, String],
  description: Option[String] = None,
  activeEdge: Option[OMSignalAssertionLevel] = None,
  clock: String, // This will always be the name of the clock signal on the to p-level module
  synchronicity: Option[Synchronicity] = None
) extends OMSignal

case class OMResetVector(
  width: Int
) extends OMBaseType

case class OMRTLInterface(
  clocks: Seq[OMClock],
  clockRelationships: Seq[OMClockRelationship],
  resets: Seq[OMRTLReset]
) extends RTLComponent

case class  OMRTLModule(
  moduleName: String,
  instanceName: Option[String] = None,  // TODO: This does not exist for the top-level module because the top-level module is the only one that is not instantiated
  hierarchicalId: Option[String] = None,  // Full dotted path from the root, where the root is described as a module name while all other path components are instance names
  interface: OMRTLInterface
)