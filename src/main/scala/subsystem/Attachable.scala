// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import scala.language.dynamics
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyScope}
import freechips.rocketchip.diplomaticobjectmodel.HasLogicalTreeNode
import freechips.rocketchip.prci.ClockGroupEphemeralNode
import freechips.rocketchip.tilelink.TLBusWrapper
import freechips.rocketchip.util.{Location, LocationMap}

/** These traits are intended to make it possible to configure to which
  * buses optional devices are attached, even after a subsystem has been instantiated.
  * Consider them experimental for now.
  */

trait LazyScopeWithParameters extends LazyScope { this: LazyModule =>
  implicit val p: Parameters
}

trait HasLogicalHierarchy extends LazyScopeWithParameters with HasLogicalTreeNode { this: LazyModule => }

trait HasPRCILocations extends HasLogicalHierarchy { this: LazyModule =>
  implicit val asyncClockGroupsNode: ClockGroupEphemeralNode
  val ibus: InterruptBusWrapper
}

// TODO make this trait extend Dynamic itself and add the locations to anyLocationMap?
trait HasLocations extends HasPRCILocations { this: LazyModule =>
  val anyLocationMap = new LocationMap[Any]
  def selectDynamic(name: String): Any = anyLocationMap.selectDynamic(name)
  def updateDynamic(name: String)(value: Any): Unit = anyLocationMap.updateDynamic(name)(value)

  val tlBusWrapperLocationMap = new LocationMap[TLBusWrapper]
  def locateTLBusWrapper(location: TLBusWrapperLocation): TLBusWrapper = locateTLBusWrapper(location.name)
  def locateTLBusWrapper(name: String): TLBusWrapper = tlBusWrapperLocationMap.selectDynamic(name)
}

trait CanInstantiateWithinContext {
  def instantiate(context: HasLocations)(implicit p: Parameters): Unit
}

trait CanConnectWithinContext {
  def connect(context: HasLocations)(implicit p: Parameters): Unit
}

/** Attachable things provide a standard interface by which other things may attach themselves to this target.
  * Right now the trait is mostly for backwards compatibility, and in case it eventually becomes valuable
  * to be able to define additional resources available to agents trying to attach themselves, other than
  * what is being made available via LocationMaps in trait HasLocations.
  */
trait Attachable extends HasLocations { this: LazyModule =>
  def locateTLBusWrapper(location: TLBusWrapperLocation): TLBusWrapper
}
