// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyScope}
import freechips.rocketchip.prci.ClockGroupEphemeralNode
import freechips.rocketchip.tilelink.TLBusWrapper
import freechips.rocketchip.util.{Location, LocationMap}

/** These traits are intended to make it possible to configure to which
  * buses optional devices are attached, even after a subsystem has been instantiated.
  * Consider them experimental for now.
  */

/** More nodes can be added to subclasses of this after they have been instantiated,
  * and the implicit context-dependent Parameters object is available.
  * These qualities comprise the fundamental capabilities of dynamic configurability.
  */
trait LazyScopeWithParameters extends LazyScope { this: LazyModule =>
  implicit val p: Parameters
}

/** Layers of hierarchy with this trait contain attachment points for neworks of power, clock, reset, and interrupt resources */
trait HasPRCILocations extends LazyScopeWithParameters { this: LazyModule =>
  implicit val asyncClockGroupsNode: ClockGroupEphemeralNode
  val ibus: InterruptBusWrapper
  val anyLocationMap = LocationMap.empty[Any]
}

/** Layers of hierarchy with this trait contain attachment points for TileLink interfaces */
trait HasTileLinkLocations extends HasPRCILocations { this: LazyModule =>
  val tlBusWrapperLocationMap = LocationMap.empty[TLBusWrapper]
  def locateTLBusWrapper(location: Location[TLBusWrapper]): TLBusWrapper = locateTLBusWrapper(location.name)
  def locateTLBusWrapper(name: String): TLBusWrapper = tlBusWrapperLocationMap(Location[TLBusWrapper](name))
}

/** Subclasses of this trait have the ability to instantiate things inside a context that has TL attachement locations */
trait CanInstantiateWithinContextThatHasTileLinkLocations {
  def instantiate(context: HasTileLinkLocations)(implicit p: Parameters): Unit
}

/** Subclasses of this trait have the ability to connect things inside a context that has TL attachement locations */
trait CanConnectWithinContextThatHasTileLinkLocations {
  def connect(context: HasTileLinkLocations)(implicit p: Parameters): Unit
}

/** Attachable things provide a standard interface by which other things may attach themselves to this target.
  * Right now the trait is mostly for backwards compatibility, and in case it eventually becomes valuable
  * to be able to define additional resources available to agents trying to attach themselves, other than
  * what is being made available via the LocationMaps in trait HasTileLinkLocations.
  */
trait Attachable extends HasTileLinkLocations { this: LazyModule =>
  def locateTLBusWrapper(location: TLBusWrapperLocation): TLBusWrapper = locateTLBusWrapper(location.name)
}
