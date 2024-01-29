// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import chisel3.{Flipped, IO}
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import freechips.rocketchip.tilelink.TLBusWrapper
import freechips.rocketchip.util._

case object SubsystemDriveClockGroupsFromIO extends Field[Boolean](true)
case class TLNetworkTopologyLocated(where: HierarchicalLocation) extends Field[Seq[CanInstantiateWithinContextThatHasTileLinkLocations with CanConnectWithinContextThatHasTileLinkLocations]]
case class TLManagerViewpointLocated(where: HierarchicalLocation) extends Field[Location[TLBusWrapper]](SBUS)

class HierarchicalLocation(override val name: String) extends Location[LazyScope](name)
case object InTile extends HierarchicalLocation("InTile")
case object InSubsystem extends HierarchicalLocation("InSubsystem")
case object InSystem extends HierarchicalLocation("InSystem")

/** BareSubsystem is the root class for creating a subsystem */
abstract class BareSubsystem(implicit p: Parameters) extends LazyModule with BindingScope {
  lazy val dts = DTS(bindingTree)
  lazy val dtb = DTB(dts)
  lazy val json = JSON(bindingTree)
}

abstract class BareSubsystemModuleImp[+L <: BareSubsystem](_outer: L) extends LazyRawModuleImp(_outer) {
  val outer = _outer
  ElaborationArtefacts.add("graphml", outer.graphML)
  ElaborationArtefacts.add("dts", outer.dts)
  ElaborationArtefacts.add("json", outer.json)
  ElaborationArtefacts.add("plusArgs", PlusArgArtefacts.serialize_cHeader())
  println(outer.dts)
}

trait SubsystemResetScheme
case object ResetSynchronous extends SubsystemResetScheme
case object ResetAsynchronous extends SubsystemResetScheme
case object ResetAsynchronousFull extends SubsystemResetScheme

case object SubsystemResetSchemeKey extends Field[SubsystemResetScheme](ResetSynchronous)

/** Concrete attachment points for PRCI-related signals.
  * These aren't actually very configurable, yet.
  */
trait HasConfigurablePRCILocations { this: HasPRCILocations =>
  val ibus = LazyModule(new InterruptBusWrapper)
  val allClockGroupsNode = ClockGroupIdentityNode()
  val io_clocks = if (p(SubsystemDriveClockGroupsFromIO)) {
    val aggregator = ClockGroupAggregator()
    val source = ClockGroupSourceNode(Seq(ClockGroupSourceParameters()))
    allClockGroupsNode :*= aggregator := source
    Some(InModuleBody {
      val elements = source.out.map(_._1.member.elements).flatten
      val io = IO(Flipped(RecordMap(elements.map { case (name, data) =>
        name -> data.cloneType
      }:_*)))
      elements.foreach { case (name, data) => io(name).foreach { data := _ } }
      io
    })
  } else {
    None
  }
}

/** Look up the topology configuration for the TL buses located within this layer of the hierarchy */
trait HasConfigurableTLNetworkTopology { this: HasTileLinkLocations =>
  val location: HierarchicalLocation

  // Calling these functions populates tlBusWrapperLocationMap and connects the locations to each other.
  val topology = p(TLNetworkTopologyLocated(location))
  topology.map(_.instantiate(this))
  topology.foreach(_.connect(this))

  // This is used lazily at DTS binding time to get a view of the network
  lazy val topManagers = tlBusWrapperLocationMap(p(TLManagerViewpointLocated(location))).unifyManagers
}

/** Base Subsystem class with no peripheral devices, ports or cores added yet */
abstract class BaseSubsystem(val location: HierarchicalLocation = InSubsystem)
                            (implicit p: Parameters)
  extends BareSubsystem
  with Attachable
  with HasConfigurablePRCILocations
  with HasConfigurableTLNetworkTopology
{
  override val module: BaseSubsystemModuleImp[BaseSubsystem]

  val busContextName = "subsystem"

  // TODO must there really always be an "sbus"?
  val sbus = tlBusWrapperLocationMap(SBUS)
  tlBusWrapperLocationMap.lift(SBUS).map { _.clockGroupNode := allClockGroupsNode }

  // TODO: Preserve legacy implicit-clock behavior for IBUS for now. If binding
  // a PLIC to the CBUS, ensure it is synchronously coupled to the SBUS.
  ibus.clockNode := sbus.fixedClockNode

  // TODO deprecate these public members to see where users are manually hardcoding a particular bus that might actually not exist in a certain dynamic topology
  val pbus = tlBusWrapperLocationMap.lift(PBUS).getOrElse(sbus)
  val fbus = tlBusWrapperLocationMap.lift(FBUS).getOrElse(sbus)
  val mbus = tlBusWrapperLocationMap.lift(MBUS).getOrElse(sbus)
  val cbus = tlBusWrapperLocationMap.lift(CBUS).getOrElse(sbus)

  // Collect information for use in DTS
  ResourceBinding {
    val managers = topManagers
    val max = managers.flatMap(_.address).map(_.max).max
    val width = ResourceInt((log2Ceil(max)+31) / 32)
    val model = p(DTSModel)
    val compat = p(DTSCompat)
    var hertz = p(DTSTimebase) // add for timebase-frequency
    val devCompat = (model +: compat).map(s => ResourceString(s + "-dev"))
    val socCompat = (model +: compat).map(s => ResourceString(s + "-soc"))
    devCompat.foreach { Resource(ResourceAnchors.root, "compat").bind(_) }
    socCompat.foreach { Resource(ResourceAnchors.soc,  "compat").bind(_) }
    Resource(ResourceAnchors.root, "model").bind(ResourceString(model))
    Resource(ResourceAnchors.root, "width").bind(width)
    Resource(ResourceAnchors.soc,  "width").bind(width)
    Resource(ResourceAnchors.cpus, "width").bind(ResourceInt(1))
    Resource(ResourceAnchors.cpus, "hertz").bind(ResourceInt(hertz))

    managers.foreach { case manager =>
      val value = manager.toResource
      manager.resources.foreach { case resource =>
        resource.bind(value)
      }
    }
  }
}


abstract class BaseSubsystemModuleImp[+L <: BaseSubsystem](_outer: L) extends BareSubsystemModuleImp(_outer) {
  private val mapping: Seq[AddressMapEntry] = Annotated.addressMapping(this, {
    outer.collectResourceAddresses.groupBy(_._2).toList.flatMap { case (key, seq) =>
      AddressRange.fromSets(key.address).map { r => AddressMapEntry(r, key.permissions, seq.map(_._1)) }
    }.sortBy(_.range)
  })

  Annotated.addressMapping(this, mapping)

  println("Generated Address Map")
  mapping.map(entry => println(entry.toString((outer.sbus.busView.bundle.addressBits-1)/4 + 1)))
  println("")

  ElaborationArtefacts.add("memmap.json", s"""{"mapping":[${mapping.map(_.toJSON).mkString(",")}]}""")

  // Confirm that all of memory was described by DTS
  private val dtsRanges = AddressRange.unify(mapping.map(_.range))
  private val allRanges = AddressRange.unify(outer.topManagers.flatMap { m => AddressRange.fromSets(m.address) })

  if (dtsRanges != allRanges) {
    println("Address map described by DTS differs from physical implementation:")
    AddressRange.subtract(allRanges, dtsRanges).foreach { case r =>
      println(s"\texists, but undescribed by DTS: ${r}")
    }
    AddressRange.subtract(dtsRanges, allRanges).foreach { case r =>
      println(s"\tdoes not exist, but described by DTS: ${r}")
    }
    println("")
  }
}
