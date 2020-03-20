// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.HasLogicalTreeNode
import freechips.rocketchip.diplomaticobjectmodel.logicaltree._
import freechips.rocketchip.prci._
import freechips.rocketchip.util._

case object SubsystemDriveAsyncClockGroupsKey extends Field[Option[ClockGroupDriverParameters]](Some(ClockGroupDriverParameters(1)))
case object AsyncClockGroupsKey extends Field[ClockGroupEphemeralNode](ClockGroupEphemeralNode()(ValName("async_clock_groups")))
case class TLNetworkTopologyLocated(where: String) extends Field[Seq[CanInstantiateWithinContext with CanConnectWithinContext]]

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

abstract class BareSubsystemModuleImp[+L <: BareSubsystem](_outer: L) extends LazyModuleImp(_outer) {
  val outer = _outer
  ElaborationArtefacts.add("graphml", outer.graphML)
  ElaborationArtefacts.add("dts", outer.dts)
  ElaborationArtefacts.add("json", outer.json)
  ElaborationArtefacts.add("plusArgs", PlusArgArtefacts.serialize_cHeader)
  println(outer.dts)
}

trait SubsystemResetScheme
case object ResetSynchronous extends SubsystemResetScheme
case object ResetAsynchronous extends SubsystemResetScheme
case object ResetAsynchronousFull extends SubsystemResetScheme

case object SubsystemResetSchemeKey extends Field[SubsystemResetScheme](ResetSynchronous)

/** Base Subsystem class with no peripheral devices or ports added */
abstract class BaseSubsystem(implicit p: Parameters) extends BareSubsystem
    with Attachable {

  override val module: BaseSubsystemModuleImp[BaseSubsystem]

  // Concrete attachment points for PRCI-related signals.
  val ibus = new InterruptBusWrapper()
  implicit val asyncClockGroupsNode = p(AsyncClockGroupsKey)
  val async_clock_groups =
    p(SubsystemDriveAsyncClockGroupsKey)
      .map(_.drive(asyncClockGroupsNode))
      .getOrElse(InModuleBody { HeterogeneousBag[ClockGroupBundle](Nil) })

  // Find the topology configuration for the TL buses located in this subsystem.
  // Calling these functions populates tlBusWrapperLocationMap and connects the locations to each other.
  val location = HierarchicalLocation("InSubsystem")
  val topology = p(TLNetworkTopologyLocated(location.name))
  topology.foreach(_.instantiate(this))
  topology.foreach(_.connect(this))

  // TODO how should this clock driving happen; must there really always be an "sbus"?
  val sbus = tlBusWrapperLocationMap.select(SBUS)
  locateTLBusWrapper(SBUS).clockGroupNode := asyncClockGroupsNode

  // TODO deprecate these public members to see where users are manually hardcoding a particular bus that might actually not exist in a certain dynamic topology
  val pbus = tlBusWrapperLocationMap.lift(PBUS).getOrElse(sbus)
  val fbus = tlBusWrapperLocationMap.lift(FBUS).getOrElse(sbus)
  val mbus = tlBusWrapperLocationMap.lift(MBUS).getOrElse(sbus)
  val cbus = tlBusWrapperLocationMap.lift(CBUS).getOrElse(sbus)

  // Collect information for use in DTS
  lazy val topManagers = locateTLBusWrapper(SBUS).unifyManagers
  ResourceBinding {
    val managers = topManagers
    val max = managers.flatMap(_.address).map(_.max).max
    val width = ResourceInt((log2Ceil(max)+31) / 32)
    val model = p(DTSModel)
    val compat = p(DTSCompat)
    val devCompat = (model +: compat).map(s => ResourceString(s + "-dev"))
    val socCompat = (model +: compat).map(s => ResourceString(s + "-soc"))
    devCompat.foreach { Resource(ResourceAnchors.root, "compat").bind(_) }
    socCompat.foreach { Resource(ResourceAnchors.soc,  "compat").bind(_) }
    Resource(ResourceAnchors.root, "model").bind(ResourceString(model))
    Resource(ResourceAnchors.root, "width").bind(width)
    Resource(ResourceAnchors.soc,  "width").bind(width)
    Resource(ResourceAnchors.cpus, "width").bind(ResourceInt(1))

    managers.foreach { case manager =>
      val value = manager.toResource
      manager.resources.foreach { case resource =>
        resource.bind(value)
      }
    }
  }

  lazy val logicalTreeNode = new SubsystemLogicalTreeNode()

  private val buses = Seq(
    sbus,
    pbus,
    fbus,
    mbus,
    cbus
  )

  buses.foreach { bus =>
    val builtIn = bus.builtInDevices
    builtIn.errorOpt.foreach { error =>
      LogicalModuleTree.add(logicalTreeNode, error.logicalTreeNode)
    }
    builtIn.zeroOpt.foreach { zero =>
      LogicalModuleTree.add(logicalTreeNode, zero.logicalTreeNode)
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
