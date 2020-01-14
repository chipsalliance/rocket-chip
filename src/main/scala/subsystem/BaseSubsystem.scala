// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.HasLogicalTreeNode
import freechips.rocketchip.diplomaticobjectmodel.logicaltree._
import freechips.rocketchip.tilelink.TLBusWrapper
import freechips.rocketchip.prci._
import freechips.rocketchip.util._


case object SystemBusKey extends Field[SystemBusParams]
case object FrontBusKey extends Field[FrontBusParams]
case object PeripheryBusKey extends Field[PeripheryBusParams]
case object ControlBusKey extends Field[PeripheryBusParams]
case object MemoryBusKey extends Field[MemoryBusParams]
case object BankedL2Key extends Field(BankedL2Params())

case object BuildSystemBus extends Field[Parameters => SystemBus](p => new SystemBus(p(SystemBusKey))(p))

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

/** These three traits are intended to make it possible to configure to which
  * buses optional devices are attached, even after a subsystem has been instantiated.
  * Consider them experimental for now.
  */

trait HasBusAttachmentFunction {
  type BusAttachmentFunction = PartialFunction[BaseSubsystemBusAttachment, TLBusWrapper]
  def attach: BusAttachmentFunction
}

case object AsyncClockGroupsKey extends Field[Option[ClockGroupEphemeralNode]](None)

trait HasAsyncClockGroupsNode { this: LazyModule =>
  implicit val p: Parameters
  private def driveAllClockGroupsFromImplicitClock(): ClockGroupEphemeralNode = {
    val dummyClockGroupSourceNode: ClockGroupSourceNode = SimpleClockGroupSource()
    val tempNode: ClockGroupEphemeralNode = ClockGroupEphemeralNode()
    tempNode :*= dummyClockGroupSourceNode
    tempNode
  }

  implicit val asyncClockGroupsNode = p(AsyncClockGroupsKey).getOrElse(driveAllClockGroupsFromImplicitClock())
}

trait Attachable extends LazyScope
    with HasLogicalTreeNode
    with HasBusAttachmentFunction
    with HasAsyncClockGroupsNode { this: LazyModule =>
  val ibus: InterruptBusWrapper
}

/** This trait contains the cases matched in baseBusAttachmentFunc below.
  * Extend/override them to offer novel attachment locations in subclasses of BaseSubsystem.
  */
trait BaseSubsystemBusAttachment
case object SBUS extends BaseSubsystemBusAttachment
case object PBUS extends BaseSubsystemBusAttachment
case object FBUS extends BaseSubsystemBusAttachment
case object MBUS extends BaseSubsystemBusAttachment
case object CBUS extends BaseSubsystemBusAttachment

/** Base Subsystem class with no peripheral devices or ports added */
abstract class BaseSubsystem(implicit p: Parameters) extends BareSubsystem 
    with Attachable {


  override val module: BaseSubsystemModuleImp[BaseSubsystem]

  // These are wrappers around the standard buses available in all subsytems, where
  // peripherals, tiles, ports, and other masters and slaves can attach themselves.
  val ibus = new InterruptBusWrapper()
  val sbus = LazyModule(p(BuildSystemBus)(p))
  val pbus = LazyModule(new PeripheryBus(p(PeripheryBusKey), "subsystem_pbus"))
  val fbus = LazyModule(new FrontBus(p(FrontBusKey)))
  val mbus = LazyModule(new MemoryBus(p(MemoryBusKey)))
  val cbus = LazyModule(new PeripheryBus(p(ControlBusKey), "subsystem_cbus"))

  def attach: BusAttachmentFunction = {
    case SBUS => sbus
    case PBUS => pbus
    case FBUS => fbus
    case MBUS => mbus
    case CBUS => cbus
  }

  // Collect information for use in DTS
  lazy val topManagers = sbus.unifyManagers
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
