// See LICENSE.SiFive for license details.

package diplomacy

import Chisel._
import config._
import scala.collection.immutable.{ListMap,SortedMap}

sealed trait ResourceValue
case class ResourcePermissions(r: Boolean, w: Boolean, x: Boolean, c: Boolean) // Not part of DTS
final case class ResourceAddress(address: Seq[AddressSet], permissions: ResourcePermissions) extends ResourceValue
final case class ResourceMapping(address: Seq[AddressSet], offset: BigInt, permissions: ResourcePermissions) extends ResourceValue
final case class ResourceInt(value: BigInt) extends ResourceValue
final case class ResourceString(value: String) extends ResourceValue
final case class ResourceReference(value: String) extends ResourceValue
final case class ResourceMap(value: Map[String, Seq[ResourceValue]], labels: Seq[String] = Nil) extends ResourceValue

/* If device is None, the value is global */
case class Binding(device: Option[Device], value: ResourceValue)
case class ResourceBindings(map: Map[String, Seq[Binding]])
{
  def apply(key: String): Seq[Binding] = map.getOrElse(key, Nil)
}

case class Description(name: String, mapping: Map[String, Seq[ResourceValue]])

abstract class Device
{
  def describe(resources: ResourceBindings): Description

  val label = "L" + Device.index.toString
  Device.index = Device.index + 1
}

object Device
{
  private var index: Int = 0
}

trait DeviceInterrupts
{
  this: Device =>
  val alwaysExtended = false
  def describeInterrupts(resources: ResourceBindings): Map[String, Seq[ResourceValue]] = {
    val int = resources("int")

    int.foreach { b => require (b.device.isDefined, "Device ${devname} property 'int' is missing user device") }
    val parents = int.map(_.device.get).distinct
    val simple = parents.size == 1 && !alwaysExtended

    val parent =
      if (!simple) None else
      Some("interrupt-parent" -> Seq(ResourceReference(parents.head.label)))
    val interrupts =
      if (!simple) None else
      Some("interrupts" -> int.map(_.value))

    val interrupts_extended =
      if (simple || parents.isEmpty) None else
      Some("interrupts-extended" -> int.flatMap(b => Seq(ResourceReference(b.device.get.label), b.value)))

    ListMap() ++ parent ++ interrupts ++ interrupts_extended
  }

  def int = Seq(Resource(this, "int"))
}

trait DeviceRegName
{
  this: Device =>
  val prefix = "soc/" // nearly everything on-chip belongs here
  def describeName(devname: String, resources: ResourceBindings): String = {
    val reg = resources.map.filterKeys(regFilter)
    if (reg.isEmpty) {
      devname
    } else {
      val (named, bulk) = reg.partition { case (k, v) => regName(k).isDefined }
      val mainreg = reg.find(x => regName(x._1) == "control").getOrElse(reg.head)._2
      require (!mainreg.isEmpty, s"reg binding for $devname is empty!")
      mainreg.head.value match {
        case x: ResourceAddress => s"${prefix}${devname}@${x.address.head.base.toString(16)}"
        case _ => require(false, "Device has the wrong type of 'reg' property (${reg.head})"); ""
      }
    }
  }

  def reg(name: String): Seq[Resource] = Seq(Resource(this, "reg/" + name))
  def reg: Seq[Resource] = Seq(Resource(this, "reg"))

  def regFilter(name: String): Boolean = name == "reg" || name.take(4) == "reg/"
  def regName(name: String): Option[String] = {
    val keys = name.split("/")
    require (keys.size >= 1 && keys.size <= 2 && keys(0) == "reg", s"Invalid reg name '${name}'")
    if (keys.size == 1) None else Some(keys(1))
  }
}

class SimpleDevice(devname: String, devcompat: Seq[String]) extends Device with DeviceInterrupts with DeviceRegName
{
  def describe(resources: ResourceBindings): Description = {
    val name = describeName(devname, resources)
    val int = describeInterrupts(resources)

    def optDef(x: String, seq: Seq[ResourceValue]) = if (seq.isEmpty) None else Some(x -> seq)
    val compat = optDef("compatible", devcompat.map(ResourceString(_)))

    val reg = resources.map.filterKeys(regFilter)
    val (named, bulk) = reg.partition { case (k, v) => regName(k).isDefined }
    // We need to be sure that each named reg has exactly one AddressRange associated to it
    named.foreach {
      case (k, Seq(Binding(_, value: ResourceAddress))) =>
        val ranges = AddressRange.fromSets(value.address)
        require (ranges.size == 1, s"DTS device $name has $k = $ranges, must be a single range!")
      case (k, seq) =>
        require (false, s"DTS device $name has $k = $seq, must be a single ResourceAddress!")
    }

    val names = optDef("reg-names", named.map(x => ResourceString(regName(x._1).get)).toList)
    val regs = optDef("reg", (named ++ bulk).flatMap(_._2.map(_.value)).toList)

    Description(name, ListMap() ++ compat ++ int ++ names ++ regs)
  }
}

class SimpleBus(devname: String, devcompat: Seq[String], offset: BigInt = 0) extends SimpleDevice(devname, devcompat ++ Seq("simple-bus"))
{
  override def describe(resources: ResourceBindings): Description = {
    val ranges = resources("ranges").map {
      case Binding(_, a: ResourceAddress) => ResourceMapping(a.address, offset, a.permissions)
    }
    require (!ranges.isEmpty, s"SimpleBus $devname must set ranges")

    val map = AddressRange.fromSets(ranges.flatMap(_.address))
    val minBase = map.map(_.base).min
    val maxBase = map.map(_.end).max
    val maxSize = map.map(_.size).max

    def ofInt(x: Int) = Seq(ResourceInt(BigInt(x)))
    val extra = Map(
      "#address-cells"   -> ofInt((log2Ceil(maxBase) + 31) / 32),
      "#size-cells"      -> ofInt((log2Ceil(maxSize) + 31) / 32),
      "ranges"           -> ranges)

    val Description(_, mapping) = super.describe(resources)
    Description(s"${prefix}${devname}@${minBase.toString(16)}", mapping ++ extra)
  }

  def ranges = Seq(Resource(this, "ranges"))
}

class MemoryDevice extends Device with DeviceRegName
{
  override val prefix = ""
  def describe(resources: ResourceBindings): Description = {
    Description(describeName("memory", resources), ListMap(
      "reg"         -> resources("reg").map(_.value),
      "device_type" -> Seq(ResourceString("memory"))))
  }
}

case class Resource(owner: Device, key: String)
{
  def bind(user: Device, value: ResourceValue) {
    val scope = BindingScope.active.get
    scope.resourceBindings = (this, Some(user), value) +: scope.resourceBindings
  }
  def bind(value: ResourceValue) {
    val scope = BindingScope.active.get
    scope.resourceBindings = (this, None, value) +: scope.resourceBindings
  }
}

trait BindingScope
{
  this: LazyModule =>

  private val parentScope = BindingScope.find(parent)
  protected[diplomacy] var resourceBindingFns: Seq[() => Unit] = Nil
  protected[diplomacy] var resourceBindings: Seq[(Resource, Option[Device], ResourceValue)] = Nil

  private case class ExpandedValue(path: Seq[String], labels: Seq[String], value: Seq[ResourceValue])
  private lazy val eval: Unit = {
    require (LazyModule.stack.isEmpty, "May not evaluate binding while still constructing LazyModules")
    parentScope.foreach { _.eval }
    resourceBindings = parentScope.map(_.resourceBindings).getOrElse(Nil)
    BindingScope.active = Some(this)
    resourceBindingFns.reverse.foreach { _() }
    BindingScope.active = None
    resourceBindingFns = Nil
  }

  private def makeTree(list: Seq[ExpandedValue]): Seq[ResourceValue] = {
    val (values_p, keys_p) = list.partition(_.path.isEmpty)
    val values = values_p.flatMap(_.value)
    val labels = values_p.flatMap(_.labels)
    val keys = keys_p.groupBy(_.path.head).toList.map { case (key, seq) =>
      (key -> makeTree(seq.map { x => x.copy(path = x.path.tail) }))
    }
    if (keys.isEmpty) values else ResourceMap(SortedMap(keys:_*), labels) +: values
  }

  private def expand(path: Seq[String], values: Seq[ResourceValue]): Seq[ExpandedValue] = {
    ExpandedValue(path, Nil, Nil) +:
    values.flatMap {
      case ResourceMap(map, labels) =>
        ExpandedValue(path, labels, Nil) +:
        map.toList.flatMap { case (key, values) => expand(path :+ key, values) }
      case z => Seq(ExpandedValue(path, Nil, Seq(z)))
    }
  }

  def bindingTree: ResourceMap = {
    eval
    val map: Map[Device, ResourceBindings] =
      resourceBindings.reverse.groupBy(_._1.owner).mapValues(seq => ResourceBindings(
        seq.groupBy(_._1.key).mapValues(_.map(z => Binding(z._2, z._3)))))
    val tree = makeTree(map.toList.flatMap { case (d, m) =>
      val Description(name, mapping) = d.describe(m)
      val tokens = name.split("/").toList
      expand(tokens, Seq(ResourceMap(mapping, Seq(d.label)))) })
    ResourceMap(SortedMap("/" -> tree))
  }
}

object BindingScope
{
  protected[diplomacy] var active: Option[BindingScope] = None
  protected[diplomacy] def find(m: Option[LazyModule] = LazyModule.stack.headOption): Option[BindingScope] = m.flatMap {
    case s: BindingScope => Some(s)
    case x => find(x.parent)
  }
}

object ResourceBinding
{
  def apply(block: => Unit) {
    val scope = BindingScope.find()
    require (scope.isDefined, "ResourceBinding must be called from within a BindingScope")
    scope.get.resourceBindingFns = { () => block } +: scope.get.resourceBindingFns
  }
}
