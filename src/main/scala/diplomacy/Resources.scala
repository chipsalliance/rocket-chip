// See LICENSE.SiFive for license details.

package diplomacy

import Chisel._
import config._
import scala.collection.immutable.{ListMap,SortedMap}

sealed trait ResourceValue
final case class ResourceAddress(address: Seq[AddressSet], r: Boolean, w: Boolean, x: Boolean) extends ResourceValue
final case class ResourceMapping(address: Seq[AddressSet], offset: BigInt) extends ResourceValue
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
    val reg = resources("reg")
    require (!reg.isEmpty, "Device is missing the 'reg' property")
    reg.head.value match {
      case x: ResourceAddress => s"${prefix}${devname}@${x.address.head.base.toString(16)}"
      case _ => require(false, "Device has the wrong type of 'reg' property (${reg.head})"); ""
    }
  }

  def reg = Seq(Resource(this, "reg"))
}

class SimpleDevice(devname: String, devcompat: Seq[String]) extends Device with DeviceInterrupts with DeviceRegName
{
  def describe(resources: ResourceBindings): Description = {
    val name = describeName(devname, resources)
    val int = describeInterrupts(resources)
    val compat =
      if (devcompat.isEmpty) None else
      Some("compatible" -> devcompat.map(ResourceString(_)))

    Description(name,
      ListMap("reg" -> resources("reg").map(_.value))
      ++ compat ++ int)
  }
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
