// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel.log2Ceil
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.model._

import scala.collection.immutable.{ListMap, SortedMap}
import scala.collection.mutable.HashMap

sealed trait ResourceValue

/** Permission of an address space.
  * @param r            readable.
  * @param w            writable.
  * @param x            executable.
  * @param c            cacheable.
  * @param a            supports all atomic operations.
  */
case class ResourcePermissions(r: Boolean, w: Boolean, x: Boolean, c: Boolean, a: Boolean) // Not part of DTS

/** An address space description.
  * @param address      the address space.
  * @param permissions  the permission attributes of this space. See [[freechips.rocketchip.diplomacy.ResourcePermissions]].
  */
final case class ResourceAddress(address: Seq[AddressSet], permissions: ResourcePermissions) extends ResourceValue
{
  /* For things like SPI which uses simple integer addressing */
  def this(x: Int) = this(Seq(AddressSet(x, 0)), ResourcePermissions(false, false, false, false, false))
}
object ResourceAddress {
  def apply(x: Int) = new ResourceAddress(x)
}

/** A mapped address space (eg: when map a device to a bus).
  * @param address      the address space.
  * @param offset       the address offset of the mapped device (eg: base address of the bus).
  * @param permissions  the permission attributes of this space. See [[freechips.rocketchip.diplomacy.ResourcePermissions]].
  */
final case class ResourceMapping(address: Seq[AddressSet], offset: BigInt, permissions: ResourcePermissions) extends ResourceValue
final case class ResourceString(value: String) extends ResourceValue
final case class ResourceInt(value: BigInt) extends ResourceValue {
  def this(x: Double) = this(BigDecimal(x).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt)
}
object ResourceInt {
  def apply(x: Double) = new ResourceInt(x)
}

/** A reference pointing to another device in DTS (eg: interrupt to interrupt controller).
  * @param value        the label (String) of the device.
  */
final case class ResourceReference(value: String) extends ResourceValue
final case class ResourceAlias(value: String) extends ResourceValue
final case class ResourceMap(value: Map[String, Seq[ResourceValue]], labels: Seq[String] = Nil) extends ResourceValue

/* If device is None, the value is global */
case class Binding(device: Option[Device], value: ResourceValue)
case class ResourceBindings(map: Map[String, Seq[Binding]] = Map.empty)
{
  def apply(key: String): Seq[Binding] = map.getOrElse(key, Nil)
}

/** A serializable description of a device.
  * @param name         the resolved name of this device. See [[freechips.rocketchip.diplomacy.DeviceRegName]].
  * @param mapping      the property map of this device.
  */
case class Description(name: String, mapping: Map[String, Seq[ResourceValue]])

case class ResourceBindingsMap(map: Map[Device, ResourceBindings])

abstract class Device
{
  def describe(resources: ResourceBindings): Description
  /* This can be overriden to make one device relative to another */

  def parent: Option[Device] = None

  /** make sure all derived devices have an unique label */
  val label = "L" + Device.index.toString
  Device.index = Device.index + 1
}

abstract class DeviceSnippet extends Device
{
  final def describe(resources: ResourceBindings) = describe()
  def describe(): Description

  ResourceBinding { Resource(this, "exists").bind(ResourceString("yes")) }
}

object Device
{
  private var index: Int = 0
  def skipIndexes(x: Int) { index += x }
}

/** A trait for devices that generate interrupts. */
trait DeviceInterrupts
{
  this: Device =>

  /** Whether to always use the expanded interrupt description in DTS: "interrupts-extended" */
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

/** A trait for devices that have an exposed clock. */
trait DeviceClocks
{
  this: Device =>

  /** The number of clocks this device must have; default to 0-1 */
  val requiredClocks = 0 to 1
  def describeClocks(resources: ResourceBindings): Map[String, Seq[ResourceValue]] = {
    val clocks = resources("clocks").map(_.value)
    require (requiredClocks.contains(clocks.size))
    if (!clocks.isEmpty) Map("clocks" -> clocks) else Map()
  }
}

/** A trait for resolving the name of a device. */
trait DeviceRegName
{
  this: Device =>
  def describeName(devname: String, resources: ResourceBindings): String = {
    val reg = resources.map.filterKeys(DiplomacyUtils.regFilter)
    if (reg.isEmpty) {
      devname
    } else {
      val (named, bulk) = reg.partition { case (k, v) => DiplomacyUtils.regName(k).isDefined }
      val mainreg = reg.find(x => DiplomacyUtils.regName(x._1) == "control").getOrElse(reg.head)._2
      require (!mainreg.isEmpty, s"reg binding for $devname is empty!")
      mainreg.head.value match {
        case x: ResourceAddress => s"${devname}@${x.address.head.base.toString(16)}"
        case _ => require(false, s"Device has the wrong type of 'reg' property (${reg.head})"); ""
      }
    }
  }

  def reg(name: String): Seq[Resource] = Seq(Resource(this, "reg/" + name))
  def reg: Seq[Resource] = Seq(Resource(this, "reg"))

}

object DiplomacyUtils {
  def regFilter(name: String): Boolean = name == "reg" || name.take(4) == "reg/"
  def rangeFilter(name: String): Boolean = name == "ranges"
  def regName(name: String): Option[String] = {
    val keys = name.split("/")
    require (keys.size >= 1 && keys.size <= 2 && keys(0) == "reg", s"Invalid reg name '${name}'")
    if (keys.size == 1) None else Some(keys(1))
  }
}

/** A simple device descriptor for devices that may support interrupts and address spaces.
  * @param devname      the base device named used in device name generation.
  * @param devcompat    a list of compatible devices. See device tree property "compatible".
  */
class SimpleDevice(val devname: String, devcompat: Seq[String]) extends Device
  with DeviceInterrupts
  with DeviceClocks
  with DeviceRegName
{
  override def parent = Some(ResourceAnchors.soc) // nearly everything on-chip belongs here

  var deviceNamePlusAddress: String = ""

  def describe(resources: ResourceBindings): Description = {
    val name = describeName(devname, resources)  // the generated device name in device tree
    val int = describeInterrupts(resources)      // interrupt description
    val clocks = describeClocks(resources)

    def optDef(x: String, seq: Seq[ResourceValue]) = if (seq.isEmpty) None else Some(x -> seq)
    val compat = optDef("compatible", devcompat.map(ResourceString(_))) // describe the list of compatiable devices

    val reg = resources.map.filterKeys(DiplomacyUtils.regFilter)
    val (named, bulk) = reg.partition { case (k, v) => DiplomacyUtils.regName(k).isDefined }
    // We need to be sure that each named reg has exactly one AddressRange associated to it
    named.foreach {
      case (k, Seq(Binding(_, value: ResourceAddress))) =>
        val ranges = AddressRange.fromSets(value.address)
        require (ranges.size == 1, s"DTS device $name has $k = $ranges, must be a single range!")
      case (k, seq) =>
        require (false, s"DTS device $name has $k = $seq, must be a single ResourceAddress!")
    }

    val names = optDef("reg-names", named.map(x => ResourceString(DiplomacyUtils.regName(x._1).get)).toList) // names of the named address space
    val regs = optDef("reg", (named ++ bulk).flatMap(_._2.map(_.value)).toList) // address ranges of all spaces (named and bulk)

    deviceNamePlusAddress = name

    Description(name, ListMap() ++ compat ++ int ++ clocks ++ names ++ regs)
  }
}

/** A simple bus
  * @param devname      the base device named used in device name generation.
  * @param devcompat    a list of compatible devices. See device tree property "compatible".
  * @param offset       the base address of this bus.
  */
class SimpleBus(devname: String, devcompat: Seq[String], offset: BigInt = 0) extends SimpleDevice(devname, devcompat ++ Seq("simple-bus"))
{
  override def describe(resources: ResourceBindings): Description = {
    val ranges = resources("ranges").collect {
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

    deviceNamePlusAddress = devname

    val Description(_, mapping) = super.describe(resources)
    Description(s"${devname}@${minBase.toString(16)}", mapping ++ extra)
  }

  def ranges = Seq(Resource(this, "ranges"))
}
/** A generic memory block. */
class MemoryDevice extends Device with DeviceRegName
{
  def describe(resources: ResourceBindings): Description = {
    Description(describeName("memory", resources), ListMap(
      "reg"         -> resources.map.filterKeys(DiplomacyUtils.regFilter).flatMap(_._2).map(_.value).toList,
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

/** The resource binding scope for a LazyModule that generates a device tree (currently Subsystem only). */
trait BindingScope
{
  this: LazyModule =>

  BindingScope.add(this)

  private val parentScope = BindingScope.find(parent)
  protected[diplomacy] var resourceBindingFns: Seq[() => Unit] = Nil // callback functions to resolve resource binding during elaboration
  protected[diplomacy] var resourceBindings: Seq[(Resource, Option[Device], ResourceValue)] = Nil

  private case class ExpandedValue(path: Seq[String], labels: Seq[String], value: Seq[ResourceValue])
  private lazy val eval: Unit = {
    require (!LazyModule.scope.isDefined, "May not evaluate binding while still constructing LazyModules")
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
    if (labels.isEmpty && keys.isEmpty) values else ResourceMap(SortedMap(keys:_*), labels) +: values
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

  private def collect(skipRoot: Int, path: List[String], offset: BigInt, map: ResourceMap): List[(String, ResourceAddress)] = {
    // Translate local addresses to global addresses
    val name = path.headOption.getOrElse("/")
    def shift(x: Seq[AddressSet]) = x.map(a => a.copy(base = a.base + offset))
    val addresses: List[(String, ResourceAddress)] = map.value.toList.flatMap { case (_, seq) => seq.collect {
      case y: ResourceAddress => (name -> y.copy(address = shift(y.address)))
    } }
    // Recursively handle children only if they use addresses
    val haveChildAddresses = map.value.values.exists(_.exists {
      case x: ResourceMap => x.value.contains("reg") || x.value.contains("ranges")
      case _ => false
    })
    def mapChildren(offset: BigInt) = map.value.toList.flatMap { case (key, seq) => seq.collect {
      case map: ResourceMap => collect(skipRoot, key :: path, offset, map)
    }.flatten }
    // How do we handle this node?
    val childAddresses = map.value.lift("ranges") match {
      // root typically is missing ranges; probe children anyway
      case None if path.size < skipRoot => mapChildren(offset)
      // no ranges -> don't probe children
      case None => Nil
      // ranges; -> probe children at same offset
      case Some(Nil) => mapChildren(offset)
      // ranges x; + no children -> report ranges as addressable regions
      case Some(seq) if !haveChildAddresses => seq.collect {
        case ResourceMapping(addr, _, perm) => (name -> ResourceAddress(shift(addr), perm))
      }
      // children + single ranges -> probe children at displaced offset
      case Some(Seq(ResourceMapping(addr, delta, perm))) => mapChildren(offset+delta)
      // multiple ranges + children -> don't know how to handle this
      case x => { require(false, s"Unexpected value in ranges key: ${x}"); Nil }
    }
    addresses ++ childAddresses
  }

  /** Generate the device tree. */
  def bindingTree: ResourceMap = {
    eval
    val map: Map[Device, ResourceBindings] = getResourceBindingsMap.map
    val descs: HashMap[Device, Description] = HashMap.empty
    def getDesc(dev: Device): Description = {
      if (descs.contains(dev)) {
        descs(dev)
      } else {
        val bindings = map.lift(dev).getOrElse(ResourceBindings())
        val Description(name, mapping) = dev.describe(bindings)
        val fullName = dev.parent match {
          case None => name
          case Some(parent) => getDesc(parent).name + "/" + name
        }
        val desc = Description(fullName, mapping)
        descs += ((dev, desc))
        desc
      }
    }
    map.keys.foreach(getDesc)
    val tree = makeTree(descs.toList.flatMap { case (d, Description(name, mapping)) =>
      val tokens = name.split("/").toList
      expand(tokens, Seq(ResourceMap(mapping, Seq(d.label)))) })
    ResourceMap(SortedMap("/" -> tree))
  }

  /** Generate the ResourceBindingsMap which stores each device's ResourceBindings
    *
    * @return
    */
  def getResourceBindingsMap: ResourceBindingsMap = {
    eval
    ResourceBindingsMap(map = resourceBindings.reverse.groupBy(_._1.owner).mapValues(seq => ResourceBindings(
        seq.groupBy(_._1.key).mapValues(_.map(z => Binding(z._2, z._3)).distinct))))
  }

  /** Collect resource addresses from tree. */
  def collectResourceAddresses = collect(2, Nil, 0, bindingTree)
}

object BindingScope
{
  protected[diplomacy] var active: Option[BindingScope] = None
  protected[diplomacy] def find(m: Option[LazyModule] = LazyModule.scope): Option[BindingScope] = m.flatMap {
    case x: BindingScope => find(x.parent).orElse(Some(x))
    case x => find(x.parent)
  }

  var bindingScopes = new collection.mutable.ArrayBuffer[BindingScope]()

  def add(bs: BindingScope) = BindingScope.bindingScopes.+=:(bs)
}

object ResourceBinding
{
  /** Add a resource callback function to the callback list BindingScope.resourceBindingFns.
    * @param block      the callback function to be added.
    */
  def apply(block: => Unit) {
    val scope = BindingScope.find()
    require (scope.isDefined, "ResourceBinding must be called from within a BindingScope")
    scope.get.resourceBindingFns = { () => block } +: scope.get.resourceBindingFns
  }
}

object ResourceAnchors
{
  val root = new Device {
    def describe(resources: ResourceBindings): Description = {
      val width = resources("width").map(_.value)
      val model = resources("model").map(_.value)
      val compat = resources("compat").map(_.value)
      Description("/", Map(
        "#address-cells" -> width,
        "#size-cells"    -> width,
        "model"          -> model,
        "compatible"     -> compat))
    }
  }

  val soc = new Device {
    def describe(resources: ResourceBindings): Description = {
      val width = resources("width").map(_.value)
      val compat = resources("compat").map(_.value) :+ ResourceString("simple-bus")
      Description("soc", Map(
        "#address-cells" -> width,
        "#size-cells"    -> width,
        "compatible"     -> compat,
        "ranges"         -> Nil))
    }
  }

  val cpus = new Device {
    def describe(resources: ResourceBindings): Description = {
      val width = resources("width").map(_.value)
      Description("cpus", Map(
        "#address-cells" -> width,
        "#size-cells"    -> Seq(ResourceInt(0))))
    }
  }

  val aliases = new Device {
    def describe(resources: ResourceBindings): Description =
      Description("aliases", Map() ++
        resources("uart").zipWithIndex.map { case (Binding(_, value), i) =>
          (s"serial${i}" -> Seq(value))})
  }
}
