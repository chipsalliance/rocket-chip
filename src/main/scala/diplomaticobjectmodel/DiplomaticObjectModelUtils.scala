// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel

import java.io.{File, FileWriter}

import Chisel.{Data, Vec, log2Ceil}
import freechips.rocketchip.diplomacy.{ AddressSet, Binding, Device, DiplomacyUtils, ResourceAddress, ResourceBindings, ResourceBindingsMap, ResourceInt, ResourceMapping, ResourcePermissions, ResourceValue, SimpleDevice}
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.util.Code
import org.json4s.jackson.JsonMethods.pretty
import org.json4s.jackson.Serialization
import org.json4s.{CustomSerializer, Extraction, NoTypeHints}


object DiplomaticObjectModelUtils {

  def toJson(json: Any): String = {
    implicit val formats = Serialization.formats(NoTypeHints) + new OMEnumSerializer
    pretty(Extraction.decompose(json))
  }

  def addTypes(json: Any): String = {
    toJson(json)
  }

  def writeJsonFile(filename: String, json: Map[String, Any]): Unit = {
    val writer = new FileWriter(new File(filename))
    writer.write(toJson(json))
    writer.close()
  }

  /**
   * Get the demangled name for the class.
   *
   * In Scala companion objects have a trailing $, so this will strip the
   * trailing $.
   */
  def getDemangledName(claz: Class[_]): String =
    """\$.*$""".r.replaceFirstIn(claz.getSimpleName, "")

  /**
    * Given a sequence of strings, remove duplicates by keeping only
    *   the last occurrence in the sequence.
    */
  def keepLast(names: Seq[String]): Seq[String] = {
    var keepers = List[String]()
    var seen = Set[String]()

    // Coded as an imperative loop, could easily be functional.
    for (name <- names.reverse)
      if (!seen(name)) {
        keepers = name +: keepers
        seen = seen + name
      }

    keepers.toSeq
  }

  /**
   * Get a list of super classes and traits for a class
   */
  def getSuperClasses(klass: Class[_]): Seq[Class[_]] = {

    if (klass == null)
      Seq()

    else {
      val superKlass = klass.getSuperclass
      val interfaces = klass.getInterfaces
      val classes    = klass.getClasses

      val parents = if (superKlass == null)  interfaces
      else          interfaces :+ superKlass

      val ancestors = for {parent <- parents; ancestor <- getSuperClasses(parent)}
        yield ancestor

      (klass +: parents) ++ ancestors
    }
  }

  def getAllClassNames(klass: Class[_]): Seq[String] =
    keepLast(getSuperClasses(klass).map(getDemangledName _))
}

class OMEnumSerializer extends CustomSerializer[OMEnum](format => {
  import org.json4s.JsonDSL._
  (
    Map.empty,
    {
      // Note: This is only meant to work for our OMEnum types, where we don't
      // need to recursively serialize child properties because our OMEnum types
      // do not have members. We currently don't have a way of recursively
      // serializing objects without basically reimplementing the default
      // serializers for case classes.
      case inst: OMEnum => ("_types" -> DiplomaticObjectModelUtils.getAllClassNames(inst.getClass))
    }
  )
})

object DiplomaticObjectModelAddressing {
  def getOMComponentHelper(resourceBindings: ResourceBindings, fn: (ResourceBindings) => Seq[OMComponent]): Seq[OMComponent] = {
    fn(resourceBindings)
  }

  private def omPerms(p: ResourcePermissions): OMPermissions = {
    OMPermissions(
      readable = p.r,
      writeable = p.w,
      executable = p.x,
      cacheable = p.c,
      atomics = p.a
    )
  }

  private def omAddressSets(ranges: Seq[AddressSet], name: String): Seq[OMAddressSet] = {
    ranges.map {
      case AddressSet(base, mask) =>
        require(mask != 0, s"omAddressSets: $name has invalid mask of 0")
        OMAddressSet(base = base, mask = mask)
    }
  }

  private def omMemoryRegion(name: String, description: String, value: ResourceValue, omRegMap: Option[OMRegisterMap]): OMMemoryRegion = {
    val (omRanges, permissions) = value match {
      case rm: ResourceMapping =>
        (omAddressSets(rm.address, name),rm.permissions)
      case ra: ResourceAddress => (omAddressSets(ra.address, name), ra.permissions)
      case _ => throw new IllegalArgumentException()
    }

    OMMemoryRegion(
      name = name,
      description = description,
      addressSets = omRanges,
      permissions = omPerms(permissions),
      registerMap = omRegMap
    )
  }

  def getOMInterrupts(resourceBindings: ResourceBindings): Seq[OMInterrupt]= {
    Nil
  }

  def getOMMemoryRegions(name: String, resourceBindings: ResourceBindings, omRegMap: Option[OMRegisterMap] = None): Seq[OMMemoryRegion]= {
    resourceBindings.map.collect {
      case (x: String, seq: Seq[Binding]) if (DiplomacyUtils.regFilter(x) || DiplomacyUtils.rangeFilter(x)) =>
        seq.map {
          case Binding(device: Option[Device], value: ResourceValue) => omMemoryRegion(name, DiplomacyUtils.regName(x).getOrElse(""), value, omRegMap)
        }
    }.flatten.toSeq
  }

  def getOMPortMemoryRegions(name: String, resourceBindings: ResourceBindings, omRegMap: Option[OMRegisterMap] = None): Seq[OMMemoryRegion]= {
    resourceBindings.map.collect {
      case (x: String, seq: Seq[Binding]) if (DiplomacyUtils.regFilter(x) || DiplomacyUtils.rangeFilter(x)) =>
        seq.map {
          case Binding(device: Option[Device], value: ResourceValue) => omMemoryRegion(name, "port memory region", value, omRegMap)
        }
    }.flatten.toSeq
  }

  def makeOMSRAM(
    desc: String,
    width: Int,
    depth: BigInt,
    granWidth: Int,
    uid: Int
  ): OMSRAM = {
    OMSRAM(
      description = desc,
      addressWidth = log2Ceil(depth),
      dataWidth = width,
      depth = depth,
      writeMaskGranularity = granWidth,
      uid = uid
    )
  }

  def makeOMMemory[T <: Data](
      desc: String,
      depth: BigInt,
      data: T
    ): OMMemory = {

      val granWidth = data match {
        case v: Vec[_] => v.head.getWidth
        case d => d.getWidth
      }

      OMMemory(
        description = desc,
        addressWidth = log2Ceil(depth),
        dataWidth = data.getWidth,
        depth = depth,
        writeMaskGranularity = granWidth
      )
    }

  private def getInterruptNumber(r: ResourceValue): BigInt = {
    r match {
      case ResourceInt(value: BigInt) => value
      case _ => throw new IllegalArgumentException
    }
  }

  private def getDeviceName(device: Device, resources: ResourceBindings): String = {
    device match {
      case sd:SimpleDevice => sd.asInstanceOf[SimpleDevice].deviceNamePlusAddress
      case _ => throw new IllegalArgumentException(s"Error: getDeviceName: " + device.getClass.toString() + "\n")
    }
  }

  def describeInterrupts(name: String, resources: ResourceBindings): Seq[OMInterrupt] = {
    val int = resources("int")
    for {
      b <- int
      grandParentOpt = b.device.get.parent
      gp <- grandParentOpt
    } yield OMInterrupt(
      receiver = getDeviceName(gp, resources),
      numberAtReceiver = getInterruptNumber(b.value),
      name = name
    )
  }

  def describeGlobalInterrupts(name: String, resources: ResourceBindings): Seq[OMInterrupt] = {
    val bindings = resources("int")
    for {
      binding <- bindings
      device = binding.device.get
    } yield OMInterrupt(
      receiver = device.describe(resources).name,
      numberAtReceiver = getInterruptNumber(binding.value),
      name = name
    )
  }
}
