// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel

import java.io.{File, FileWriter}

import Chisel.{Data, Vec, log2Ceil}
import freechips.rocketchip.diplomacy.{AddressRange, AddressSet, Binding, Device, DiplomacyUtils, ResourceAddress, ResourceBindings, ResourceBindingsMap, ResourceMapping, ResourcePermissions, ResourceValue}
import freechips.rocketchip.diplomaticobjectmodel.model._
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

  def getResourceBindings(device: Device, resourceBindingsMap: ResourceBindingsMap): Option[ResourceBindings] = {
    require(resourceBindingsMap.map.contains(device))
    resourceBindingsMap.map.get(device)
  }

  def getOMComponentHelper(device: Device, resourceBindingsMap: ResourceBindingsMap, fn: (ResourceBindings) => Seq[OMComponent]): Seq[OMComponent] = {
    require(resourceBindingsMap.map.contains(device))
    val resourceBindings = resourceBindingsMap.map.get(device)
    resourceBindings.map { case rb => fn(rb) }.getOrElse(Nil)
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

  private def omAddressSets(ranges: Seq[AddressSet]): Seq[OMAddressSet] = {
    ranges.map {
      case AddressSet(base, mask) =>
        OMAddressSet(base = base, mask = mask)
    }
  }

  private def omMemoryRegion(name: String, description: String, value: ResourceValue, omRegMap: Option[OMRegisterMap]): OMMemoryRegion = {
    val (omRanges, permissions) = value match {
      case rm: ResourceMapping =>
        (omAddressSets(rm.address),rm.permissions)
      case ra: ResourceAddress => (omAddressSets(ra.address), ra.permissions)
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
      case (x: String, seq: Seq[Binding]) if (DiplomacyUtils.regFilter(x)) =>
        seq.map {
          case Binding(device: Option[Device], value: ResourceValue) => omMemoryRegion(name, DiplomacyUtils.regName(x).getOrElse(""), value, omRegMap)
        }
    }.flatten.toSeq
  }

  def getOMPortMemoryRegions(name: String, resourceBindings: ResourceBindings, omRegMap: Option[OMRegisterMap] = None): Seq[OMMemoryRegion]= {
    resourceBindings.map.collect {
      case (x: String, seq: Seq[Binding]) if (DiplomacyUtils.rangeFilter(x)) =>
        seq.map {
          case Binding(device: Option[Device], value: ResourceValue) => omMemoryRegion(name, "port memory region", value, omRegMap)
        }
    }.flatten.toSeq
  }

  def makeOMMemory[T <: Data](
      desc: String,
      depth: Int,
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
}
