// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel

import java.io.{File, FileWriter}
import java.lang.management.OperatingSystemMXBean

import Chisel.{Data, SeqMem, Vec, log2Ceil}
import chisel3.SyncReadMem
import freechips.rocketchip.diplomacy.DTS.{Cells, fmtCell}
import freechips.rocketchip.diplomacy.{AddressRange, Binding, Device, ResourceAddress, ResourceAlias, ResourceBindings, ResourceInt, ResourceMap, ResourceMapping, ResourcePermissions, ResourceReference, ResourceString, ResourceValue}
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.util.Annotated
import org.json4s.jackson.JsonMethods.pretty
import org.json4s.jackson.Serialization
import org.json4s.{Extraction, NoTypeHints}


object DiplomaticObjectModelUtils {

  def toJson(json: Any): String = {
    implicit val formats = Serialization.formats(NoTypeHints)
    pretty(Extraction.decompose(json))
  }

  def writeJsonFile(filename: String, json: Map[String, Any]): Unit = {
    val writer = new FileWriter(new File(filename))
    writer.write(toJson(json))
    writer.close()
  }
}

object DiplomaticObjectModelAddressing {

  private def omPerms(p: ResourcePermissions): OMPermissions = {
    OMPermissions(
      readable = p.r,
      writeable = p.w,
      executable = p.x,
      cacheable = p.c,
      atomics = p.a
    )
  }

  private def omAddressSets(ranges: Seq[AddressRange]): Seq[OMAddressSet] = {
    ranges.map {
      case AddressRange(base, size) =>
        OMAddressSet(base = base, mask = size - 1)
    }
  }

  private def omAddress(x: ResourceAddress): Seq[OMAddressSet] = {
    val ranges = AddressRange.fromSets(x.address)
    omAddressSets(ranges)
  }

  private def omMemoryRegion(name: String, value: ResourceValue): OMMemoryRegion = {
    val (omRanges, permissions) = value match {
      case rm: ResourceMapping =>
        val ranges = AddressRange.fromSets(rm.address)
        (omAddressSets(ranges),rm.permissions)
      case ra: ResourceAddress => (omAddress(ra), ra.permissions)
      case _ => throw new IllegalArgumentException()
    }

    OMMemoryRegion(
      name = name,
      description = "",
      addressSets = omRanges,
      permissions = omPerms(permissions),
      registerMap = None // Option[OMRegisterMap]
    )
  }

  def getOMInterrupts(resourceBindings: ResourceBindings): Seq[OMInterrupt]= {
    Nil
  }

  def regFilter(name: String): Boolean = name == "reg" || name.take(4) == "reg/"

  def regName(name: String): Option[String] = {
    val keys = name.split("/")
    require (keys.size >= 1 && keys.size <= 2 && keys(0) == "reg", s"Invalid reg name '${name}'")
    if (keys.size == 1) None else Some(keys(1))
  }

  def getOMMemoryRegions(name: String, resourceBindings: ResourceBindings): Seq[OMMemoryRegion]= {
    resourceBindings.map.flatMap {
      case (x: String, seq: Seq[Binding]) if (regFilter(x)) =>
        println(s"ResourceBindings: key = reg/control Binding")
        Some(seq.map {
          case Binding(device: Option[Device], value: ResourceValue) => omMemoryRegion(name, value)
        })
      case _ => None
    }.flatten.toSeq
  }

  def printMR(name: String, seq: Seq[OMMemoryRegion]): Unit = {
    println(s"printMR name = %s".format(name))
    seq.map{
      case mr =>
        println(s"  printMR perms r = %s w = %s x = %s c = %s a = %s ".format(mr.permissions.readable, mr.permissions.writeable,
          mr.permissions.executable, mr.permissions.cacheable, mr.permissions.atomics))
        mr.addressSets.map{
          case as =>
            println(s"    printMR base = %s mask = %s".format(as.base, as.mask))
        }
    }
  }

  def makeOMMemory[T <: Data](
      rtlModule: OMRTLModule,
      desc: String,
      size: Int, // depth
      data: T
    ): OMMemory = {

      val granWidth = data match {
        case v: Vec[_] => v.head.getWidth
        case d => d.getWidth
      }

      OMMemory(
        description = desc,
        addressWidth = log2Ceil(size),
        dataWidth = data.getWidth,
        depth = size,
        writeMaskGranularity = granWidth,
        rtlModule = rtlModule
      )
    }
}
