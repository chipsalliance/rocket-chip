// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel

import java.io.{File, FileWriter}

import Chisel.{Data, Vec, log2Ceil}
import freechips.rocketchip.diplomacy.{AddressRange, AddressSet, Binding, Device, DiplomacyUtils, ResourceAddress, ResourceBindings, ResourceMapping, ResourcePermissions, ResourceValue}
import freechips.rocketchip.diplomaticobjectmodel.model._
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

  private def omAddressSets(ranges: Seq[AddressSet]): Seq[OMAddressSet] = {
    ranges.map {
      case AddressSet(base, mask) =>
        OMAddressSet(base = base, mask = mask)
    }
  }

  private def omMemoryRegion(name: String, regName: String, value: ResourceValue): OMMemoryRegion = {
    val (omRanges, permissions) = value match {
      case rm: ResourceMapping =>
        (omAddressSets(rm.address),rm.permissions)
      case ra: ResourceAddress => (omAddressSets(ra.address), ra.permissions)
      case _ => throw new IllegalArgumentException()
    }

    OMMemoryRegion(
      name = name,
      description = regName,
      addressSets = omRanges,
      permissions = omPerms(permissions),
      registerMap = None // Option[OMRegisterMap]
    )
  }

  def getOMInterrupts(resourceBindings: ResourceBindings): Seq[OMInterrupt]= {
    Nil
  }

  def getOMMemoryRegions(name: String, resourceBindings: ResourceBindings): Seq[OMMemoryRegion]= {
    resourceBindings.map.collect {
      case (x: String, seq: Seq[Binding]) if (DiplomacyUtils.regFilter(x)) =>
        seq.map {
          case Binding(device: Option[Device], value: ResourceValue) => omMemoryRegion(name, DiplomacyUtils.regName(x).getOrElse(""), value)
        }
    }.flatten.toSeq
  }

  def makeOMMemory[T <: Data](
      rtlModule: OMRTLModule,
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
        writeMaskGranularity = granWidth,
        rtlModule = rtlModule
      )
    }
}
