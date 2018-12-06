// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

import freechips.rocketchip.diplomacy.ResourceBindings
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.rocket.{DCacheParams, ICacheParams}

sealed trait OMECC extends OMBaseType

case object Identity extends OMECC
case object Parity extends OMECC
case object SEC extends OMECC
case object SECDED extends OMECC

trait OMCache extends OMDevice {
  def memoryRegions(): Seq[OMMemoryRegion]
  def interrupts(): Seq[OMInterrupt]
  def nSets: Int
  def nWays: Int
  def blockSizeBytes: Int
  def dataMemorySizeBytes: Int
  def dataECC: Option[OMECC]
  def tagECC: Option[OMECC]
  def nTLBEntries: Int
}

case class OMICache(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  nSets: Int,
  nWays: Int,
  blockSizeBytes: Int,
  dataMemorySizeBytes: Int,
  dataECC: Option[OMECC],
  tagECC: Option[OMECC],
  nTLBEntries: Int,
  maxTimSize: Int,
  _types: Seq[String] = Seq("OMICache", "OMCache", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMCache

case class OMDCache(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  nSets: Int,
  nWays: Int,
  blockSizeBytes: Int,
  dataMemorySizeBytes: Int,
  dataECC: Option[OMECC],
  tagECC: Option[OMECC],
  nTLBEntries: Int,
  _types: Seq[String] = Seq("OMDCache", "OMCache", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMCache

object OMECC {
  def getCode(code: String): OMECC = {
    code.toLowerCase match {
      case "identity" => Identity
      case "parity"   => Parity
      case "sec"      => SEC
      case "secded"   => SECDED
      case _ => throw new IllegalArgumentException(s"ERROR: invalid getCode arg: $code")
    }
  }
}

object OMCaches {
  //      //If DCache is configured as a DTIM, then you should populate the memory regions
  def dcache(p: DCacheParams): OMDCache = {
    val memoryRegions = Nil // TODO
    OMDCache(
      memoryRegions = Nil,
      interrupts = Nil,
      nSets = p.nSets,
      nWays = p.nWays,
      blockSizeBytes = p.blockBytes,
      dataMemorySizeBytes = p.nSets * p.nWays * p.blockBytes,
      dataECC = p.dataECC.map(OMECC.getCode(_)),
      tagECC = p.tagECC.map(OMECC.getCode(_)),
      nTLBEntries = p.nTLBEntries
    )
  }

  def icache(p: ICacheParams, resourceBindings: ResourceBindings): Seq[OMComponent] = {
    Seq[OMComponent](
      OMICache(
        memoryRegions = DiplomaticObjectModelAddressing.getOMMemoryRegions("ICache", resourceBindings),
        interrupts = Nil,
        nSets = p.nSets,
        nWays = p.nWays,
        blockSizeBytes = p.blockBytes,
        dataMemorySizeBytes = p.nSets * p.nWays * p.blockBytes,
        dataECC = p.dataECC.map(OMECC.getCode),
        tagECC = p.tagECC.map(OMECC.getCode),
        nTLBEntries = p.nTLBEntries,
        maxTimSize = p.nSets * (p.nWays-1) * p.blockBytes
      )
    )
  }

}