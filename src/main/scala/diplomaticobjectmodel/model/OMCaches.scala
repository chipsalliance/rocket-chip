// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

import freechips.rocketchip.rocket.DCacheParams

sealed trait OMECC extends OMBaseType

case object Identity extends OMECC
case object Parity extends OMECC
case object SEC extends OMECC
case object SECDED extends OMECC

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

trait OMCache extends OMDevice {
  def nSets: Int
  def nWays: Int
  def blockSizeBytes: Int
  def dataMemorySizeBytes: Int
  def dataECC: Option[OMECC]
  def tagECC: Option[OMECC]
  def nTLBEntries: Int
  def memories: Seq[OMMemory]
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
  memories: Seq[OMMemory],
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
  memories: Seq[OMMemory],
  maxTimSize: Int,
  _types: Seq[String] = Seq("OMDCache", "OMCache", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMCache

object OMDCache {
  def makeOMI(p: DCacheParams, memories: Seq[OMMemory]): OMDCache = {
    val x = p.dataECC.map(OMECC.getCode(_)).getOrElse()
    OMDCache(
      memoryRegions = Nil,
      interrupts = Nil,
      nSets = p.nSets,
      nWays = p.nWays,
      blockSizeBytes = p.blockBytes,
      dataMemorySizeBytes = p.nSets * p.nWays * p.blockBytes,
      dataECC = p.dataECC.map(OMECC.getCode(_)),
      tagECC = p.tagECC.map(OMECC.getCode(_)),
      nTLBEntries = p.nTLBEntries,
      memories = memories,
      maxTimSize = 0
    )
  }
}
