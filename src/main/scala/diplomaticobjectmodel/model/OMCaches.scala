// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

sealed trait OMECC extends OMBaseType

case object Identity extends OMECC
case object Parity extends OMECC
case object SEC extends OMECC
case object SECDED extends OMECC

object OMECC {
  def getCode(code: String): OMECC = {
    code match {
      case "Identity" => Identity
      case "Parity"   => Parity
      case "SEC"      => SEC
      case "SECDED"   => SECDED
      case _ => throw new IllegalArgumentException("ERROR: invalid getCode arg: $code")
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
  maxTimSize: Int
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
  memories: List[OMMemory],
  maxTimSize: Int
) extends OMCache
