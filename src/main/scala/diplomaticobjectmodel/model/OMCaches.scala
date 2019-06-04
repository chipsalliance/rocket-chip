// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

import freechips.rocketchip.util.{Code, IdentityCode, ParityCode, SECCode, SECDEDCode}

trait OMCache extends OMDevice {
  def memoryRegions(): Seq[OMMemoryRegion]
  def interrupts(): Seq[OMInterrupt]
  def nSets: Int
  def nWays: Int
  def blockSizeBytes: Int
  def dataMemorySizeBytes: Int
  def dataECC: Option[OMECC]
  def tagECC: Option[OMECC]
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

trait OMECC extends OMEnum

case object OMECCIdentity extends OMECC
case object OMECCParity extends OMECC
case object OMECCSEC extends OMECC
case object OMECCSECDED extends OMECC

object OMECC {
  def fromString(code: String): OMECC = {
    code.toLowerCase match {
      case "identity" => OMECCIdentity
      case "parity"   => OMECCParity
      case "sec"      => OMECCSEC
      case "secded"   => OMECCSECDED
      case _ => throw new IllegalArgumentException(s"ERROR: invalid getCode arg: $code")
    }
  }

  def fromCode(code: Code): OMECC = {
    code match {
      case _: IdentityCode => OMECCIdentity
      case _: ParityCode   => OMECCParity
      case _: SECCode      => OMECCSEC
      case _: SECDEDCode   => OMECCSECDED
      case _ => throw new IllegalArgumentException(s"ERROR: invalid getCode arg: $code")
    }
  }
}
