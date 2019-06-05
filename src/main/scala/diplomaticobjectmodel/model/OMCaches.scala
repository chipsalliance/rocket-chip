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

case class OMECC(code: String) extends OMEnum

object OMECC {
  val Identity = OMECC("Identity")
  val Parity = OMECC("Parity")
  val SEC = OMECC("SEC")
  val SECDED = OMECC("SECDED")

  def fromString(code: String): OMECC = {
    code.toLowerCase match {
      case "identity" => OMECC.Identity
      case "parity"   => OMECC.Parity
      case "sec"      => OMECC.SEC
      case "secded"   => OMECC.SECDED
      case _ => throw new IllegalArgumentException(s"ERROR: invalid getCode arg: $code")
    }
  }

  def fromCode(code: Code): OMECC = {
    code match {
      case _: IdentityCode => OMECC.Identity
      case _: ParityCode   => OMECC.Parity
      case _: SECCode      => OMECC.SEC
      case _: SECDEDCode   => OMECC.SECDED
      case _ => throw new IllegalArgumentException(s"ERROR: invalid getCode arg: $code")
    }
  }
}
