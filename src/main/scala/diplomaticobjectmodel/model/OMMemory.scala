// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model


sealed trait RAMArchitecture extends OMEnum
case object OMAXI4RAM extends RAMArchitecture
case object OMAHBRAM extends RAMArchitecture
case object OMAPBRAM extends RAMArchitecture
case object OMTLRAM extends RAMArchitecture

case class OMMemory(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt] = Nil,
  override val rtlModule: Option[OMRTLModule] = None,
  architecture: RAMArchitecture,
  description: String,
  addressWidth: Int,
  dataWidth: Int,
  depth: Int,
  writeMaskGranularity: Int,
  ecc: Option[OMECC] = None,
  hasAtomics: Option[Boolean] = None,
  _types: Seq[String] = Seq("OMMemory")
) extends OMDevice

