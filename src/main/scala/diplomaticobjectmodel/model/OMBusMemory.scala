// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model


sealed trait RAMArchitecture extends OMEnum
case object OMAXI4RAM extends RAMArchitecture
case object OMAHBRAM extends RAMArchitecture
case object OMAPBRAM extends RAMArchitecture
case object OMTLRAM extends RAMArchitecture

case class OMBusMemory(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  specifications: List[OMSpecification],
  memories: Seq[OMSRAM],
  architecture: RAMArchitecture,
  ecc: Option[OMECC] = None,
  hasAtomics: Option[Boolean] = None,
  _types: Seq[String] = Seq("OMBusMemory", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMDevice
