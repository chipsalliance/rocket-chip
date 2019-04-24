// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model


sealed trait RAMArchitecture extends OMEnum
case object OMAXI4RAM extends RAMArchitecture
case object OMAHBRAM extends RAMArchitecture
case object OMAPBRAM extends RAMArchitecture
case object OMTLRAM extends RAMArchitecture

case class OMTestMemory(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  specifications: List[OMSpecification],
  memories: Seq[OMMemory],
  architecture: RAMArchitecture,
  ecc: Option[OMECC] = None,
  hasAtomics: Option[Boolean] = None,
  _types: Seq[String] = Seq("OMTestMemory", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMDevice

case class OMTestHarness(
  components: Seq[OMComponent],
  _types: Seq[String] = Seq("OMTestHarness")
) extends OMComponent
