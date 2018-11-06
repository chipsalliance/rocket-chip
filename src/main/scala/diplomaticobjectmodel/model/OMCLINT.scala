// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMCLINT(
  memoryRegions: List[OMMemoryRegion],
  interrupts: List[OMInterrupt],
  specifications: List[OMSpecification]
) extends OMDevice