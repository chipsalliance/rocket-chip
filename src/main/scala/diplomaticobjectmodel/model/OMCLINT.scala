// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMCLINT(
                    memoryRegions: Seq[OMMemoryRegion],
                    interrupts: Seq[OMInterrupt],
                    specifications: List[OMSpecification]
) extends OMDevice