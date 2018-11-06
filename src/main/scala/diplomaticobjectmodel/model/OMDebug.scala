// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMDebug(
                    memoryRegions: List[OMMemoryRegion],
                    interrupts: List[OMInterrupt],
  specifications: List[OMSpecification],
  nAbstractDataWords: Int,
  nProgramBufferWords: Int,
  hasJtagDTM: Boolean
) extends OMDevice