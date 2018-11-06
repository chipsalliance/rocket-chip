// See LICENSE for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

trait OMDevice extends OMComponent {
  def memoryRegions: List[OMMemoryRegion]
  def interrupts: List[OMInterrupt]
}

