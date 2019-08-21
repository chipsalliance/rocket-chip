// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

trait OMDevice extends OMComponent {
  def memoryRegions: Seq[OMMemoryRegion]
  def interrupts: Seq[OMInterrupt]
}

