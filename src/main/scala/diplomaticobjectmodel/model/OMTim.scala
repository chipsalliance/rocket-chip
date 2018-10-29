// See LICENSE for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMTim(
  memoryRegions: List[OMMemoryRegion],
  interrupts: List[OMInterrupt]
)  extends OMDevice

