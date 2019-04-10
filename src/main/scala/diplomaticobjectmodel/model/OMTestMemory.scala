// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMTestMemory(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  memory: OMMemory,
  override val rtlModule: Option[OMRTLModule]
) extends OMDevice

