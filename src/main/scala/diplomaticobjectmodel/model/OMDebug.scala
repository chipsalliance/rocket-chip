// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMDebug(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  specifications: List[OMSpecification],
  nAbstractDataWords: Int,
  nProgramBufferWords: Int,
  hasJtagDTM: Boolean,
  _types: Seq[String] = Seq("OMDebug", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMDevice
