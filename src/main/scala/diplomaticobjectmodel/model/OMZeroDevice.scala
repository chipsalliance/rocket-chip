// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model


case class OMZeroDevice(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  _types: Seq[String] = Seq("OMZeroDevice", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMDevice
