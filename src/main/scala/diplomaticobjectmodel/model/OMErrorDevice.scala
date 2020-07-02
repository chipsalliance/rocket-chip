// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model


case class OMErrorDevice(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  _types: Seq[String] = Seq("OMErrorDevice", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMDevice
