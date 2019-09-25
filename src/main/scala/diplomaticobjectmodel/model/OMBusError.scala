// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model


case class OMBusError(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  specifications: Seq[OMSpecification],
  _types: Seq[String] = Seq("OMBusError", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMDevice
