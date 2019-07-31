// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMCLINT(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
   omReference: OMReference,
specifications: List[OMSpecification],
  _types: Seq[String] = Seq("OMCLINT", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMDevice
