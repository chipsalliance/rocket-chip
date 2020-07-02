// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMBusMemory(
  memoryRegions: Seq[OMMemoryRegion] = Nil,
  interrupts: Seq[OMInterrupt] = Nil,
  specifications: Seq[OMSpecification] = Nil,
  busProtocol: Option[OMProtocol] = None,
  dataECC: OMECC = OMECCIdentity,
  hasAtomics: Boolean = false,
  memories: Seq[OMSRAM],
  _types: Seq[String] = Seq("OMBusMemory", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMDevice
