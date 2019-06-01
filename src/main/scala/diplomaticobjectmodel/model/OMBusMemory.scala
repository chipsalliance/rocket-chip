// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class OMBusMemory(
  memoryRegions: Seq[OMMemoryRegion] = Nil,
  interrupts: Seq[OMInterrupt] = Nil,
  specifications: Seq[OMSpecification] = Nil,
  busProtocol: Option[OMProtocol] = None,
  dataECC: OMECC = OMECC.Identity,
  hasAtomics: Boolean = false,
  memories: Seq[OMSRAM]
) extends OMDevice
