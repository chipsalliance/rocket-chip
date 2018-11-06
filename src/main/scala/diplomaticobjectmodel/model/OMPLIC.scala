// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

sealed trait OMPrivilegeMode extends OMEnum
case object MachineMode extends OMPrivilegeMode
case object SupervisorMode extends OMPrivilegeMode
case object UserMode extends OMPrivilegeMode

case class InterruptTarget(
  hartId: Int,
  mode: OMPrivilegeMode
) extends OMCompoundType

case class OMPLIC(
  memoryRegions: List[OMMemoryRegion],
  interrupts: List[OMInterrupt],
  specifications: List[OMSpecification],
  latency: Int,
  nInterrupts: Int,  // plic.nInterrupts - coreComplex.nExternalGlobalInterrupts == internal global interrupts from devices inside of the Core Complex
  nPriorities: Int,
  targets: List[InterruptTarget]
) extends OMDevice