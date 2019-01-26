// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

sealed trait OMPrivilegeMode extends OMEnum
case object OMMachineMode extends OMPrivilegeMode
case object OMSupervisorMode extends OMPrivilegeMode
case object OMUserMode extends OMPrivilegeMode

case class OMInterruptTarget(
  hartId: Int,
  mode: OMPrivilegeMode,
  _types: Seq[String] = Seq("OMInterrupt", "OMCompoundType")
) extends OMCompoundType

case class OMPLIC(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  specifications: List[OMSpecification],
  latency: Int,
  nInterrupts: Int, // plic.nInterrupts - coreComplex.nExternalGlobalInterrupts == internal global interrupts from devices inside of the Core Complex
  nPriorities: Int,
  targets: List[OMInterruptTarget],
  _types: Seq[String] = Seq("OMPLIC", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMDevice
