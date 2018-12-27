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
  nGlobalInterrupts: Int,  // Number of global interrupts going into the PLIC  nPriorities: Int,
  targets: List[OMInterruptTarget],
  _types: Seq[String] = Seq("OMPLIC", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMDevice
