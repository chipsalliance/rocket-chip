// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

sealed trait OMPrivilegeMode extends OMEnum
case object OMMachineMode extends OMPrivilegeMode
case object OMSupervisorMode extends OMPrivilegeMode
case object OMUserMode extends OMPrivilegeMode

object OMModes {
  def getModes(useVM: Boolean): Seq[OMPrivilegeMode] = {
    useVM match {
      case false => Seq(OMMachineMode)
      case true => Seq(OMMachineMode, OMSupervisorMode)
    }
  }
}

case class OMInterruptTarget(
  hartId: Int,
  modes: Seq[OMPrivilegeMode],
  _types: Seq[String] = Seq("OMInterruptTarget", "OMCompoundType")
) extends OMCompoundType

case class OMPLIC(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  specifications: Seq[OMSpecification],
  latency: Int,
  nPriorities: Int,
  nInterrupts: Int,
  targets: Seq[OMInterruptTarget],
  _types: Seq[String] = Seq("OMPLIC", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMDevice
