// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

sealed trait OMPrivilegeMode extends OMEnum
case object OMMachineMode extends OMPrivilegeMode
case object OMSupervisorMode extends OMPrivilegeMode
case object OMUserMode extends OMPrivilegeMode

case class OMInterruptTarget(
  hartId: Int,
  mode: OMPrivilegeMode,
  _types: Seq[String] = Seq("OMInterruptTarget", "OMCompoundType")
) extends OMCompoundType

case class OMPLIC(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  specifications: Seq[OMSpecification],
  latency: Int,
  nPriorities: Int,
  targets: Seq[OMInterruptTarget],
  _types: Seq[String] = Seq("OMPLIC", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMDevice

object OMPLIC {
  def getMode(mode: String): OMPrivilegeMode = {
    mode match {
      case "M" => OMMachineMode
      case "S" => OMSupervisorMode
      case "U" => OMUserMode
    }
  }

}