// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

sealed trait DebugInterfaceType extends OMEnum
case object JTAG extends DebugInterfaceType
case object CJTAG extends DebugInterfaceType
case object DMI extends DebugInterfaceType

case class OMDebug(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  specifications: List[OMSpecification],
  nAbstractDataWords: Int,
  nProgramBufferWords: Int,
  interfaceType: DebugInterfaceType,
  _types: Seq[String] = Seq("OMDebug", "OMDevice", "OMComponent", "OMCompoundType")
)  extends OMDevice

object OMDebug {

  def getDebugInterfaceType(jtag: Boolean, cjtag: Boolean, dmi: Boolean): DebugInterfaceType = {
    if (jtag) { JTAG }
    else if (cjtag) { CJTAG }
    else if (dmi) { DMI }
    else { throw new IllegalArgumentException }
  }
}