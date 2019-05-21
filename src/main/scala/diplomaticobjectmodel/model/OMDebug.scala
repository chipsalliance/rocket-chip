// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model


import freechips.rocketchip.config._
import freechips.rocketchip.devices.debug.{DebugModuleParams, ExportDebugCJTAG, ExportDebugDMI, ExportDebugJTAG, ExportDebugAPB}

sealed trait OMDebugInterfaceType extends OMEnum
case object JTAG extends OMDebugInterfaceType
case object CJTAG extends OMDebugInterfaceType
case object DMI extends OMDebugInterfaceType
case object DebugAPB extends OMDebugInterfaceType

sealed trait OMDebugAuthenticationType extends OMEnum
case object NONE extends OMDebugAuthenticationType

// These directly come from RISC-V Debug Spec 0.14
case class OMDebug(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  specifications: List[OMSpecification],
  interfaceType: OMDebugInterfaceType,
  nSupportedHarts: Int,
  nAbstractDataWords: Int,
  nProgramBufferWords: Int,
  nDMIAddressSizeBits: Int,
  hasSystemBusAccess: Boolean,
  supportsQuickAccess: Boolean,
  supportsHartArray: Boolean,
  hasImplicitEbreak: Boolean,
  sbcsSBAVersion: Int, // This should just always be 1. 0 has tons of issues.
  sbaAddressSizeBits: Int,
  hasSBAccess8: Boolean,
  hasSBAccess16: Boolean,
  hasSBAccess32: Boolean,
  hasSBAccess64: Boolean,
  hasSBAccess128: Boolean,
  hartSeltoHartIDMapping: List[Int], // HartSel goes from 0->N but HartID is not contiguious or increasing
  authenticationType: OMDebugAuthenticationType,
  nHartsellenBits: Int, // Number of actually implemented bits of Hartsel
  hasHartInfo: Boolean,
  //supportedHartArrayWindowBits: List[Int], -- Getting rid of this because it makes no sense.
  hasAbstractauto: Boolean,
  cfgStrPtrValid: Boolean,
  nHaltSummaryRegisters: Int,
  nHaltGroups: Int,
  nExtTriggers: Int,
  hasResetHaltReq: Boolean,
  hasHartReset: Boolean,
  hasAbstractAccessFPU: Boolean,
  hasAbstractAccessCSR: Boolean,
  hasAbstractAccessMemory: Boolean, // There is a bunch more stuff if this is true, but we're ignoring it because we didn't and won't implement it.
  hasCustom: Boolean, // (This makes some registers visible in the non-standard extensions range. More info would be necessary for exactly what registers)
  hasAbstractPostIncrement: Boolean,
  hasAbstractPostExec: Boolean,
  hasClockGate: Boolean,
  _types: Seq[String] = Seq("OMDebug", "OMDevice", "OMComponent", "OMCompoundType")
) extends OMDevice

object OMDebug {
  def getOMDebugInterfaceType(p: Parameters): OMDebugInterfaceType = {
    if (p(ExportDebugJTAG)) { JTAG }
    else if (p(ExportDebugCJTAG)) { CJTAG }
    else if (p(ExportDebugDMI)) { DMI }
    else if (p(ExportDebugAPB)) { DebugAPB }
    else { throw new IllegalArgumentException }
  }
}
