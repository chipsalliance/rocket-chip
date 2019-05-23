// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class CoreComplexRTLInterface(
  clocks: List[OMClock] = Nil,
  clockRelationships: List[OMClockRelationship] = Nil,
  resets: List[OMRTLReset] = Nil,
  statuses: List[Status] = Nil,
  localInterrupts: Option[OMInterruptSignal] = None,  // E.g. local_interrupts_X (all local interrupts for core X)
  globalInterrupts: Option[OMInterruptSignal] = None,  // E.g. global_interrupts
  machineExternalInterrupts: Option[OMInterruptSignal] = None,  // E.g. meip_X
  testModeSignals: List[OMSignal] = Nil  // E.g. debug_psd_test_mode and debug_psd_test_mode_reset
)  extends OMRTLInterface

case class OMCoreComplexRTLModule(
  moduleName: String,
  instanceName: Option[String],
  hierarchicalId: Option[String],
  coreComplexinterface: CoreComplexRTLInterface
)

case class OMCoreComplex(
  components: Seq[OMComponent],
  documentationName: String,
  rtlModule: Option[OMCoreComplexRTLModule],
  _types: Seq[String] = Seq("OMCoreComplex", "OMComponent", "OMCompoundType")
) extends OMComponent
