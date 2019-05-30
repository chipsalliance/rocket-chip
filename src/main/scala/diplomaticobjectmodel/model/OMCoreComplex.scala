// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.model

case class CoreComplexRTLInterface(
  clocks: Seq[OMClock] = Nil,
  clockRelationships: Seq[OMClockRelationship] = Nil,
  resets: Seq[OMRTLReset] = Nil,
  statuses: Seq[OMStatus] = Nil,
  localInterrupts: Seq[OMInterruptSignal] = Nil,  // E.g. local_interrupts_X (all local interrupts for core X)
  globalInterrupts: Seq[OMInterruptSignal] = Nil,  // E.g. global_interrupts
  machineExternalInterrupts: Option[OMInterruptSignal] = None,  // E.g. meip_X
  testModeSignals: Seq[OMSignal] = Nil  // E.g. debug_psd_test_mode and debug_psd_test_mode_reset
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
