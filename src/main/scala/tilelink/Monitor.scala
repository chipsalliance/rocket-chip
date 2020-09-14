// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import chisel3.internal.sourceinfo.SourceLine
import chisel3.experimental.chiselName
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.PlusArg
import freechips.rocketchip.formal._

case class TLMonitorArgs(edge: TLEdge)

	abstract class TLMonitorBase(args: TLMonitorArgs) extends Module
{
	val io = IO(new Bundle {
			val in = Input(new TLBundle(args.edge.bundle))
			})

	def legalize(bundle: TLBundle, edge: TLEdge, reset: Reset): Unit
								    legalize(io.in, args.edge, reset)
}

object TLMonitor {
	def apply(enable: Boolean, node: TLNode)(implicit p: Parameters): TLNode = {
		if (enable) {
			EnableMonitors { implicit p => node := TLEphemeralNode()(ValName("monitor")) }
		} else { node }
	}
}

	@chiselName
class TLMonitor(args: TLMonitorArgs, monitorDir: MonitorDirection = MonitorDirection.Monitor) extends TLMonitorBase(args)
{
  require (args.edge.params(TLMonitorStrictMode) || (! args.edge.params(TestplanTestType).formal))

  val cover_prop_class = PropertyClass.Default

  //Like assert but can flip to being an assumption for formal verification
  def monAssert(cond: Bool, message: String): Unit =
  if (monitorDir == MonitorDirection.Monitor) {
    assert(cond, message)
  } else {
    Property(monitorDir, cond, message, PropertyClass.Default)
  }

  def assume(cond: Bool, message: String): Unit =
  if (monitorDir == MonitorDirection.Monitor) {
    assert(cond, message)
  } else {
    Property(monitorDir.flip, cond, message, PropertyClass.Default)
  }

  def extra = {
    args.edge.sourceInfo match {
      case SourceLine(filename, line, col) => s" (connected at $filename:$line:$col)"
      case _ => ""
    }
  }

  def visible(address: UInt, source: UInt, edge: TLEdge) =
    edge.client.clients.map { c =>
      !c.sourceId.contains(source) ||
      c.visibility.map(_.contains(address)).reduce(_ || _)
    }.reduce(_ && _)

  def legalizeFormatA(bundle: TLBundleA, edge: TLEdge): Unit = {
    //switch this flag to turn on diplomacy in error messages
    def diplomacyInfo = if (true) "" else "\nThe diplomacy information for the edge is as follows:\n" + edge.formatEdge + "\n"
    monAssert (TLMessages.isA(bundle.opcode), "'A' channel has invalid opcode" + extra)

    // Reuse these subexpressions to save some firrtl lines
    val source_ok = edge.client.contains(bundle.source)
    val is_aligned = edge.isAligned(bundle.address, bundle.size)
    val mask = edge.full_mask(bundle)

    monAssert (visible(edge.address(bundle), bundle.source, edge), "'A' channel carries an address illegal for the specified bank visibility")

    //The monitor doesn’t check for acquire T vs acquire B, it assumes that acquire B implies acquire T and only checks for acquire B
    //TODO: check for acquireT?
    when (bundle.opcode === TLMessages.AcquireBlock) {
      monAssert (edge.expectsVipCheckerMasterToSlaveAcquireB(bundle.source, edge.address(bundle), bundle.size), "'A' channel carries AcquireBlock type which is unexpected using diplomatic parameters" + diplomacyInfo + extra)
      monAssert (edge.expectsVipCheckerSlaveToMasterProbe(edge.source(bundle), edge.address(bundle), bundle.size), "'A' channel carries AcquireBlock from a client which does not support Probe" + diplomacyInfo + extra)
      monAssert (source_ok, "'A' channel AcquireBlock carries invalid source ID" + diplomacyInfo + extra)
      monAssert (bundle.size >= log2Ceil(edge.manager.beatBytes).U, "'A' channel AcquireBlock smaller than a beat" + extra)
      monAssert (is_aligned, "'A' channel AcquireBlock address not aligned to size" + extra)
      monAssert (TLPermissions.isGrow(bundle.param), "'A' channel AcquireBlock carries invalid grow param" + extra)
      monAssert (~bundle.mask === 0.U, "'A' channel AcquireBlock contains invalid mask" + extra)
      monAssert (!bundle.corrupt, "'A' channel AcquireBlock is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.AcquirePerm) {
      monAssert (edge.expectsVipCheckerMasterToSlaveAcquireB(bundle.source, edge.address(bundle), bundle.size), "'A' channel carries AcquirePerm type which is unexpected using diplomatic parameters" + diplomacyInfo + extra)
      monAssert (edge.expectsVipCheckerSlaveToMasterProbe(edge.source(bundle), edge.address(bundle), bundle.size), "'A' channel carries AcquirePerm from a client which does not support Probe" + diplomacyInfo + extra)
      monAssert (source_ok, "'A' channel AcquirePerm carries invalid source ID" + diplomacyInfo + extra)
      monAssert (bundle.size >= log2Ceil(edge.manager.beatBytes).U, "'A' channel AcquirePerm smaller than a beat" + extra)
      monAssert (is_aligned, "'A' channel AcquirePerm address not aligned to size" + extra)
      monAssert (TLPermissions.isGrow(bundle.param), "'A' channel AcquirePerm carries invalid grow param" + extra)
      monAssert (bundle.param =/= TLPermissions.NtoB, "'A' channel AcquirePerm requests NtoB" + extra)
      monAssert (~bundle.mask === 0.U, "'A' channel AcquirePerm contains invalid mask" + extra)
      monAssert (!bundle.corrupt, "'A' channel AcquirePerm is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.Get) {
      monAssert (edge.master.expectsVipCheckerEmitsGet(bundle.source, bundle.size), "'A' channel carries Get type which master claims it can't emit" + diplomacyInfo + extra)
      monAssert (edge.slave.expectsVipCheckerSupportsGet(edge.address(bundle), bundle.size, None), "'A' channel carries Get type which slave claims it can't support" + diplomacyInfo + extra)
      monAssert (source_ok, "'A' channel Get carries invalid source ID" + diplomacyInfo + extra)
      monAssert (is_aligned, "'A' channel Get address not aligned to size" + extra)
      monAssert (bundle.param === 0.U, "'A' channel Get carries invalid param" + extra)
      monAssert (bundle.mask === mask, "'A' channel Get contains invalid mask" + extra)
      monAssert (!bundle.corrupt, "'A' channel Get is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.PutFullData) {
      monAssert (edge.expectsVipCheckerMasterToSlavePutFull(bundle.source, edge.address(bundle), bundle.size), "'A' channel carries PutFull type which is unexpected using diplomatic parameters" + diplomacyInfo + extra)
      monAssert (source_ok, "'A' channel PutFull carries invalid source ID" + diplomacyInfo + extra)
      monAssert (is_aligned, "'A' channel PutFull address not aligned to size" + extra)
      monAssert (bundle.param === 0.U, "'A' channel PutFull carries invalid param" + extra)
      monAssert (bundle.mask === mask, "'A' channel PutFull contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.PutPartialData) {
      monAssert (edge.expectsVipCheckerMasterToSlavePutPartial(bundle.source, edge.address(bundle), bundle.size), "'A' channel carries PutPartial type which is unexpected using diplomatic parameters" + extra)
      monAssert (source_ok, "'A' channel PutPartial carries invalid source ID" + diplomacyInfo + extra)
      monAssert (is_aligned, "'A' channel PutPartial address not aligned to size" + extra)
      monAssert (bundle.param === 0.U, "'A' channel PutPartial carries invalid param" + extra)
      monAssert ((bundle.mask & ~mask) === 0.U, "'A' channel PutPartial contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.ArithmeticData) {
      monAssert (edge.expectsVipCheckerMasterToSlaveArithmetic(bundle.source, edge.address(bundle), bundle.size), "'A' channel carries Arithmetic type which is unexpected using diplomatic parameters" + extra)
      monAssert (source_ok, "'A' channel Arithmetic carries invalid source ID" + diplomacyInfo + extra)
      monAssert (is_aligned, "'A' channel Arithmetic address not aligned to size" + extra)
      monAssert (TLAtomics.isArithmetic(bundle.param), "'A' channel Arithmetic carries invalid opcode param" + extra)
      monAssert (bundle.mask === mask, "'A' channel Arithmetic contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.LogicalData) {
      monAssert (edge.expectsVipCheckerMasterToSlaveLogical(bundle.source, edge.address(bundle), bundle.size), "'A' channel carries Logical type which is unexpected using diplomatic parameters" + extra)
      monAssert (source_ok, "'A' channel Logical carries invalid source ID" + diplomacyInfo + extra)
      monAssert (is_aligned, "'A' channel Logical address not aligned to size" + extra)
      monAssert (TLAtomics.isLogical(bundle.param), "'A' channel Logical carries invalid opcode param" + extra)
      monAssert (bundle.mask === mask, "'A' channel Logical contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.Hint) {
      monAssert (edge.master.expectsVipCheckerEmitsHint(bundle.source, bundle.size), "'A' channel carries Hint type which master claims it can't emit" + diplomacyInfo + extra)
      monAssert (edge.slave.expectsVipCheckerSupportsHint(edge.address(bundle), bundle.size, None), "'A' channel carries Hint type which slave claims it can't support" + diplomacyInfo + extra)
      monAssert (source_ok, "'A' channel Hint carries invalid source ID" + diplomacyInfo + extra)
      monAssert (is_aligned, "'A' channel Hint address not aligned to size" + extra)
      monAssert (TLHints.isHints(bundle.param), "'A' channel Hint carries invalid opcode param" + extra)
      monAssert (bundle.mask === mask, "'A' channel Hint contains invalid mask" + extra)
      monAssert (!bundle.corrupt, "'A' channel Hint is corrupt" + extra)
    }
  }

  def legalizeFormatB(bundle: TLBundleB, edge: TLEdge): Unit = {
    monAssert (TLMessages.isB(bundle.opcode), "'B' channel has invalid opcode" + extra)

    monAssert (visible(edge.address(bundle), bundle.source, edge), "'B' channel carries an address illegal for the specified bank visibility")

    // Reuse these subexpressions to save some firrtl lines
    val address_ok = edge.manager.containsSafe(edge.address(bundle))
    val is_aligned = edge.isAligned(bundle.address, bundle.size)
    val mask = edge.full_mask(bundle)
    val legal_source = Mux1H(edge.client.find(bundle.source), edge.client.clients.map(c => c.sourceId.start.U)) === bundle.source

    when (bundle.opcode === TLMessages.Probe) {
      assume (edge.expectsVipCheckerSlaveToMasterProbe(edge.source(bundle), edge.address(bundle), bundle.size), "'B' channel carries Probe type which is unexpected using diplomatic parameters" + extra)
      assume (address_ok, "'B' channel Probe carries unmanaged address" + extra)
      assume (legal_source, "'B' channel Probe carries source that is not first source" + extra)
      assume (is_aligned, "'B' channel Probe address not aligned to size" + extra)
      assume (TLPermissions.isCap(bundle.param), "'B' channel Probe carries invalid cap param" + extra)
      assume (bundle.mask === mask, "'B' channel Probe contains invalid mask" + extra)
      assume (!bundle.corrupt, "'B' channel Probe is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.Get) {
      monAssert (edge.expectsVipCheckerSlaveToMasterPutFull(edge.source(bundle), edge.address(bundle), bundle.size), "'B' channel carries Get type which is unexpected using diplomatic parameters" + extra)
      monAssert (address_ok, "'B' channel Get carries unmanaged address" + extra)
      monAssert (legal_source, "'B' channel Get carries source that is not first source" + extra)
      monAssert (is_aligned, "'B' channel Get address not aligned to size" + extra)
      monAssert (bundle.param === 0.U, "'B' channel Get carries invalid param" + extra)
      monAssert (bundle.mask === mask, "'B' channel Get contains invalid mask" + extra)
      monAssert (!bundle.corrupt, "'B' channel Get is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.PutFullData) {
      monAssert (edge.expectsVipCheckerSlaveToMasterPutFull(edge.source(bundle), edge.address(bundle), bundle.size), "'B' channel carries PutFull type which is unexpected using diplomatic parameters" + extra)
      monAssert (address_ok, "'B' channel PutFull carries unmanaged address" + extra)
      monAssert (legal_source, "'B' channel PutFull carries source that is not first source" + extra)
      monAssert (is_aligned, "'B' channel PutFull address not aligned to size" + extra)
      monAssert (bundle.param === 0.U, "'B' channel PutFull carries invalid param" + extra)
      monAssert (bundle.mask === mask, "'B' channel PutFull contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.PutPartialData) {
      monAssert (edge.expectsVipCheckerSlaveToMasterPutPartial(edge.source(bundle), edge.address(bundle), bundle.size), "'B' channel carries PutPartial type which is unexpected using diplomatic parameters" + extra)
      monAssert (address_ok, "'B' channel PutPartial carries unmanaged address" + extra)
      monAssert (legal_source, "'B' channel PutPartial carries source that is not first source" + extra)
      monAssert (is_aligned, "'B' channel PutPartial address not aligned to size" + extra)
      monAssert (bundle.param === 0.U, "'B' channel PutPartial carries invalid param" + extra)
      monAssert ((bundle.mask & ~mask) === 0.U, "'B' channel PutPartial contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.ArithmeticData) {
      monAssert (edge.expectsVipCheckerSlaveToMasterArithmetic(edge.source(bundle), edge.address(bundle), bundle.size), "'B' channel carries Arithmetic type unsupported by master" + extra)
      monAssert (address_ok, "'B' channel Arithmetic carries unmanaged address" + extra)
      monAssert (legal_source, "'B' channel Arithmetic carries source that is not first source" + extra)
      monAssert (is_aligned, "'B' channel Arithmetic address not aligned to size" + extra)
      monAssert (TLAtomics.isArithmetic(bundle.param), "'B' channel Arithmetic carries invalid opcode param" + extra)
      monAssert (bundle.mask === mask, "'B' channel Arithmetic contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.LogicalData) {
      monAssert (edge.expectsVipCheckerSlaveToMasterLogical(edge.source(bundle), edge.address(bundle), bundle.size), "'B' channel carries Logical type unsupported by client" + extra)
      monAssert (address_ok, "'B' channel Logical carries unmanaged address" + extra)
      monAssert (legal_source, "'B' channel Logical carries source that is not first source" + extra)
      monAssert (is_aligned, "'B' channel Logical address not aligned to size" + extra)
      monAssert (TLAtomics.isLogical(bundle.param), "'B' channel Logical carries invalid opcode param" + extra)
      monAssert (bundle.mask === mask, "'B' channel Logical contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.Hint) {
      monAssert (edge.expectsVipCheckerSlaveToMasterHint(edge.source(bundle), edge.address(bundle), bundle.size), "'B' channel carries Hint type unsupported by client" + extra)
      monAssert (address_ok, "'B' channel Hint carries unmanaged address" + extra)
      monAssert (legal_source, "'B' channel Hint carries source that is not first source" + extra)
      monAssert (is_aligned, "'B' channel Hint address not aligned to size" + extra)
      monAssert (bundle.mask === mask, "'B' channel Hint contains invalid mask" + extra)
      monAssert (!bundle.corrupt, "'B' channel Hint is corrupt" + extra)
    }
  }

  def legalizeFormatC(bundle: TLBundleC, edge: TLEdge): Unit = {
    monAssert (TLMessages.isC(bundle.opcode), "'C' channel has invalid opcode" + extra)

    val source_ok = edge.client.contains(bundle.source)
    val is_aligned = edge.isAligned(bundle.address, bundle.size)
    val address_ok = edge.manager.containsSafe(edge.address(bundle))

    monAssert (visible(edge.address(bundle), bundle.source, edge), "'C' channel carries an address illegal for the specified bank visibility")

    when (bundle.opcode === TLMessages.ProbeAck) {
      monAssert (address_ok, "'C' channel ProbeAck carries unmanaged address" + extra)
      monAssert (source_ok, "'C' channel ProbeAck carries invalid source ID" + extra)
      monAssert (bundle.size >= log2Ceil(edge.manager.beatBytes).U, "'C' channel ProbeAck smaller than a beat" + extra)
      monAssert (is_aligned, "'C' channel ProbeAck address not aligned to size" + extra)
      monAssert (TLPermissions.isReport(bundle.param), "'C' channel ProbeAck carries invalid report param" + extra)
      monAssert (!bundle.corrupt, "'C' channel ProbeAck is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.ProbeAckData) {
      monAssert (address_ok, "'C' channel ProbeAckData carries unmanaged address" + extra)
      monAssert (source_ok, "'C' channel ProbeAckData carries invalid source ID" + extra)
      monAssert (bundle.size >= log2Ceil(edge.manager.beatBytes).U, "'C' channel ProbeAckData smaller than a beat" + extra)
      monAssert (is_aligned, "'C' channel ProbeAckData address not aligned to size" + extra)
      monAssert (TLPermissions.isReport(bundle.param), "'C' channel ProbeAckData carries invalid report param" + extra)
    }

    when (bundle.opcode === TLMessages.Release) {
      monAssert (edge.expectsVipCheckerMasterToSlaveAcquireB(edge.source(bundle), edge.address(bundle), bundle.size), "'C' channel carries Release type unsupported by manager" + extra)
      monAssert (edge.expectsVipCheckerSlaveToMasterProbe(edge.source(bundle), edge.address(bundle), bundle.size), "'C' channel carries Release from a client which does not support Probe" + extra)
      monAssert (source_ok, "'C' channel Release carries invalid source ID" + extra)
      monAssert (bundle.size >= log2Ceil(edge.manager.beatBytes).U, "'C' channel Release smaller than a beat" + extra)
      monAssert (is_aligned, "'C' channel Release address not aligned to size" + extra)
      monAssert (TLPermissions.isReport(bundle.param), "'C' channel Release carries invalid report param" + extra)
      monAssert (!bundle.corrupt, "'C' channel Release is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.ReleaseData) {
      monAssert (edge.expectsVipCheckerMasterToSlaveAcquireB(edge.source(bundle), edge.address(bundle), bundle.size), "'C' channel carries ReleaseData type unsupported by manager" + extra)
      monAssert (edge.expectsVipCheckerSlaveToMasterProbe(edge.source(bundle), edge.address(bundle), bundle.size), "'C' channel carries Release from a client which does not support Probe" + extra)
      monAssert (source_ok, "'C' channel ReleaseData carries invalid source ID" + extra)
      monAssert (bundle.size >= log2Ceil(edge.manager.beatBytes).U, "'C' channel ReleaseData smaller than a beat" + extra)
      monAssert (is_aligned, "'C' channel ReleaseData address not aligned to size" + extra)
      monAssert (TLPermissions.isReport(bundle.param), "'C' channel ReleaseData carries invalid report param" + extra)
    }

    when (bundle.opcode === TLMessages.AccessAck) {
      monAssert (address_ok, "'C' channel AccessAck carries unmanaged address" + extra)
      monAssert (source_ok, "'C' channel AccessAck carries invalid source ID" + extra)
      monAssert (is_aligned, "'C' channel AccessAck address not aligned to size" + extra)
      monAssert (bundle.param === 0.U, "'C' channel AccessAck carries invalid param" + extra)
      monAssert (!bundle.corrupt, "'C' channel AccessAck is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.AccessAckData) {
      monAssert (address_ok, "'C' channel AccessAckData carries unmanaged address" + extra)
      monAssert (source_ok, "'C' channel AccessAckData carries invalid source ID" + extra)
      monAssert (is_aligned, "'C' channel AccessAckData address not aligned to size" + extra)
      monAssert (bundle.param === 0.U, "'C' channel AccessAckData carries invalid param" + extra)
    }

    when (bundle.opcode === TLMessages.HintAck) {
      monAssert (address_ok, "'C' channel HintAck carries unmanaged address" + extra)
      monAssert (source_ok, "'C' channel HintAck carries invalid source ID" + extra)
      monAssert (is_aligned, "'C' channel HintAck address not aligned to size" + extra)
      monAssert (bundle.param === 0.U, "'C' channel HintAck carries invalid param" + extra)
      monAssert (!bundle.corrupt, "'C' channel HintAck is corrupt" + extra)
    }
  }

  def legalizeFormatD(bundle: TLBundleD, edge: TLEdge): Unit = {
    assume (TLMessages.isD(bundle.opcode), "'D' channel has invalid opcode" + extra)

    val source_ok = edge.client.contains(bundle.source)
    val sink_ok = bundle.sink < edge.manager.endSinkId.U
    val deny_put_ok = edge.manager.mayDenyPut.B
    val deny_get_ok = edge.manager.mayDenyGet.B

    when (bundle.opcode === TLMessages.ReleaseAck) {
      assume (source_ok, "'D' channel ReleaseAck carries invalid source ID" + extra)
      assume (bundle.size >= log2Ceil(edge.manager.beatBytes).U, "'D' channel ReleaseAck smaller than a beat" + extra)
      assume (bundle.param === 0.U, "'D' channel ReleaseeAck carries invalid param" + extra)
      assume (!bundle.corrupt, "'D' channel ReleaseAck is corrupt" + extra)
      assume (!bundle.denied, "'D' channel ReleaseAck is denied" + extra)
    }

    when (bundle.opcode === TLMessages.Grant) {
      assume (source_ok, "'D' channel Grant carries invalid source ID" + extra)
      assume (sink_ok, "'D' channel Grant carries invalid sink ID" + extra)
      assume (bundle.size >= log2Ceil(edge.manager.beatBytes).U, "'D' channel Grant smaller than a beat" + extra)
      assume (TLPermissions.isCap(bundle.param), "'D' channel Grant carries invalid cap param" + extra)
      assume (bundle.param =/= TLPermissions.toN, "'D' channel Grant carries toN param" + extra)
      assume (!bundle.corrupt, "'D' channel Grant is corrupt" + extra)
      assume (deny_put_ok || !bundle.denied, "'D' channel Grant is denied" + extra)
    }

    when (bundle.opcode === TLMessages.GrantData) {
      assume (source_ok, "'D' channel GrantData carries invalid source ID" + extra)
      assume (sink_ok, "'D' channel GrantData carries invalid sink ID" + extra)
      assume (bundle.size >= log2Ceil(edge.manager.beatBytes).U, "'D' channel GrantData smaller than a beat" + extra)
      assume (TLPermissions.isCap(bundle.param), "'D' channel GrantData carries invalid cap param" + extra)
      assume (bundle.param =/= TLPermissions.toN, "'D' channel GrantData carries toN param" + extra)
      assume (!bundle.denied || bundle.corrupt, "'D' channel GrantData is denied but not corrupt" + extra)
      assume (deny_get_ok || !bundle.denied, "'D' channel GrantData is denied" + extra)
    }

    when (bundle.opcode === TLMessages.AccessAck) {
      assume (source_ok, "'D' channel AccessAck carries invalid source ID" + extra)
      // size is ignored
      assume (bundle.param === 0.U, "'D' channel AccessAck carries invalid param" + extra)
      assume (!bundle.corrupt, "'D' channel AccessAck is corrupt" + extra)
      assume (deny_put_ok || !bundle.denied, "'D' channel AccessAck is denied" + extra)
    }

    when (bundle.opcode === TLMessages.AccessAckData) {
      assume (source_ok, "'D' channel AccessAckData carries invalid source ID" + extra)
      // size is ignored
      assume (bundle.param === 0.U, "'D' channel AccessAckData carries invalid param" + extra)
      assume (!bundle.denied || bundle.corrupt, "'D' channel AccessAckData is denied but not corrupt" + extra)
      assume (deny_get_ok || !bundle.denied, "'D' channel AccessAckData is denied" + extra)
    }

    when (bundle.opcode === TLMessages.HintAck) {
      assume (source_ok, "'D' channel HintAck carries invalid source ID" + extra)
      // size is ignored
      assume (bundle.param === 0.U, "'D' channel HintAck carries invalid param" + extra)
      assume (!bundle.corrupt, "'D' channel HintAck is corrupt" + extra)
      assume (deny_put_ok || !bundle.denied, "'D' channel HintAck is denied" + extra)
    }
  }

  def legalizeFormatE(bundle: TLBundleE, edge: TLEdge): Unit = {
    val sink_ok = bundle.sink < edge.manager.endSinkId.U
    monAssert (sink_ok, "'E' channels carries invalid sink ID" + extra)
  }

  def legalizeFormat(bundle: TLBundle, edge: TLEdge) = {
    when (bundle.a.valid) { legalizeFormatA(bundle.a.bits, edge) }
    when (bundle.d.valid) { legalizeFormatD(bundle.d.bits, edge) }
    if (edge.client.anySupportProbe && edge.manager.anySupportAcquireB) {
      when (bundle.b.valid) { legalizeFormatB(bundle.b.bits, edge) }
      when (bundle.c.valid) { legalizeFormatC(bundle.c.bits, edge) }
      when (bundle.e.valid) { legalizeFormatE(bundle.e.bits, edge) }
    } else {
      monAssert (!bundle.b.valid, "'B' channel valid and not TL-C" + extra)
      monAssert (!bundle.c.valid, "'C' channel valid and not TL-C" + extra)
      monAssert (!bundle.e.valid, "'E' channel valid and not TL-C" + extra)
    }
  }

  def legalizeMultibeatA(a: DecoupledIO[TLBundleA], edge: TLEdge): Unit = {
    val a_first = edge.first(a.bits, a.fire())
    val opcode  = Reg(UInt())
    val param   = Reg(UInt())
    val size    = Reg(UInt())
    val source  = Reg(UInt())
    val address = Reg(UInt())
    when (a.valid && !a_first) {
      monAssert (a.bits.opcode === opcode, "'A' channel opcode changed within multibeat operation" + extra)
      monAssert (a.bits.param  === param,  "'A' channel param changed within multibeat operation" + extra)
      monAssert (a.bits.size   === size,   "'A' channel size changed within multibeat operation" + extra)
      monAssert (a.bits.source === source, "'A' channel source changed within multibeat operation" + extra)
      monAssert (a.bits.address=== address,"'A' channel address changed with multibeat operation" + extra)
    }
    when (a.fire() && a_first) {
      opcode  := a.bits.opcode
      param   := a.bits.param
      size    := a.bits.size
      source  := a.bits.source
      address := a.bits.address
    }
  }

  def legalizeMultibeatB(b: DecoupledIO[TLBundleB], edge: TLEdge): Unit = {
    val b_first = edge.first(b.bits, b.fire())
    val opcode  = Reg(UInt())
    val param   = Reg(UInt())
    val size    = Reg(UInt())
    val source  = Reg(UInt())
    val address = Reg(UInt())
    when (b.valid && !b_first) {
      monAssert (b.bits.opcode === opcode, "'B' channel opcode changed within multibeat operation" + extra)
      monAssert (b.bits.param  === param,  "'B' channel param changed within multibeat operation" + extra)
      monAssert (b.bits.size   === size,   "'B' channel size changed within multibeat operation" + extra)
      monAssert (b.bits.source === source, "'B' channel source changed within multibeat operation" + extra)
      monAssert (b.bits.address=== address,"'B' channel addresss changed with multibeat operation" + extra)
    }
    when (b.fire() && b_first) {
      opcode  := b.bits.opcode
      param   := b.bits.param
      size    := b.bits.size
      source  := b.bits.source
      address := b.bits.address
    }
  }

  def legalizeADSourceFormal(bundle: TLBundle, edge: TLEdge): Unit = {
    // Symbolic variable
    val sym_source = Wire(UInt(edge.client.endSourceId.W))
    // TODO: Connect sym_source to a fixed value for simulation and to a
    //       free wire in formal
    sym_source := 0.U

    // Type casting Int to UInt
    val maxSourceId = Wire(UInt(edge.client.endSourceId.W))
    maxSourceId := edge.client.endSourceId.U

    // Delayed verison of sym_source
    val sym_source_d = Reg(UInt(edge.client.endSourceId.W))
    sym_source_d := sym_source

    // These will be constraints for FV setup
    Property(
        MonitorDirection.Monitor,
        (sym_source === sym_source_d),
        "sym_source should remain stable",
        PropertyClass.Default)
    Property(
        MonitorDirection.Monitor,
        (sym_source <= maxSourceId),
        "sym_source should take legal value",
        PropertyClass.Default)

    val my_resp_pend = RegInit(false.B)
    val my_opcode    = Reg(UInt())
    val my_size      = Reg(UInt())

    val a_first = bundle.a.valid && edge.first(bundle.a.bits, bundle.a.fire())
    val d_first = bundle.d.valid && edge.first(bundle.d.bits, bundle.d.fire())

    val my_a_first_beat = a_first && (bundle.a.bits.source === sym_source)
    val my_d_first_beat = d_first && (bundle.d.bits.source === sym_source)

    val my_clr_resp_pend = (bundle.d.fire() && my_d_first_beat)
    val my_set_resp_pend = (bundle.a.fire() && my_a_first_beat && !my_clr_resp_pend)
    when (my_set_resp_pend) {
      my_resp_pend := true.B
    } .elsewhen (my_clr_resp_pend) {
      my_resp_pend := false.B
    }

    when (my_a_first_beat) {
      my_opcode := bundle.a.bits.opcode
      my_size   := bundle.a.bits.size
    }

    val my_resp_size   = Mux(my_a_first_beat, bundle.a.bits.size, my_size)
    val my_resp_opcode = Mux(my_a_first_beat, bundle.a.bits.opcode, my_opcode)

    val my_resp_opcode_legal = Wire(Bool())
    when ((my_resp_opcode === TLMessages.Get) || (my_resp_opcode === TLMessages.ArithmeticData) ||
          (my_resp_opcode === TLMessages.LogicalData)) {
      my_resp_opcode_legal := (bundle.d.bits.opcode === TLMessages.AccessAckData)
    } .elsewhen ((my_resp_opcode === TLMessages.PutFullData) || (my_resp_opcode === TLMessages.PutPartialData)) {
      my_resp_opcode_legal := (bundle.d.bits.opcode === TLMessages.AccessAck)
    } .otherwise {
      my_resp_opcode_legal := (bundle.d.bits.opcode === TLMessages.HintAck)
    }

    monAssert (IfThen(my_resp_pend, !my_a_first_beat),
               "Request message should not be sent with a source ID, for which a response message" +
               "is already pending (not received until current cycle) for a prior request message" +
               "with the same source ID" + extra)

    assume (IfThen(my_clr_resp_pend, (my_set_resp_pend || my_resp_pend)),
            "Response message should be accepted with a source ID only if a request message with the" +
            "same source ID has been accepted or is being accepted in the current cycle" + extra)
    assume (IfThen(my_d_first_beat, (my_a_first_beat || my_resp_pend)),
            "Response message should be sent with a source ID only if a request message with the" +
            "same source ID has been accepted or is being sent in the current cycle" + extra)
    assume (IfThen(my_d_first_beat, (bundle.d.bits.size === my_resp_size)),
            "If d_valid is 1, then d_size should be same as a_size of the corresponding request" +
            "message" + extra)
    assume (IfThen(my_d_first_beat, my_resp_opcode_legal),
            "If d_valid is 1, then d_opcode should correspond with a_opcode of the corresponding" +
            "request message" + extra)
  }

  def legalizeMultibeatC(c: DecoupledIO[TLBundleC], edge: TLEdge): Unit = {
    val c_first = edge.first(c.bits, c.fire())
    val opcode  = Reg(UInt())
    val param   = Reg(UInt())
    val size    = Reg(UInt())
    val source  = Reg(UInt())
    val address = Reg(UInt())
    when (c.valid && !c_first) {
      monAssert (c.bits.opcode === opcode, "'C' channel opcode changed within multibeat operation" + extra)
      monAssert (c.bits.param  === param,  "'C' channel param changed within multibeat operation" + extra)
      monAssert (c.bits.size   === size,   "'C' channel size changed within multibeat operation" + extra)
      monAssert (c.bits.source === source, "'C' channel source changed within multibeat operation" + extra)
      monAssert (c.bits.address=== address,"'C' channel address changed with multibeat operation" + extra)
    }
    when (c.fire() && c_first) {
      opcode  := c.bits.opcode
      param   := c.bits.param
      size    := c.bits.size
      source  := c.bits.source
      address := c.bits.address
    }
  }

  def legalizeMultibeatD(d: DecoupledIO[TLBundleD], edge: TLEdge): Unit = {
    val d_first = edge.first(d.bits, d.fire())
    val opcode  = Reg(UInt())
    val param   = Reg(UInt())
    val size    = Reg(UInt())
    val source  = Reg(UInt())
    val sink    = Reg(UInt())
    val denied  = Reg(Bool())
    when (d.valid && !d_first) {
      assume (d.bits.opcode === opcode, "'D' channel opcode changed within multibeat operation" + extra)
      assume (d.bits.param  === param,  "'D' channel param changed within multibeat operation" + extra)
      assume (d.bits.size   === size,   "'D' channel size changed within multibeat operation" + extra)
      assume (d.bits.source === source, "'D' channel source changed within multibeat operation" + extra)
      assume (d.bits.sink   === sink,   "'D' channel sink changed with multibeat operation" + extra)
      assume (d.bits.denied === denied, "'D' channel denied changed with multibeat operation" + extra)
    }
    when (d.fire() && d_first) {
      opcode  := d.bits.opcode
      param   := d.bits.param
      size    := d.bits.size
      source  := d.bits.source
      sink    := d.bits.sink
      denied  := d.bits.denied
    }
  }

  def legalizeMultibeat(bundle: TLBundle, edge: TLEdge): Unit = {
    legalizeMultibeatA(bundle.a, edge)
    legalizeMultibeatD(bundle.d, edge)
    if (edge.client.anySupportProbe && edge.manager.anySupportAcquireB) {
      legalizeMultibeatB(bundle.b, edge)
      legalizeMultibeatC(bundle.c, edge)
    }
  }

  //This is left in for almond which doesn't adhere to the tilelink protocol
  @deprecated("Use legalizeADSource instead if possible","")
  def legalizeADSourceOld(bundle: TLBundle, edge: TLEdge): Unit = {
    val inflight = RegInit(0.U(edge.client.endSourceId.W))

    val a_first = edge.first(bundle.a.bits, bundle.a.fire())
    val d_first = edge.first(bundle.d.bits, bundle.d.fire())

    val a_set = WireInit(0.U(edge.client.endSourceId.W))
    when (bundle.a.fire() && a_first && edge.isRequest(bundle.a.bits)) {
      a_set := UIntToOH(bundle.a.bits.source)
      assert(!inflight(bundle.a.bits.source), "'A' channel re-used a source ID" + extra)
    }

    val d_clr = WireInit(0.U(edge.client.endSourceId.W))
    val d_release_ack = bundle.d.bits.opcode === TLMessages.ReleaseAck
    when (bundle.d.fire() && d_first && edge.isResponse(bundle.d.bits) && !d_release_ack) {
      d_clr := UIntToOH(bundle.d.bits.source)
      assume((a_set | inflight)(bundle.d.bits.source), "'D' channel acknowledged for nothing inflight" + extra)
    }

    if (edge.manager.minLatency > 0) {
      assume(a_set =/= d_clr || !a_set.orR, s"'A' and 'D' concurrent, despite minlatency ${edge.manager.minLatency}" + extra)
    }

    inflight := (inflight | a_set) & ~d_clr

    val watchdog = RegInit(0.U(32.W))
    val limit = PlusArg("tilelink_timeout",
      docstring="Kill emulation after INT waiting TileLink cycles. Off if 0.")
    assert (!inflight.orR || limit === 0.U || watchdog < limit, "TileLink timeout expired" + extra)

    watchdog := watchdog + 1.U
    when (bundle.a.fire() || bundle.d.fire()) { watchdog := 0.U }
  }

  def legalizeADSource(bundle: TLBundle, edge: TLEdge): Unit = {
    val a_size_bus_size       = edge.bundle.sizeBits + 1 //add one so that 0 is not mapped to anything (size 0 -> size 1 in map, size 0 in map means unset)
    val a_opcode_bus_size     = 3 + 1 //opcode size is 3, but add so that 0 is not mapped to anything
    val log_a_opcode_bus_size = log2Ceil(a_opcode_bus_size)
    val log_a_size_bus_size   = log2Ceil(a_size_bus_size)
    def size_to_numfullbits(x: UInt): UInt = (1.U << x) - 1.U //convert a number to that many full bits

    val inflight = RegInit(0.U(edge.client.endSourceId.W))
    inflight.suggestName("inflight")
    val inflight_opcodes = RegInit(0.U((edge.client.endSourceId << log_a_opcode_bus_size).W))
    inflight_opcodes.suggestName("inflight_opcodes")
    val inflight_sizes = RegInit(0.U((edge.client.endSourceId << log_a_size_bus_size).W))
    inflight_sizes.suggestName("inflight_sizes")

    val a_first = edge.first(bundle.a.bits, bundle.a.fire())
    a_first.suggestName("a_first")
    val d_first = edge.first(bundle.d.bits, bundle.d.fire())
    d_first.suggestName("d_first")

    val a_set          = WireInit(0.U(edge.client.endSourceId.W))
    val a_set_wo_ready = WireInit(0.U(edge.client.endSourceId.W))
    a_set.suggestName("a_set")
    a_set_wo_ready.suggestName("a_set_wo_ready")
    val a_opcodes_set = WireInit(0.U((edge.client.endSourceId << log_a_opcode_bus_size).W))
    a_opcodes_set.suggestName("a_opcodes_set")
    val a_sizes_set = WireInit(0.U((edge.client.endSourceId << log_a_size_bus_size).W))
    a_sizes_set.suggestName("a_sizes_set")

    val a_opcode_lookup = WireInit(0.U((1 << log_a_opcode_bus_size).W))
    a_opcode_lookup.suggestName("a_opcode_lookup")
    a_opcode_lookup := ((inflight_opcodes) >> (bundle.d.bits.source << log_a_opcode_bus_size.U) & size_to_numfullbits(1.U << log_a_opcode_bus_size.U)) >> 1.U

    val a_size_lookup = WireInit(0.U((1 << log_a_size_bus_size).W))
    a_size_lookup.suggestName("a_size_lookup")
    a_size_lookup := ((inflight_sizes) >> (bundle.d.bits.source << log_a_size_bus_size.U) & size_to_numfullbits(1.U << log_a_size_bus_size.U)) >> 1.U

    val responseMap             = VecInit(Seq(TLMessages.AccessAck, TLMessages.AccessAck, TLMessages.AccessAckData, TLMessages.AccessAckData, TLMessages.AccessAckData, TLMessages.HintAck, TLMessages.Grant,     TLMessages.Grant))
    val responseMapSecondOption = VecInit(Seq(TLMessages.AccessAck, TLMessages.AccessAck, TLMessages.AccessAckData, TLMessages.AccessAckData, TLMessages.AccessAckData, TLMessages.HintAck, TLMessages.GrantData, TLMessages.Grant))

    val a_opcodes_set_interm = WireInit(0.U(a_opcode_bus_size.W))
    a_opcodes_set_interm.suggestName("a_opcodes_set_interm")
    val a_sizes_set_interm = WireInit(0.U(a_size_bus_size.W))
    a_sizes_set_interm.suggestName("a_sizes_set_interm")

    when (bundle.a.valid && a_first && edge.isRequest(bundle.a.bits)) {
      a_set_wo_ready := UIntToOH(bundle.a.bits.source)
    }

    when (bundle.a.fire() && a_first && edge.isRequest(bundle.a.bits)) {
      a_set                := UIntToOH(bundle.a.bits.source)
      a_opcodes_set_interm := (bundle.a.bits.opcode << 1.U) | 1.U
      a_sizes_set_interm   := (bundle.a.bits.size << 1.U) | 1.U
      a_opcodes_set        := (a_opcodes_set_interm) << (bundle.a.bits.source << log_a_opcode_bus_size.U)
      a_sizes_set          := (a_sizes_set_interm) << (bundle.a.bits.source << log_a_size_bus_size.U)
      monAssert(!inflight(bundle.a.bits.source), "'A' channel re-used a source ID" + extra)
    }

    val d_clr          = WireInit(0.U(edge.client.endSourceId.W))
    val d_clr_wo_ready = WireInit(0.U(edge.client.endSourceId.W))
    d_clr.suggestName("d_clr")
    d_clr_wo_ready.suggestName("d_clr_wo_ready")
    val d_opcodes_clr = WireInit(0.U((edge.client.endSourceId << log_a_opcode_bus_size).W))
    d_opcodes_clr.suggestName("d_opcodes_clr")
    val d_sizes_clr = WireInit(0.U((edge.client.endSourceId << log_a_size_bus_size).W))
    d_sizes_clr.suggestName("d_sizes_clr")

    val d_release_ack = bundle.d.bits.opcode === TLMessages.ReleaseAck
    when (bundle.d.valid && d_first && edge.isResponse(bundle.d.bits) && !d_release_ack) {
      d_clr_wo_ready := UIntToOH(bundle.d.bits.source)
    }

    when (bundle.d.fire() && d_first && edge.isResponse(bundle.d.bits) && !d_release_ack) {
      d_clr         := UIntToOH(bundle.d.bits.source)
      d_opcodes_clr := size_to_numfullbits(1.U << log_a_opcode_bus_size.U) << (bundle.d.bits.source << log_a_opcode_bus_size.U)
      d_sizes_clr   := size_to_numfullbits(1.U << log_a_size_bus_size.U) << (bundle.d.bits.source << log_a_size_bus_size.U)
    }
    when (bundle.d.valid && d_first && edge.isResponse(bundle.d.bits) && !d_release_ack) {
      val same_cycle_resp = bundle.a.valid && a_first && edge.isRequest(bundle.a.bits) && (bundle.a.bits.source === bundle.d.bits.source)
      assume(((inflight)(bundle.d.bits.source)) || same_cycle_resp, "'D' channel acknowledged for nothing inflight" + extra)

      when (same_cycle_resp) {
        assume((bundle.d.bits.opcode === responseMap(bundle.a.bits.opcode)) ||
                (bundle.d.bits.opcode === responseMapSecondOption(bundle.a.bits.opcode)), "'D' channel contains improper opcode response" + extra)
        assume((bundle.a.bits.size === bundle.d.bits.size), "'D' channel contains improper response size" + extra)
      } .otherwise {
        assume((bundle.d.bits.opcode === responseMap(a_opcode_lookup)) ||
               (bundle.d.bits.opcode === responseMapSecondOption(a_opcode_lookup)), "'D' channel contains improper opcode response" + extra)
        assume((bundle.d.bits.size === a_size_lookup), "'D' channel contains improper response size" + extra)
      }
    }
    when(bundle.d.valid && d_first && a_first && bundle.a.valid && (bundle.a.bits.source === bundle.d.bits.source) && !d_release_ack) {
      assume((!bundle.d.ready) || bundle.a.ready, "ready check")
    }

    if (edge.manager.minLatency > 0) {
      assume(a_set_wo_ready =/= d_clr_wo_ready || !a_set_wo_ready.orR, s"'A' and 'D' concurrent, despite minlatency ${edge.manager.minLatency}" + extra)
    }

    inflight := (inflight | a_set) & ~d_clr
    inflight_opcodes := (inflight_opcodes | a_opcodes_set) & ~d_opcodes_clr
    inflight_sizes := (inflight_sizes | a_sizes_set) & ~d_sizes_clr

    val watchdog = RegInit(0.U(32.W))
    val limit = PlusArg("tilelink_timeout",
      docstring="Kill emulation after INT waiting TileLink cycles. Off if 0.")
    monAssert (!inflight.orR || limit === 0.U || watchdog < limit, "TileLink timeout expired" + extra)

    watchdog := watchdog + 1.U
    when (bundle.a.fire() || bundle.d.fire()) { watchdog := 0.U }
  }

  def legalizeCDSource(bundle: TLBundle, edge: TLEdge): Unit = {
    val c_size_bus_size   = edge.bundle.sizeBits + 1 //add one so that 0 is not mapped to anything (size 0 -> size 1 in map, size 0 in map means unset)
    val c_opcode_bus_size = 3 + 1                    //opcode size is 3, but add so that 0 is not mapped to anything

    val log_c_opcode_bus_size = log2Ceil(c_opcode_bus_size)
    val log_c_size_bus_size   = log2Ceil(c_size_bus_size)
    def size_to_numfullbits(x: UInt): UInt = (1.U << x) - 1.U //convert a number to that many full bits

    val inflight         = RegInit(0.U(edge.client.endSourceId.W))
    val inflight_opcodes = RegInit(0.U((edge.client.endSourceId << log_c_opcode_bus_size).W))
    val inflight_sizes   = RegInit(0.U((edge.client.endSourceId << log_c_size_bus_size).W))
    inflight.suggestName("inflight")
    inflight_opcodes.suggestName("inflight_opcodes")
    inflight_sizes.suggestName("inflight_sizes")

    val c_first = edge.first(bundle.c.bits, bundle.c.fire())
    val d_first = edge.first(bundle.d.bits, bundle.d.fire())
    c_first.suggestName("c_first")
    d_first.suggestName("d_first")

    val c_set          = WireInit(0.U(edge.client.endSourceId.W))
    val c_set_wo_ready = WireInit(0.U(edge.client.endSourceId.W))
    val c_opcodes_set  = WireInit(0.U((edge.client.endSourceId << log_c_opcode_bus_size).W))
    val c_sizes_set    = WireInit(0.U((edge.client.endSourceId << log_c_size_bus_size).W))
    c_set.suggestName("c_set")
    c_set_wo_ready.suggestName("c_set_wo_ready")
    c_opcodes_set.suggestName("c_opcodes_set")
    c_sizes_set.suggestName("c_sizes_set")

    val c_opcode_lookup = WireInit(0.U((1 << log_c_opcode_bus_size).W))
    val c_size_lookup   = WireInit(0.U((1 << log_c_size_bus_size).W))
    c_opcode_lookup := ((inflight_opcodes) >> (bundle.d.bits.source << log_c_opcode_bus_size.U) & size_to_numfullbits(1.U << log_c_opcode_bus_size.U)) >> 1.U
    c_size_lookup   := ((inflight_sizes) >> (bundle.d.bits.source << log_c_size_bus_size.U) & size_to_numfullbits(1.U << log_c_size_bus_size.U)) >> 1.U
    c_opcode_lookup.suggestName("c_opcode_lookup")
    c_size_lookup.suggestName("c_size_lookup")

    val c_opcodes_set_interm = WireInit(0.U(c_opcode_bus_size.W))
    val c_sizes_set_interm   = WireInit(0.U(c_size_bus_size.W))
    c_opcodes_set_interm.suggestName("c_opcodes_set_interm")
    c_sizes_set_interm.suggestName("c_sizes_set_interm")

    when (bundle.c.valid && c_first && edge.isRequest(bundle.c.bits)) {
      c_set_wo_ready := UIntToOH(bundle.c.bits.source)
    }

    when (bundle.c.fire() && c_first && edge.isRequest(bundle.c.bits)) {
      c_set                := UIntToOH(bundle.c.bits.source)
      c_opcodes_set_interm := (bundle.c.bits.opcode << 1.U) | 1.U
      c_sizes_set_interm   := (bundle.c.bits.size << 1.U) | 1.U
      c_opcodes_set        := (c_opcodes_set_interm) << (bundle.c.bits.source << log_c_opcode_bus_size.U)
      c_sizes_set          := (c_sizes_set_interm) << (bundle.c.bits.source << log_c_size_bus_size.U)
      monAssert(!inflight(bundle.c.bits.source), "'C' channel re-used a source ID" + extra)
    }

    val d_clr          = WireInit(0.U(edge.client.endSourceId.W))
    val d_clr_wo_ready = WireInit(0.U(edge.client.endSourceId.W))
    val d_opcodes_clr  = WireInit(0.U((edge.client.endSourceId << log_c_opcode_bus_size).W))
    val d_sizes_clr    = WireInit(0.U((edge.client.endSourceId << log_c_size_bus_size).W))
    d_clr.suggestName("d_clr")
    d_clr_wo_ready.suggestName("d_clr_wo_ready")
    d_opcodes_clr.suggestName("d_opcodes_clr")
    d_sizes_clr.suggestName("d_sizes_clr")

    val d_release_ack = bundle.d.bits.opcode === TLMessages.ReleaseAck
    when (bundle.d.valid && d_first && edge.isResponse(bundle.d.bits) && d_release_ack) {
      d_clr_wo_ready := UIntToOH(bundle.d.bits.source)
    }

    when (bundle.d.fire() && d_first && edge.isResponse(bundle.d.bits) && d_release_ack) {
      d_clr         := UIntToOH(bundle.d.bits.source)
      d_opcodes_clr := size_to_numfullbits(1.U << log_c_opcode_bus_size.U) << (bundle.d.bits.source << log_c_opcode_bus_size.U)
      d_sizes_clr   := size_to_numfullbits(1.U << log_c_size_bus_size.U) << (bundle.d.bits.source << log_c_size_bus_size.U)
    }

    when (bundle.d.valid && d_first && edge.isResponse(bundle.d.bits) && d_release_ack) {
      val same_cycle_resp = bundle.c.valid && c_first && edge.isRequest(bundle.c.bits) && (bundle.c.bits.source === bundle.d.bits.source)
      assume(((inflight)(bundle.d.bits.source)) || same_cycle_resp, "'D' channel acknowledged for nothing inflight" + extra)
      when (same_cycle_resp) {
        assume((bundle.d.bits.size === bundle.c.bits.size), "'D' channel contains improper response size" + extra)
      } .otherwise {
        assume((bundle.d.bits.size === c_size_lookup), "'D' channel contains improper response size" + extra)
      }
    }

    when(bundle.d.valid && d_first && c_first && bundle.c.valid && (bundle.c.bits.source === bundle.d.bits.source) && d_release_ack) {
      assume((!bundle.d.ready) || bundle.c.ready, "ready check")
    }

    if (edge.manager.minLatency > 0) {
      when (c_set_wo_ready.orR) {
        assume(c_set_wo_ready =/= d_clr_wo_ready, s"'C' and 'D' concurrent, despite minlatency ${edge.manager.minLatency}" + extra)
      }
    }

    inflight         := (inflight | c_set) & ~d_clr
    inflight_opcodes := (inflight_opcodes | c_opcodes_set) & ~d_opcodes_clr
    inflight_sizes   := (inflight_sizes | c_sizes_set) & ~d_sizes_clr

    val watchdog = RegInit(0.U(32.W))
    val limit = PlusArg("tilelink_timeout",
      docstring="Kill emulation after INT waiting TileLink cycles. Off if 0.")
    monAssert (!inflight.orR || limit === 0.U || watchdog < limit, "TileLink timeout expired" + extra)

    watchdog := watchdog + 1.U
    when (bundle.c.fire() || bundle.d.fire()) { watchdog := 0.U }
  }

  def legalizeDESink(bundle: TLBundle, edge: TLEdge): Unit = {
    val inflight = RegInit(0.U(edge.manager.endSinkId.W))

    val d_first = edge.first(bundle.d.bits, bundle.d.fire())
    val e_first = true.B

    val d_set = WireInit(0.U(edge.manager.endSinkId.W))
    when (bundle.d.fire() && d_first && edge.isRequest(bundle.d.bits)) {
      d_set := UIntToOH(bundle.d.bits.sink)
      assume(!inflight(bundle.d.bits.sink), "'D' channel re-used a sink ID" + extra)
    }

    val e_clr = WireInit(0.U(edge.manager.endSinkId.W))
    when (bundle.e.fire() && e_first && edge.isResponse(bundle.e.bits)) {
      e_clr := UIntToOH(bundle.e.bits.sink)
      monAssert((d_set | inflight)(bundle.e.bits.sink), "'E' channel acknowledged for nothing inflight" + extra)
    }

    // edge.client.minLatency applies to BC, not DE

    inflight := (inflight | d_set) & ~e_clr
  }

  def legalizeUnique(bundle: TLBundle, edge: TLEdge): Unit = {
    val sourceBits = log2Ceil(edge.client.endSourceId)
    val tooBig = 14 // >16kB worth of flight information gets to be too much
    if (sourceBits > tooBig) {
      println(s"WARNING: TLMonitor instantiated on a bus with source bits (${sourceBits}) > ${tooBig}; A=>D transaction flight will not be checked")
    } else {
      if (args.edge.params(TestplanTestType).simulation) {
        if (args.edge.params(TLMonitorStrictMode)) {
          legalizeADSource(bundle, edge)
          legalizeCDSource(bundle, edge)
        } else {
          legalizeADSourceOld(bundle, edge)
        }
      }
      if (args.edge.params(TestplanTestType).formal) {
        legalizeADSourceFormal(bundle, edge)
      }
    }
    if (edge.client.anySupportProbe && edge.manager.anySupportAcquireB) {
      // legalizeBCSourceAddress(bundle, edge) // too much state needed to synthesize...
      val sinkBits = log2Ceil(edge.manager.endSinkId)
      if (sinkBits > tooBig) {
        println(s"WARNING: TLMonitor instantiated on a bus with sink bits (${sinkBits}) > ${tooBig}; D=>E transaction flight will not be checked")
      } else {
        legalizeDESink(bundle, edge)
      }
    }
  }

  def legalize(bundle: TLBundle, edge: TLEdge, reset: Reset): Unit = {
    legalizeFormat    (bundle, edge)
    legalizeMultibeat (bundle, edge)
    legalizeUnique    (bundle, edge)
  }
}
