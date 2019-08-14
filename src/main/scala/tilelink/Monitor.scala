// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.{SourceInfo, SourceLine}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.{HeterogeneousBag, PlusArg}

case class TLMonitorArgs(edge: TLEdge)

abstract class TLMonitorBase(args: TLMonitorArgs) extends Module
{
  val io = new Bundle {
    val in = new TLBundleSnoop(args.edge.bundle).flip
  }

  def legalize(bundle: TLBundleSnoop, edge: TLEdge, reset: Bool): Unit
  legalize(io.in, args.edge, reset)
}

class TLMonitor(args: TLMonitorArgs) extends TLMonitorBase(args)
{
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

  def legalizeFormatA(bundle: TLBundleA, edge: TLEdge) {
    assert (TLMessages.isA(bundle.opcode), "'A' channel has invalid opcode" + extra)

    // Reuse these subexpressions to save some firrtl lines
    val source_ok = edge.client.contains(bundle.source)
    val is_aligned = edge.isAligned(bundle.address, bundle.size)
    val mask = edge.full_mask(bundle)

    assert (visible(edge.address(bundle), bundle.source, edge), "'A' channel carries an address illegal for the specified bank visibility")

    when (bundle.opcode === TLMessages.AcquireBlock) {
      assert (edge.manager.supportsAcquireBSafe(edge.address(bundle), bundle.size), "'A' channel carries AcquireBlock type unsupported by manager" + extra)
      assert (edge.client.supportsProbe(edge.source(bundle), bundle.size), "'A' channel carries AcquireBlock from a client which does not support Probe" + extra)
      assert (source_ok, "'A' channel AcquireBlock carries invalid source ID" + extra)
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'A' channel AcquireBlock smaller than a beat" + extra)
      assert (is_aligned, "'A' channel AcquireBlock address not aligned to size" + extra)
      assert (TLPermissions.isGrow(bundle.param), "'A' channel AcquireBlock carries invalid grow param" + extra)
      assert (~bundle.mask === UInt(0), "'A' channel AcquireBlock contains invalid mask" + extra)
      assert (!bundle.corrupt, "'A' channel AcquireBlock is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.AcquirePerm) {
      assert (edge.manager.supportsAcquireBSafe(edge.address(bundle), bundle.size), "'A' channel carries AcquirePerm type unsupported by manager" + extra)
      assert (edge.client.supportsProbe(edge.source(bundle), bundle.size), "'A' channel carries AcquirePerm from a client which does not support Probe" + extra)
      assert (source_ok, "'A' channel AcquirePerm carries invalid source ID" + extra)
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'A' channel AcquirePerm smaller than a beat" + extra)
      assert (is_aligned, "'A' channel AcquirePerm address not aligned to size" + extra)
      assert (TLPermissions.isGrow(bundle.param), "'A' channel AcquirePerm carries invalid grow param" + extra)
      assert (bundle.param =/= TLPermissions.NtoB, "'A' channel AcquirePerm requests NtoB" + extra)
      assert (~bundle.mask === UInt(0), "'A' channel AcquirePerm contains invalid mask" + extra)
      assert (!bundle.corrupt, "'A' channel AcquirePerm is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.Get) {
      assert (edge.manager.supportsGetSafe(edge.address(bundle), bundle.size), "'A' channel carries Get type unsupported by manager" + extra)
      assert (source_ok, "'A' channel Get carries invalid source ID" + extra)
      assert (is_aligned, "'A' channel Get address not aligned to size" + extra)
      assert (bundle.param === UInt(0), "'A' channel Get carries invalid param" + extra)
      assert (bundle.mask === mask, "'A' channel Get contains invalid mask" + extra)
      assert (!bundle.corrupt, "'A' channel Get is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.PutFullData) {
      assert (edge.manager.supportsPutFullSafe(edge.address(bundle), bundle.size), "'A' channel carries PutFull type unsupported by manager" + extra)
      assert (source_ok, "'A' channel PutFull carries invalid source ID" + extra)
      assert (is_aligned, "'A' channel PutFull address not aligned to size" + extra)
      assert (bundle.param === UInt(0), "'A' channel PutFull carries invalid param" + extra)
      assert (bundle.mask === mask, "'A' channel PutFull contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.PutPartialData) {
      assert (edge.manager.supportsPutPartialSafe(edge.address(bundle), bundle.size), "'A' channel carries PutPartial type unsupported by manager" + extra)
      assert (source_ok, "'A' channel PutPartial carries invalid source ID" + extra)
      assert (is_aligned, "'A' channel PutPartial address not aligned to size" + extra)
      assert (bundle.param === UInt(0), "'A' channel PutPartial carries invalid param" + extra)
      assert ((bundle.mask & ~mask) === UInt(0), "'A' channel PutPartial contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.ArithmeticData) {
      assert (edge.manager.supportsArithmeticSafe(edge.address(bundle), bundle.size), "'A' channel carries Arithmetic type unsupported by manager" + extra)
      assert (source_ok, "'A' channel Arithmetic carries invalid source ID" + extra)
      assert (is_aligned, "'A' channel Arithmetic address not aligned to size" + extra)
      assert (TLAtomics.isArithmetic(bundle.param), "'A' channel Arithmetic carries invalid opcode param" + extra)
      assert (bundle.mask === mask, "'A' channel Arithmetic contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.LogicalData) {
      assert (edge.manager.supportsLogicalSafe(edge.address(bundle), bundle.size), "'A' channel carries Logical type unsupported by manager" + extra)
      assert (source_ok, "'A' channel Logical carries invalid source ID" + extra)
      assert (is_aligned, "'A' channel Logical address not aligned to size" + extra)
      assert (TLAtomics.isLogical(bundle.param), "'A' channel Logical carries invalid opcode param" + extra)
      assert (bundle.mask === mask, "'A' channel Logical contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.Hint) {
      assert (edge.manager.supportsHintSafe(edge.address(bundle), bundle.size), "'A' channel carries Hint type unsupported by manager" + extra)
      assert (source_ok, "'A' channel Hint carries invalid source ID" + extra)
      assert (is_aligned, "'A' channel Hint address not aligned to size" + extra)
      assert (bundle.mask === mask, "'A' channel Hint contains invalid mask" + extra)
      assert (!bundle.corrupt, "'A' channel Hint is corrupt" + extra)
    }
  }

  def legalizeFormatB(bundle: TLBundleB, edge: TLEdge) {
    assert (TLMessages.isB(bundle.opcode), "'B' channel has invalid opcode" + extra)

    assert (visible(edge.address(bundle), bundle.source, edge), "'B' channel carries an address illegal for the specified bank visibility")

    // Reuse these subexpressions to save some firrtl lines
    val address_ok = edge.manager.containsSafe(edge.address(bundle))
    val is_aligned = edge.isAligned(bundle.address, bundle.size)
    val mask = edge.full_mask(bundle)
    val legal_source = Mux1H(edge.client.find(bundle.source), edge.client.clients.map(c => UInt(c.sourceId.start))) === bundle.source

    when (bundle.opcode === TLMessages.Probe) {
      assert (edge.client.supportsProbe(bundle.source, bundle.size), "'B' channel carries Probe type unsupported by client" + extra)
      assert (address_ok, "'B' channel Probe carries unmanaged address" + extra)
      assert (legal_source, "'B' channel Probe carries source that is not first source" + extra)
      assert (is_aligned, "'B' channel Probe address not aligned to size" + extra)
      assert (TLPermissions.isCap(bundle.param), "'B' channel Probe carries invalid cap param" + extra)
      assert (bundle.mask === mask, "'B' channel Probe contains invalid mask" + extra)
      assert (!bundle.corrupt, "'B' channel Probe is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.Get) {
      assert (edge.client.supportsGet(bundle.source, bundle.size), "'B' channel carries Get type unsupported by client" + extra)
      assert (address_ok, "'B' channel Get carries unmanaged address" + extra)
      assert (legal_source, "'B' channel Get carries source that is not first source" + extra)
      assert (is_aligned, "'B' channel Get address not aligned to size" + extra)
      assert (bundle.param === UInt(0), "'B' channel Get carries invalid param" + extra)
      assert (bundle.mask === mask, "'B' channel Get contains invalid mask" + extra)
      assert (!bundle.corrupt, "'B' channel Get is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.PutFullData) {
      assert (edge.client.supportsPutFull(bundle.source, bundle.size), "'B' channel carries PutFull type unsupported by client" + extra)
      assert (address_ok, "'B' channel PutFull carries unmanaged address" + extra)
      assert (legal_source, "'B' channel PutFull carries source that is not first source" + extra)
      assert (is_aligned, "'B' channel PutFull address not aligned to size" + extra)
      assert (bundle.param === UInt(0), "'B' channel PutFull carries invalid param" + extra)
      assert (bundle.mask === mask, "'B' channel PutFull contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.PutPartialData) {
      assert (edge.client.supportsPutPartial(bundle.source, bundle.size), "'B' channel carries PutPartial type unsupported by client" + extra)
      assert (address_ok, "'B' channel PutPartial carries unmanaged address" + extra)
      assert (legal_source, "'B' channel PutPartial carries source that is not first source" + extra)
      assert (is_aligned, "'B' channel PutPartial address not aligned to size" + extra)
      assert (bundle.param === UInt(0), "'B' channel PutPartial carries invalid param" + extra)
      assert ((bundle.mask & ~mask) === UInt(0), "'B' channel PutPartial contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.ArithmeticData) {
      assert (edge.client.supportsArithmetic(bundle.source, bundle.size), "'B' channel carries Arithmetic type unsupported by client" + extra)
      assert (address_ok, "'B' channel Arithmetic carries unmanaged address" + extra)
      assert (legal_source, "'B' channel Arithmetic carries source that is not first source" + extra)
      assert (is_aligned, "'B' channel Arithmetic address not aligned to size" + extra)
      assert (TLAtomics.isArithmetic(bundle.param), "'B' channel Arithmetic carries invalid opcode param" + extra)
      assert (bundle.mask === mask, "'B' channel Arithmetic contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.LogicalData) {
      assert (edge.client.supportsLogical(bundle.source, bundle.size), "'B' channel carries Logical type unsupported by client" + extra)
      assert (address_ok, "'B' channel Logical carries unmanaged address" + extra)
      assert (legal_source, "'B' channel Logical carries source that is not first source" + extra)
      assert (is_aligned, "'B' channel Logical address not aligned to size" + extra)
      assert (TLAtomics.isLogical(bundle.param), "'B' channel Logical carries invalid opcode param" + extra)
      assert (bundle.mask === mask, "'B' channel Logical contains invalid mask" + extra)
    }

    when (bundle.opcode === TLMessages.Hint) {
      assert (edge.client.supportsHint(bundle.source, bundle.size), "'B' channel carries Hint type unsupported by client" + extra)
      assert (address_ok, "'B' channel Hint carries unmanaged address" + extra)
      assert (legal_source, "'B' channel Hint carries source that is not first source" + extra)
      assert (is_aligned, "'B' channel Hint address not aligned to size" + extra)
      assert (bundle.mask === mask, "'B' channel Hint contains invalid mask" + extra)
      assert (!bundle.corrupt, "'B' channel Hint is corrupt" + extra)
    }
  }

  def legalizeFormatC(bundle: TLBundleC, edge: TLEdge) {
    assert (TLMessages.isC(bundle.opcode), "'C' channel has invalid opcode" + extra)

    val source_ok = edge.client.contains(bundle.source)
    val is_aligned = edge.isAligned(bundle.address, bundle.size)
    val address_ok = edge.manager.containsSafe(edge.address(bundle))

    assert (visible(edge.address(bundle), bundle.source, edge), "'C' channel carries an address illegal for the specified bank visibility")

    when (bundle.opcode === TLMessages.ProbeAck) {
      assert (address_ok, "'C' channel ProbeAck carries unmanaged address" + extra)
      assert (source_ok, "'C' channel ProbeAck carries invalid source ID" + extra)
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'C' channel ProbeAck smaller than a beat" + extra)
      assert (is_aligned, "'C' channel ProbeAck address not aligned to size" + extra)
      assert (TLPermissions.isReport(bundle.param), "'C' channel ProbeAck carries invalid report param" + extra)
      assert (!bundle.corrupt, "'C' channel ProbeAck is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.ProbeAckData) {
      assert (address_ok, "'C' channel ProbeAckData carries unmanaged address" + extra)
      assert (source_ok, "'C' channel ProbeAckData carries invalid source ID" + extra)
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'C' channel ProbeAckData smaller than a beat" + extra)
      assert (is_aligned, "'C' channel ProbeAckData address not aligned to size" + extra)
      assert (TLPermissions.isReport(bundle.param), "'C' channel ProbeAckData carries invalid report param" + extra)
    }

    when (bundle.opcode === TLMessages.Release) {
      assert (edge.manager.supportsAcquireBSafe(edge.address(bundle), bundle.size), "'C' channel carries Release type unsupported by manager" + extra)
      assert (edge.client.supportsProbe(edge.source(bundle), bundle.size), "'C' channel carries Release from a client which does not support Probe" + extra)
      assert (source_ok, "'C' channel Release carries invalid source ID" + extra)
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'C' channel Release smaller than a beat" + extra)
      assert (is_aligned, "'C' channel Release address not aligned to size" + extra)
      assert (TLPermissions.isShrink(bundle.param), "'C' channel Release carries invalid shrink param" + extra)
      assert (!bundle.corrupt, "'C' channel Release is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.ReleaseData) {
      assert (edge.manager.supportsAcquireBSafe(edge.address(bundle), bundle.size), "'C' channel carries ReleaseData type unsupported by manager" + extra)
      assert (edge.client.supportsProbe(edge.source(bundle), bundle.size), "'C' channel carries Release from a client which does not support Probe" + extra)
      assert (source_ok, "'C' channel ReleaseData carries invalid source ID" + extra)
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'C' channel ReleaseData smaller than a beat" + extra)
      assert (is_aligned, "'C' channel ReleaseData address not aligned to size" + extra)
      assert (TLPermissions.isShrink(bundle.param), "'C' channel ReleaseData carries invalid shrink param" + extra)
    }

    when (bundle.opcode === TLMessages.AccessAck) {
      assert (address_ok, "'C' channel AccessAck carries unmanaged address" + extra)
      assert (source_ok, "'C' channel AccessAck carries invalid source ID" + extra)
      assert (is_aligned, "'C' channel AccessAck address not aligned to size" + extra)
      assert (bundle.param === UInt(0), "'C' channel AccessAck carries invalid param" + extra)
      assert (!bundle.corrupt, "'C' channel AccessAck is corrupt" + extra)
    }

    when (bundle.opcode === TLMessages.AccessAckData) {
      assert (address_ok, "'C' channel AccessAckData carries unmanaged address" + extra)
      assert (source_ok, "'C' channel AccessAckData carries invalid source ID" + extra)
      assert (is_aligned, "'C' channel AccessAckData address not aligned to size" + extra)
      assert (bundle.param === UInt(0), "'C' channel AccessAckData carries invalid param" + extra)
    }

    when (bundle.opcode === TLMessages.HintAck) {
      assert (address_ok, "'C' channel HintAck carries unmanaged address" + extra)
      assert (source_ok, "'C' channel HintAck carries invalid source ID" + extra)
      assert (is_aligned, "'C' channel HintAck address not aligned to size" + extra)
      assert (bundle.param === UInt(0), "'C' channel HintAck carries invalid param" + extra)
      assert (!bundle.corrupt, "'C' channel HintAck is corrupt" + extra)
    }
  }

  def legalizeFormatD(bundle: TLBundleD, edge: TLEdge) {
    assert (TLMessages.isD(bundle.opcode), "'D' channel has invalid opcode" + extra)

    val source_ok = edge.client.contains(bundle.source)
    val sink_ok = bundle.sink < UInt(edge.manager.endSinkId)
    val deny_put_ok = Bool(edge.manager.mayDenyPut)
    val deny_get_ok = Bool(edge.manager.mayDenyGet)

    when (bundle.opcode === TLMessages.ReleaseAck) {
      assert (source_ok, "'D' channel ReleaseAck carries invalid source ID" + extra)
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'D' channel ReleaseAck smaller than a beat" + extra)
      assert (bundle.param === UInt(0), "'D' channel ReleaseeAck carries invalid param" + extra)
      assert (!bundle.corrupt, "'D' channel ReleaseAck is corrupt" + extra)
      assert (!bundle.denied, "'D' channel ReleaseAck is denied" + extra)
    }

    when (bundle.opcode === TLMessages.Grant) {
      assert (source_ok, "'D' channel Grant carries invalid source ID" + extra)
      assert (sink_ok, "'D' channel Grant carries invalid sink ID" + extra)
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'D' channel Grant smaller than a beat" + extra)
      assert (TLPermissions.isCap(bundle.param), "'D' channel Grant carries invalid cap param" + extra)
      assert (bundle.param =/= TLPermissions.toN, "'D' channel Grant carries toN param" + extra)
      assert (!bundle.corrupt, "'D' channel Grant is corrupt" + extra)
      assert (deny_put_ok || !bundle.denied, "'D' channel Grant is denied" + extra)
    }

    when (bundle.opcode === TLMessages.GrantData) {
      assert (source_ok, "'D' channel GrantData carries invalid source ID" + extra)
      assert (sink_ok, "'D' channel GrantData carries invalid sink ID" + extra)
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'D' channel GrantData smaller than a beat" + extra)
      assert (TLPermissions.isCap(bundle.param), "'D' channel GrantData carries invalid cap param" + extra)
      assert (bundle.param =/= TLPermissions.toN, "'D' channel GrantData carries toN param" + extra)
      assert (!bundle.denied || bundle.corrupt, "'D' channel GrantData is denied but not corrupt" + extra)
      assert (deny_get_ok || !bundle.denied, "'D' channel GrantData is denied" + extra)
    }

    when (bundle.opcode === TLMessages.AccessAck) {
      assert (source_ok, "'D' channel AccessAck carries invalid source ID" + extra)
      // size is ignored
      assert (bundle.param === UInt(0), "'D' channel AccessAck carries invalid param" + extra)
      assert (!bundle.corrupt, "'D' channel AccessAck is corrupt" + extra)
      assert (deny_put_ok || !bundle.denied, "'D' channel AccessAck is denied" + extra)
    }

    when (bundle.opcode === TLMessages.AccessAckData) {
      assert (source_ok, "'D' channel AccessAckData carries invalid source ID" + extra)
      // size is ignored
      assert (bundle.param === UInt(0), "'D' channel AccessAckData carries invalid param" + extra)
      assert (!bundle.denied || bundle.corrupt, "'D' channel AccessAckData is denied but not corrupt" + extra)
      assert (deny_get_ok || !bundle.denied, "'D' channel AccessAckData is denied" + extra)
    }

    when (bundle.opcode === TLMessages.HintAck) {
      assert (source_ok, "'D' channel HintAck carries invalid source ID" + extra)
      // size is ignored
      assert (bundle.param === UInt(0), "'D' channel HintAck carries invalid param" + extra)
      assert (!bundle.corrupt, "'D' channel HintAck is corrupt" + extra)
      assert (deny_put_ok || !bundle.denied, "'D' channel HintAck is denied" + extra)
    }
  }

  def legalizeFormatE(bundle: TLBundleE, edge: TLEdge) {
    val sink_ok = bundle.sink < UInt(edge.manager.endSinkId)
    assert (sink_ok, "'E' channels carries invalid sink ID" + extra)
  }

  def legalizeFormat(bundle: TLBundleSnoop, edge: TLEdge) = {
    when (bundle.a.valid) { legalizeFormatA(bundle.a.bits, edge) }
    when (bundle.d.valid) { legalizeFormatD(bundle.d.bits, edge) }
    if (edge.client.anySupportProbe && edge.manager.anySupportAcquireB) {
      when (bundle.b.valid) { legalizeFormatB(bundle.b.bits, edge) }
      when (bundle.c.valid) { legalizeFormatC(bundle.c.bits, edge) }
      when (bundle.e.valid) { legalizeFormatE(bundle.e.bits, edge) }
    } else {
      assert (!bundle.b.valid, "'B' channel valid and not TL-C" + extra)
      assert (!bundle.c.valid, "'C' channel valid and not TL-C" + extra)
      assert (!bundle.e.valid, "'E' channel valid and not TL-C" + extra)
    }
  }

  def legalizeMultibeatA(a: DecoupledSnoop[TLBundleA], edge: TLEdge) {
    val a_first = edge.first(a.bits, a.fire())
    val opcode  = Reg(UInt())
    val param   = Reg(UInt())
    val size    = Reg(UInt())
    val source  = Reg(UInt())
    val address = Reg(UInt())
    when (a.valid && !a_first) {
      assert (a.bits.opcode === opcode, "'A' channel opcode changed within multibeat operation" + extra)
      assert (a.bits.param  === param,  "'A' channel param changed within multibeat operation" + extra)
      assert (a.bits.size   === size,   "'A' channel size changed within multibeat operation" + extra)
      assert (a.bits.source === source, "'A' channel source changed within multibeat operation" + extra)
      assert (a.bits.address=== address,"'A' channel address changed with multibeat operation" + extra)
    }
    when (a.fire() && a_first) {
      opcode  := a.bits.opcode
      param   := a.bits.param
      size    := a.bits.size
      source  := a.bits.source
      address := a.bits.address
    }
  }

  def legalizeMultibeatB(b: DecoupledSnoop[TLBundleB], edge: TLEdge) {
    val b_first = edge.first(b.bits, b.fire())
    val opcode  = Reg(UInt())
    val param   = Reg(UInt())
    val size    = Reg(UInt())
    val source  = Reg(UInt())
    val address = Reg(UInt())
    when (b.valid && !b_first) {
      assert (b.bits.opcode === opcode, "'B' channel opcode changed within multibeat operation" + extra)
      assert (b.bits.param  === param,  "'B' channel param changed within multibeat operation" + extra)
      assert (b.bits.size   === size,   "'B' channel size changed within multibeat operation" + extra)
      assert (b.bits.source === source, "'B' channel source changed within multibeat operation" + extra)
      assert (b.bits.address=== address,"'B' channel addresss changed with multibeat operation" + extra)
    }
    when (b.fire() && b_first) {
      opcode  := b.bits.opcode
      param   := b.bits.param
      size    := b.bits.size
      source  := b.bits.source
      address := b.bits.address
    }
  }

  def legalizeMultibeatC(c: DecoupledSnoop[TLBundleC], edge: TLEdge) {
    val c_first = edge.first(c.bits, c.fire())
    val opcode  = Reg(UInt())
    val param   = Reg(UInt())
    val size    = Reg(UInt())
    val source  = Reg(UInt())
    val address = Reg(UInt())
    when (c.valid && !c_first) {
      assert (c.bits.opcode === opcode, "'C' channel opcode changed within multibeat operation" + extra)
      assert (c.bits.param  === param,  "'C' channel param changed within multibeat operation" + extra)
      assert (c.bits.size   === size,   "'C' channel size changed within multibeat operation" + extra)
      assert (c.bits.source === source, "'C' channel source changed within multibeat operation" + extra)
      assert (c.bits.address=== address,"'C' channel address changed with multibeat operation" + extra)
    }
    when (c.fire() && c_first) {
      opcode  := c.bits.opcode
      param   := c.bits.param
      size    := c.bits.size
      source  := c.bits.source
      address := c.bits.address
    }
  }

  def legalizeMultibeatD(d: DecoupledSnoop[TLBundleD], edge: TLEdge) {
    val d_first = edge.first(d.bits, d.fire())
    val opcode  = Reg(UInt())
    val param   = Reg(UInt())
    val size    = Reg(UInt())
    val source  = Reg(UInt())
    val sink    = Reg(UInt())
    val denied  = Reg(Bool())
    when (d.valid && !d_first) {
      assert (d.bits.opcode === opcode, "'D' channel opcode changed within multibeat operation" + extra)
      assert (d.bits.param  === param,  "'D' channel param changed within multibeat operation" + extra)
      assert (d.bits.size   === size,   "'D' channel size changed within multibeat operation" + extra)
      assert (d.bits.source === source, "'D' channel source changed within multibeat operation" + extra)
      assert (d.bits.sink   === sink,   "'D' channel sink changed with multibeat operation" + extra)
      assert (d.bits.denied === denied, "'D' channel denied changed with multibeat operation" + extra)
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

  def legalizeMultibeat(bundle: TLBundleSnoop, edge: TLEdge) {
    legalizeMultibeatA(bundle.a, edge)
    legalizeMultibeatD(bundle.d, edge)
    if (edge.client.anySupportProbe && edge.manager.anySupportAcquireB) {
      legalizeMultibeatB(bundle.b, edge)
      legalizeMultibeatC(bundle.c, edge)
    }
  }

  def legalizeADSource(bundle: TLBundleSnoop, edge: TLEdge) {
    val inflight = RegInit(UInt(0, width = edge.client.endSourceId))

    val a_first = edge.first(bundle.a.bits, bundle.a.fire())
    val d_first = edge.first(bundle.d.bits, bundle.d.fire())

    val a_set = Wire(init = UInt(0, width = edge.client.endSourceId))
    when (bundle.a.fire() && a_first && edge.isRequest(bundle.a.bits)) {
      a_set := UIntToOH(bundle.a.bits.source)
      assert(!inflight(bundle.a.bits.source), "'A' channel re-used a source ID" + extra)
    }

    val d_clr = Wire(init = UInt(0, width = edge.client.endSourceId))
    val d_release_ack = bundle.d.bits.opcode === TLMessages.ReleaseAck
    when (bundle.d.fire() && d_first && edge.isResponse(bundle.d.bits) && !d_release_ack) {
      d_clr := UIntToOH(bundle.d.bits.source)
      assert((a_set | inflight)(bundle.d.bits.source), "'D' channel acknowledged for nothing inflight" + extra)
    }

    if (edge.manager.minLatency > 0) {
      assert(a_set =/= d_clr || !a_set.orR, s"'A' and 'D' concurrent, despite minlatency ${edge.manager.minLatency}" + extra)
    }

    inflight := (inflight | a_set) & ~d_clr

    val watchdog = RegInit(UInt(0, width = 32))
    val limit = PlusArg("tilelink_timeout",
      docstring="Kill emulation after INT waiting TileLink cycles. Off if 0.")
    assert (!inflight.orR || limit === UInt(0) || watchdog < limit, "TileLink timeout expired" + extra)

    watchdog := watchdog + UInt(1)
    when (bundle.a.fire() || bundle.d.fire()) { watchdog := UInt(0) }
  }

  def legalizeDESink(bundle: TLBundleSnoop, edge: TLEdge) {
    val inflight = RegInit(UInt(0, width = edge.manager.endSinkId))

    val d_first = edge.first(bundle.d.bits, bundle.d.fire())
    val e_first = Bool(true)

    val d_set = Wire(init = UInt(0, width = edge.manager.endSinkId))
    when (bundle.d.fire() && d_first && edge.isRequest(bundle.d.bits)) {
      d_set := UIntToOH(bundle.d.bits.sink)
      assert(!inflight(bundle.d.bits.sink), "'D' channel re-used a sink ID" + extra)
    }

    val e_clr = Wire(init = UInt(0, width = edge.manager.endSinkId))
    when (bundle.e.fire() && e_first && edge.isResponse(bundle.e.bits)) {
      e_clr := UIntToOH(bundle.e.bits.sink)
      assert((d_set | inflight)(bundle.e.bits.sink), "'E' channel acknowledged for nothing inflight" + extra)
    }

    // edge.client.minLatency applies to BC, not DE

    inflight := (inflight | d_set) & ~e_clr
  }

  def legalizeUnique(bundle: TLBundleSnoop, edge: TLEdge) {
    val sourceBits = log2Ceil(edge.client.endSourceId)
    val tooBig = 14 // >16kB worth of flight information gets to be too much
    if (sourceBits > tooBig) {
      println(s"WARNING: TLMonitor instantiated on a bus with source bits (${sourceBits}) > ${tooBig}; A=>D transaction flight will not be checked")
    } else {
      legalizeADSource(bundle, edge)
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

  def legalize(bundle: TLBundleSnoop, edge: TLEdge, reset: Bool) {
    legalizeFormat   (bundle, edge)
    legalizeMultibeat(bundle, edge)
    legalizeUnique   (bundle, edge)
  }
}
