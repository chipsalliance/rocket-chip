// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo

object TLMonitor
{
  def legalizeFormatA(bundle: TLBundleA, edge: TLEdgeOut)(implicit sourceInfo: SourceInfo) = {
    assert (TLMessages.isA(bundle.opcode), "'A' channel has invalid opcode")

    // Reuse these subexpressions to save some firrtl lines
    val source_ok = edge.client.contains(bundle.source)
    val is_aligned = edge.isAligned(bundle.address, bundle.size)
    val mask = edge.fullMask(bundle.address, bundle.size)

    when (bundle.opcode === TLMessages.Acquire) {
      assert (edge.manager.supportsAcquire(bundle.address, bundle.size), "'A' channel carries Acquire type unsupported by manager")
      assert (source_ok, "'A' channel Acquire carries invalid source ID")
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'A' channel Acquire smaller than a beat")
      assert (is_aligned, "'A' channel Acquire address not aligned to size")
      assert (TLPermissions.isGrow(bundle.param), "'A' channel Acquire carries invalid grow param")
      assert (bundle.mask === SInt(-1).asUInt, "'A' channel Acquire contains invalid mask")
    }

    when (bundle.opcode === TLMessages.Get) {
      assert (edge.manager.supportsGet(bundle.address, bundle.size), "'A' channel carries Get type unsupported by manager")
      assert (source_ok, "'A' channel Get carries invalid source ID")
      assert (is_aligned, "'A' channel Get address not aligned to size")
      assert (bundle.param === UInt(0), "'A' channel Get carries invalid param")
      assert (bundle.mask === mask, "'A' channel Get contains invalid mask")
    }

    when (bundle.opcode === TLMessages.PutFullData) {
      assert (edge.manager.supportsPutFull(bundle.address, bundle.size), "'A' channel carries PutFull type unsupported by manager")
      assert (source_ok, "'A' channel PutFull carries invalid source ID")
      assert (is_aligned, "'A' channel PutFull address not aligned to size")
      assert (bundle.param === UInt(0), "'A' channel PutFull carries invalid param")
      assert (bundle.mask === mask, "'A' channel PutFull contains invalid mask")
    }

    when (bundle.opcode === TLMessages.PutPartialData) {
      assert (edge.manager.supportsPutPartial(bundle.address, bundle.size), "'A' channel carries PutPartial type unsupported by manager")
      assert (source_ok, "'A' channel PutPartial carries invalid source ID")
      assert (is_aligned, "'A' channel PutPartial address not aligned to size")
      assert (bundle.param === UInt(0), "'A' channel PutPartial carries invalid param")
      assert ((bundle.mask & ~mask) === UInt(0), "'A' channel PutPartial contains invalid mask")
      assert (bundle.mask =/= UInt(0), "'A' channel PutPartial has a zero mask")
    }

    when (bundle.opcode === TLMessages.ArithmeticData) {
      assert (edge.manager.supportsArithmetic(bundle.address, bundle.size), "'A' channel carries Arithmetic type unsupported by manager")
      assert (source_ok, "'A' channel Arithmetic carries invalid source ID")
      assert (is_aligned, "'A' channel Arithmetic address not aligned to size")
      assert (TLAtomics.isArithmetic(bundle.param), "'A' channel Arithmetic carries invalid opcode param")
      assert (bundle.mask === mask, "'A' channel Arithmetic contains invalid mask")
    }

    when (bundle.opcode === TLMessages.LogicalData) {
      assert (edge.manager.supportsLogical(bundle.address, bundle.size), "'A' channel carries Logical type unsupported by manager")
      assert (source_ok, "'A' channel Logical carries invalid source ID")
      assert (is_aligned, "'A' channel Logical address not aligned to size")
      assert (TLAtomics.isLogical(bundle.param), "'A' channel Logical carries invalid opcode param")
      assert (bundle.mask === mask, "'A' channel Logical contains invalid mask")
    }

    when (bundle.opcode === TLMessages.Hint) {
      assert (edge.manager.supportsHint(bundle.address), "'A' channel carries Hint type unsupported by manager")
      assert (source_ok, "'A' channel Hint carries invalid source ID")
      assert (is_aligned, "'A' channel Hint address not aligned to size")
      assert (bundle.mask === mask, "'A' channel Hint contains invalid mask")
    }
  }

  def legalizeFormatB(bundle: TLBundleB, edge: TLEdgeIn)(implicit sourceInfo: SourceInfo) = {
    assert (TLMessages.isB(bundle.opcode), "'B' channel has invalid opcode")

    // Reuse these subexpressions to save some firrtl lines
    val address_ok = edge.manager.contains(bundle.source)
    val is_aligned = edge.isAligned(bundle.address, bundle.size)
    val mask = edge.fullMask(bundle.address, bundle.size)

    when (bundle.opcode === TLMessages.Probe) {
      assert (edge.client.supportsProbe(bundle.source, bundle.size), "'B' channel carries Probe type unsupported by client")
      assert (address_ok, "'B' channel Probe carries unmanaged address")
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'B' channel Probe smaller than a beat")
      assert (is_aligned, "'B' channel Probe address not aligned to size")
      assert (TLPermissions.isCap(bundle.param), "'B' channel Probe carries invalid cap param")
      assert (bundle.mask === SInt(-1).asUInt, "'B' channel Probe contains invalid mask")
    }

    when (bundle.opcode === TLMessages.Get) {
      assert (edge.client.supportsGet(bundle.source, bundle.size), "'B' channel carries Get type unsupported by client")
      assert (address_ok, "'B' channel Get carries unmanaged address")
      assert (is_aligned, "'B' channel Get address not aligned to size")
      assert (bundle.param === UInt(0), "'B' channel Get carries invalid param")
      assert (bundle.mask === mask, "'A' channel Get contains invalid mask")
    }

    when (bundle.opcode === TLMessages.PutFullData) {
      assert (edge.client.supportsPutFull(bundle.source, bundle.size), "'B' channel carries PutFull type unsupported by client")
      assert (address_ok, "'B' channel PutFull carries unmanaged address")
      assert (is_aligned, "'B' channel PutFull address not aligned to size")
      assert (bundle.param === UInt(0), "'B' channel PutFull carries invalid param")
      assert (bundle.mask === mask, "'B' channel PutFull contains invalid mask")
    }

    when (bundle.opcode === TLMessages.PutPartialData) {
      assert (edge.client.supportsPutPartial(bundle.source, bundle.size), "'B' channel carries PutPartial type unsupported by client")
      assert (address_ok, "'B' channel PutPartial carries unmanaged address")
      assert (is_aligned, "'B' channel PutPartial address not aligned to size")
      assert (bundle.param === UInt(0), "'B' channel PutPartial carries invalid param")
      assert ((bundle.mask & ~mask) === UInt(0), "'B' channel PutPartial contains invalid mask")
      assert (bundle.mask =/= UInt(0), "'B' channel PutPartial has a zero mask")
    }

    when (bundle.opcode === TLMessages.ArithmeticData) {
      assert (edge.client.supportsArithmetic(bundle.source, bundle.size), "'B' channel carries Arithmetic type unsupported by client")
      assert (address_ok, "'B' channel Arithmetic carries unmanaged address")
      assert (is_aligned, "'B' channel Arithmetic address not aligned to size")
      assert (TLAtomics.isArithmetic(bundle.param), "'B' channel Arithmetic carries invalid opcode param")
      assert (bundle.mask === mask, "'B' channel Arithmetic contains invalid mask")
    }

    when (bundle.opcode === TLMessages.LogicalData) {
      assert (edge.client.supportsLogical(bundle.source, bundle.size), "'B' channel carries Logical type unsupported by client")
      assert (address_ok, "'B' channel Logical carries unmanaged address")
      assert (is_aligned, "'B' channel Logical address not aligned to size")
      assert (TLAtomics.isLogical(bundle.param), "'B' channel Logical carries invalid opcode param")
      assert (bundle.mask === mask, "'B' channel Logical contains invalid mask")
    }

    when (bundle.opcode === TLMessages.Hint) {
      assert (edge.client.supportsHint(bundle.source), "'B' channel carries Hint type unsupported by client")
      assert (address_ok, "'B' channel Hint carries unmanaged address")
      assert (is_aligned, "'B' channel Hint address not aligned to size")
      assert (bundle.mask === mask, "'B' channel Hint contains invalid mask")
    }
  }

  def legalizeFormatC(bundle: TLBundleC, edge: TLEdgeOut)(implicit sourceInfo: SourceInfo) = {
    assert (TLMessages.isC(bundle.opcode), "'C' channel has invalid opcode")

    val source_ok = edge.client.contains(bundle.source)
    val is_aligned = edge.isAligned(bundle.address, bundle.size)
    val address_ok = edge.manager.contains(bundle.source)

    when (bundle.opcode === TLMessages.ProbeAck) {
      assert (address_ok, "'C' channel ProbeAck carries unmanaged address")
      // source is ignored
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'C' channel ProbeAck smaller than a beat")
      assert (is_aligned, "'C' channel ProbeAck address not aligned to size")
      assert (TLPermissions.isReport(bundle.param), "'C' channel ProbeAck carries invalid report param")
      assert (!bundle.error, "'C' channel Probe carries an error")
    }

    when (bundle.opcode === TLMessages.ProbeAckData) {
      assert (address_ok, "'C' channel ProbeAckData carries unmanaged address")
      // source is ignored
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'C' channel ProbeAckData smaller than a beat")
      assert (is_aligned, "'C' channel ProbeAckData address not aligned to size")
      assert (TLPermissions.isReport(bundle.param), "'C' channel ProbeAckData carries invalid report param")
      assert (!bundle.error, "'C' channel ProbeData carries an error")
    }

    when (bundle.opcode === TLMessages.Release) {
      assert (edge.manager.supportsAcquire(bundle.address, bundle.size), "'C' channel carries Release type unsupported by manager")
      assert (source_ok, "'C' channel Release carries invalid source ID")
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'C' channel Release smaller than a beat")
      assert (is_aligned, "'C' channel Release address not aligned to size")
      assert (TLPermissions.isShrink(bundle.param), "'C' channel Release carries invalid shrink param")
      assert (!bundle.error, "'C' channel Release carries an error")
    }

    when (bundle.opcode === TLMessages.ReleaseData) {
      assert (edge.manager.supportsAcquire(bundle.address, bundle.size), "'C' channel carries ReleaseData type unsupported by manager")
      assert (source_ok, "'C' channel ReleaseData carries invalid source ID")
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'C' channel ReleaseData smaller than a beat")
      assert (is_aligned, "'C' channel ReleaseData address not aligned to size")
      assert (TLPermissions.isShrink(bundle.param), "'C' channel ReleaseData carries invalid shrink param")
      assert (!bundle.error, "'C' channel ReleaseData carries an error")
    }

    when (bundle.opcode === TLMessages.AccessAck) {
      assert (address_ok, "'C' channel AccessAck carries unmanaged address")
      // source is ignored
      assert (is_aligned, "'C' channel AccessAck address not aligned to size")
      assert (bundle.param === UInt(0), "'C' channel AccessAck carries invalid param")
    }

    when (bundle.opcode === TLMessages.AccessAckData) {
      assert (address_ok, "'C' channel AccessAckData carries unmanaged address")
      // source is ignored
      assert (is_aligned, "'C' channel AccessAckData address not aligned to size")
      assert (bundle.param === UInt(0), "'C' channel AccessAckData carries invalid param")
    }

    when (bundle.opcode === TLMessages.HintAck) {
      assert (address_ok, "'C' channel HintAck carries unmanaged address")
      // source is ignored
      assert (is_aligned, "'C' channel HintAck address not aligned to size")
      assert (bundle.param === UInt(0), "'C' channel HintAck carries invalid param")
      assert (!bundle.error, "'C' channel HintAck carries an error")
    }
  }

  def legalizeFormatD(bundle: TLBundleD, edge: TLEdgeIn)(implicit sourceInfo: SourceInfo) = {
    assert (TLMessages.isD(bundle.opcode), "'D' channel has invalid opcode")

    val source_ok = edge.client.contains(bundle.source)
    val sink_ok = edge.manager.containsById(bundle.sink)

    when (bundle.opcode === TLMessages.ReleaseAck) {
      assert (source_ok, "'D' channel ReleaseAck carries invalid source ID")
      // sink is ignored
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'D' channel ReleaseAck smaller than a beat")
      assert (bundle.param === UInt(0), "'D' channel ReleaseeAck carries invalid param")
      assert (!bundle.error, "'D' channel ReleaseAck carries an error")
    }

    when (bundle.opcode === TLMessages.Grant) {
      assert (source_ok, "'D' channel Grant carries invalid source ID")
      assert (sink_ok, "'D' channel Grant carries invalid sink ID")
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'D' channel Grant smaller than a beat")
      assert (TLPermissions.isCap(bundle.param), "'D' channel Grant carries invalid cap param")
    }

    when (bundle.opcode === TLMessages.GrantData) {
      assert (source_ok, "'D' channel GrantData carries invalid source ID")
      assert (sink_ok, "'D' channel GrantData carries invalid sink ID")
      assert (bundle.size >= UInt(log2Ceil(edge.manager.beatBytes)), "'D' channel GrantData smaller than a beat")
      assert (TLPermissions.isCap(bundle.param), "'D' channel GrantData carries invalid cap param")
    }

    when (bundle.opcode === TLMessages.AccessAck) {
      assert (source_ok, "'D' channel AccessAck carries invalid source ID")
      // sink is ignored
      // size is ignored
      assert (bundle.param === UInt(0), "'D' channel AccessAck carries invalid param")
    }

    when (bundle.opcode === TLMessages.AccessAckData) {
      assert (source_ok, "'D' channel AccessAckData carries invalid source ID")
      // sink is ignored
      // size is ignored
      assert (bundle.param === UInt(0), "'D' channel AccessAckData carries invalid param")
    }

    when (bundle.opcode === TLMessages.HintAck) {
      assert (source_ok, "'D' channel HintAck carries invalid source ID")
      // sink is ignored
      // size is ignored
      assert (bundle.param === UInt(0), "'D' channel HintAck carries invalid param")
      assert (!bundle.error, "'D' channel HintAck carries an error")
    }
  }

  def legalizeFormatE(bundle: TLBundleE, edge: TLEdgeOut)(implicit sourceInfo: SourceInfo) = {
    assert (edge.manager.containsById(bundle.sink), "'E' channels carries invalid sink ID")
  }

  def legalizeFormat(bundleOut: TLBundle, edgeOut: TLEdgeOut, bundleIn: TLBundle, edgeIn: TLEdgeIn)(implicit sourceInfo: SourceInfo) = {
    when (bundleOut.a.valid) { legalizeFormatA(bundleOut.a.bits, edgeOut) }
    when (bundleIn .b.valid) { legalizeFormatB(bundleIn .b.bits, edgeIn)  }
    when (bundleOut.c.valid) { legalizeFormatC(bundleOut.c.bits, edgeOut) }
    when (bundleIn .d.valid) { legalizeFormatD(bundleIn .d.bits, edgeIn)  }
    when (bundleOut.e.valid) { legalizeFormatE(bundleOut.e.bits, edgeOut) }
  }

  def legalizeMultibeatA(a: DecoupledIO[TLBundleA], edge: TLEdgeOut)(implicit sourceInfo: SourceInfo) = {
    val counter = RegInit(UInt(0, width = log2Up(edge.maxTransfer)))
    val opcode  = Reg(UInt())
    val param   = Reg(UInt())
    val size    = Reg(UInt())
    val source  = Reg(UInt())
    val address = Reg(UInt())
    when (a.valid && counter =/= UInt(0)) {
      assert (a.bits.opcode === opcode, "'A' channel opcode changed within multibeat operation")
      assert (a.bits.param  === param,  "'A' channel param changed within multibeat operation")
      assert (a.bits.size   === size,   "'A' channel size changed within multibeat operation")
      assert (a.bits.source === source, "'A' channel source changed within multibeat operation")
      assert (a.bits.address=== address,"'A' channel address changed with multibeat operation")
    }
    when (a.fire()) {
      counter := counter - UInt(1)
      when (counter === UInt(0)) {
        counter := edge.numBeats(a.bits) - UInt(1)
        opcode  := a.bits.opcode
        param   := a.bits.param
        size    := a.bits.size
        source  := a.bits.source
        address := a.bits.address
      }
    }
  }

  def legalizeMultibeatB(b: DecoupledIO[TLBundleB], edge: TLEdgeIn)(implicit sourceInfo: SourceInfo) = {
    val counter = RegInit(UInt(0, width = log2Up(edge.maxTransfer)))
    val opcode  = Reg(UInt())
    val param   = Reg(UInt())
    val size    = Reg(UInt())
    val source  = Reg(UInt())
    val address = Reg(UInt())
    when (b.valid && counter =/= UInt(0)) {
      assert (b.bits.opcode === opcode, "'B' channel opcode changed within multibeat operation")
      assert (b.bits.param  === param,  "'B' channel param changed within multibeat operation")
      assert (b.bits.size   === size,   "'B' channel size changed within multibeat operation")
      assert (b.bits.source === source, "'B' channel source changed within multibeat operation")
      assert (b.bits.address=== address,"'B' channel address changed with multibeat operation")
    }
    when (b.fire()) {
      counter := counter - UInt(1)
      when (counter === UInt(0)) {
        counter := edge.numBeats(b.bits) - UInt(1)
        opcode  := b.bits.opcode
        param   := b.bits.param
        size    := b.bits.size
        source  := b.bits.source
        address := b.bits.address
      }
    }
  }

  def legalizeMultibeatC(c: DecoupledIO[TLBundleC], edge: TLEdgeOut)(implicit sourceInfo: SourceInfo) = {
    val counter = RegInit(UInt(0, width = log2Up(edge.maxTransfer)))
    val opcode  = Reg(UInt())
    val param   = Reg(UInt())
    val size    = Reg(UInt())
    val source  = Reg(UInt())
    val address = Reg(UInt())
    when (c.valid && counter =/= UInt(0)) {
      assert (c.bits.opcode === opcode, "'C' channel opcode changed within multibeat operation")
      assert (c.bits.param  === param,  "'C' channel param changed within multibeat operation")
      assert (c.bits.size   === size,   "'C' channel size changed within multibeat operation")
      assert (c.bits.source === source, "'C' channel source changed within multibeat operation")
      assert (c.bits.address=== address,"'C' channel address changed with multibeat operation")
    }
    when (c.fire()) {
      counter := counter - UInt(1)
      when (counter === UInt(0)) {
        counter := edge.numBeats(c.bits) - UInt(1)
        opcode  := c.bits.opcode
        param   := c.bits.param
        size    := c.bits.size
        source  := c.bits.source
        address := c.bits.address
      }
    }
  }

  def legalizeMultibeatD(d: DecoupledIO[TLBundleD], edge: TLEdgeIn)(implicit sourceInfo: SourceInfo) = {
    val counter = RegInit(UInt(0, width = log2Up(edge.maxTransfer)))
    val opcode  = Reg(UInt())
    val param   = Reg(UInt())
    val size    = Reg(UInt())
    val source  = Reg(UInt())
    val sink    = Reg(UInt())
    when (d.valid && counter =/= UInt(0)) {
      assert (d.bits.opcode === opcode, "'D' channel opcode changed within multibeat operation")
      assert (d.bits.param  === param,  "'D' channel param changed within multibeat operation")
      assert (d.bits.size   === size,   "'D' channel size changed within multibeat operation")
      assert (d.bits.source === source, "'D' channel source changed within multibeat operation")
      assert (d.bits.sink   === sink,   "'D' channel sink changed with multibeat operation")
    }
    when (d.fire()) {
      counter := counter - UInt(1)
      when (counter === UInt(0)) {
        counter := edge.numBeats(d.bits) - UInt(1)
        opcode  := d.bits.opcode
        param   := d.bits.param
        size    := d.bits.size
        source  := d.bits.source
        sink    := d.bits.sink
      }
    }
  }

  def legalizeMultibeat(bundleOut: TLBundle, edgeOut: TLEdgeOut, bundleIn: TLBundle, edgeIn: TLEdgeIn)(implicit sourceInfo: SourceInfo) = {
    legalizeMultibeatA(bundleOut.a, edgeOut)
    legalizeMultibeatB(bundleOut.b, edgeIn)
    legalizeMultibeatC(bundleOut.c, edgeOut)
    legalizeMultibeatD(bundleOut.d, edgeIn)
  }

  def legalize(bundleOut: TLBundle, edgeOut: TLEdgeOut, bundleIn: TLBundle, edgeIn: TLEdgeIn)(implicit sourceInfo: SourceInfo) = {
    legalizeFormat   (bundleOut, edgeOut, bundleIn, edgeIn)
    legalizeMultibeat(bundleOut, edgeOut, bundleIn, edgeIn)
    // !!! validate source uniqueness
  }
}
