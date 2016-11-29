// See LICENSE.Berkeley for license details.

package uncore.agents

import Chisel._
import uncore.tilelink._
import config._

class MMIOTileLinkManagerData(implicit p: Parameters)
    extends TLBundle()(p)
    with HasClientId
    with HasClientTransactionId

class MMIOTileLinkManager(implicit p: Parameters)
    extends CoherenceAgentModule()(p) {
  val io = new ManagerTLIO

  // MMIO requests should never need probe or release
  io.inner.probe.valid := Bool(false)
  io.inner.release.ready := Bool(false)

  val multibeat_fire = io.outer.acquire.fire() && io.oacq().hasMultibeatData()
  val multibeat_start = multibeat_fire && io.oacq().addr_beat === UInt(0)
  val multibeat_end = multibeat_fire && io.oacq().addr_beat === UInt(outerDataBeats - 1)

  // Acquire and Grant are basically passthru,
  // except client_id and client_xact_id need to be converted.
  // Associate the inner client_id and client_xact_id
  // with the outer client_xact_id.
  val xact_pending = Reg(init = UInt(0, maxManagerXacts))
  val xact_id_sel = PriorityEncoder(~xact_pending)
  val xact_id_reg = RegEnable(xact_id_sel, multibeat_start)
  val xact_multibeat = Reg(init = Bool(false))
  val outer_xact_id = Mux(xact_multibeat, xact_id_reg, xact_id_sel)
  val xact_free = !xact_pending.andR
  val xact_buffer = Reg(Vec(maxManagerXacts, new MMIOTileLinkManagerData))

  io.inner.acquire.ready := io.outer.acquire.ready && xact_free
  io.outer.acquire.valid := io.inner.acquire.valid && xact_free
  io.outer.acquire.bits  <> io.inner.acquire.bits
  io.outer.acquire.bits.client_xact_id := outer_xact_id

  def isLastBeat[T <: TileLinkChannel with HasTileLinkBeatId](in: T): Bool =
    !in.hasMultibeatData() || in.addr_beat === UInt(outerDataBeats - 1)

  def addPendingBitOnAcq[T <: AcquireMetadata](in: DecoupledIO[T]): UInt =
    Mux(in.fire() && isLastBeat(in.bits), UIntToOH(in.bits.client_xact_id), UInt(0))

  def clearPendingBitOnGnt[T <: GrantMetadata](in: DecoupledIO[T]): UInt =
    ~Mux(in.fire() && isLastBeat(in.bits) && !in.bits.requiresAck(),
      UIntToOH(in.bits.manager_xact_id), UInt(0))

  def clearPendingBitOnFin(in: DecoupledIO[Finish]): UInt =
    ~Mux(in.fire(), UIntToOH(in.bits.manager_xact_id), UInt(0))

  xact_pending := (xact_pending | addPendingBitOnAcq(io.outer.acquire)) &
                                  clearPendingBitOnFin(io.inner.finish) &
                                  clearPendingBitOnGnt(io.inner.grant)

  when (io.outer.acquire.fire() && isLastBeat(io.outer.acquire.bits)) {
    xact_buffer(outer_xact_id) <> io.iacq()
  }

  when (multibeat_start) { xact_multibeat := Bool(true) }
  when (multibeat_end)   { xact_multibeat := Bool(false) }

  val gnt_xact = xact_buffer(io.ognt().client_xact_id)
  io.outer.grant.ready := io.inner.grant.ready
  io.inner.grant.valid := io.outer.grant.valid
  io.inner.grant.bits  <> io.outer.grant.bits
  io.inner.grant.bits.client_id := gnt_xact.client_id
  io.inner.grant.bits.client_xact_id := gnt_xact.client_xact_id
  io.inner.grant.bits.manager_xact_id := io.ognt().client_xact_id
  io.inner.finish.ready := Bool(true)
}
