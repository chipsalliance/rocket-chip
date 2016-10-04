package groundtest

import Chisel._
import uncore.tilelink._
import uncore.constants._
import uncore.agents._
import util._
import cde.{Parameters, Field}

class CacheFillTest(implicit p: Parameters) extends GroundTest()(p)
    with HasTileLinkParameters {
  val capacityKb: Int = p("L2_CAPACITY_IN_KB")
  val nblocks = capacityKb * 1024 / p(CacheBlockBytes)
  val s_start :: s_prefetch :: s_retrieve :: s_finished :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_start)

  val active = state.isOneOf(s_prefetch, s_retrieve)

  val xact_pending = Reg(init = UInt(0, tlMaxClientXacts))
  val xact_id = PriorityEncoder(~xact_pending)

  val (req_block, round_done) = Counter(io.mem.head.acquire.fire(), nblocks)

  io.mem.head.acquire.valid := active && !xact_pending.andR
  io.mem.head.acquire.bits := Mux(state === s_prefetch,
    GetPrefetch(xact_id, UInt(memStartBlock) + req_block),
    GetBlock(xact_id, UInt(memStartBlock) + req_block))
  io.mem.head.grant.ready := xact_pending.orR

  def add_pending(acq: DecoupledIO[Acquire]): UInt =
    Mux(acq.fire(), UIntToOH(acq.bits.client_xact_id), UInt(0))

  def remove_pending(gnt: DecoupledIO[Grant]): UInt = {
    val last_grant = !gnt.bits.hasMultibeatData() ||
                      gnt.bits.addr_beat === UInt(tlDataBeats - 1)
    ~Mux(gnt.fire() && last_grant, UIntToOH(gnt.bits.client_xact_id), UInt(0))
  }

  xact_pending := (xact_pending |
    add_pending(io.mem.head.acquire)) &
    remove_pending(io.mem.head.grant)

  when (state === s_start) { state := s_prefetch }
  when (state === s_prefetch && round_done) { state := s_retrieve }
  when (state === s_retrieve && round_done) { state := s_finished }

  io.status.finished := (state === s_finished)
  io.status.timeout.valid := Bool(false)
  io.status.error.valid := Bool(false)
}
