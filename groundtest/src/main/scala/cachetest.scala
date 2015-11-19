package groundtest

import Chisel._
import uncore._
import cde.{Parameters, Field}

class CacheFillTest(implicit p: Parameters) extends GroundTest()(p)
    with HasTileLinkParameters {
  val capacityKb: Int = p("L2_CAPACITY_IN_KB")
  val nblocks = capacityKb * 1024 / p(CacheBlockBytes)
  val s_start :: s_prefetch :: s_retrieve :: s_finished :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_start)

  val inflight = Reg(init = Bool(false))
  val active = state === s_prefetch || state === s_retrieve

  disablePorts(mem = false)

  val (xact_id, xact_flip) = Counter(io.mem.acquire.fire(), tlMaxClientXacts)
  val (req_block, round_done) = Counter(io.mem.acquire.fire(), nblocks)

  io.mem.acquire.valid := active && !inflight
  io.mem.acquire.bits := Mux(state === s_prefetch,
    GetPrefetch(xact_id, req_block),
    GetBlock(xact_id, req_block))
  io.mem.grant.ready := active

  when (io.mem.acquire.fire()) {
    inflight := Bool(true)
  }

  val last_grant = !io.mem.grant.bits.hasMultibeatData() ||
                    io.mem.grant.bits.addr_beat === UInt(tlDataBeats - 1)

  when (io.mem.grant.fire() && last_grant) {
    inflight := Bool(false)
  }

  when (state === s_start) { state := s_prefetch }
  when (state === s_prefetch && round_done) { state := s_retrieve }
  when (state === s_retrieve && round_done) { state := s_finished }

  io.finished := (state === s_finished)
}
