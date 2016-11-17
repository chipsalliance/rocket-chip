package groundtest

import Chisel._
import uncore.tilelink._
import uncore.agents._
import uncore.coherence.{InnerTLId, OuterTLId}
import util._
import junctions.HasAddrMapParameters
import rocketchip._
import cde.Parameters

/**
 * An example bus mastering devices that writes some preset data to memory.
 * When it receives an MMIO put request, it starts writing out the data.
 * When it receives an MMIO get request, it responds with the progress of
 * the write. A grant data of 1 means it is still writing, grant data 0 
 * means it has finished.
 */
class ExampleBusMaster(implicit val p: Parameters) extends Module
    with HasAddrMapParameters
    with HasTileLinkParameters {
  val mmioParams = p.alterPartial({ case TLId => p(InnerTLId) })
  val memParams = p.alterPartial({ case TLId => p(OuterTLId) })
  val memStart = p(ExtMem).base
  val memStartBlock = memStart >> p(CacheBlockOffsetBits)

  val io = new Bundle {
    val mmio = new ClientUncachedTileLinkIO()(mmioParams).flip
    val mem = new ClientUncachedTileLinkIO()(memParams)
  }

  val s_idle :: s_put :: s_resp :: Nil = Enum(Bits(), 3)
  val state = Reg(init = s_idle)
  val send_resp = Reg(init = Bool(false))
  val r_acq = Reg(new AcquireMetadata)

  io.mmio.acquire.ready := !send_resp
  io.mmio.grant.valid := send_resp
  io.mmio.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = r_acq.getBuiltInGrantType(),
    client_xact_id = r_acq.client_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = r_acq.addr_beat,
    data = Mux(state === s_idle, UInt(0), UInt(1)))

  when (io.mmio.acquire.fire()) {
    send_resp := Bool(true)
    r_acq := io.mmio.acquire.bits
    when (state === s_idle && io.mmio.acquire.bits.hasData()) { state := s_put }
  }
  when (io.mmio.grant.fire()) { send_resp := Bool(false) }

  val (put_beat, put_done) = Counter(io.mem.acquire.fire(), tlDataBeats)
  when (put_done) { state := s_resp }
  when (io.mem.grant.fire()) { state := s_idle }

  io.mem.acquire.valid := state === s_put
  io.mem.acquire.bits := PutBlock(
    client_xact_id = UInt(0),
    addr_block = UInt(memStartBlock),
    addr_beat = put_beat,
    data = put_beat)
  io.mem.grant.ready := state === s_resp
}

class BusMasterTest(implicit p: Parameters) extends GroundTest()(p)
    with HasTileLinkParameters {
  val (s_idle :: s_req_start :: s_resp_start :: s_req_poll :: s_resp_poll ::
       s_req_check :: s_resp_check :: s_done :: Nil) = Enum(Bits(), 8)
  val state = Reg(init = s_idle)

  val busMasterBlock = p(ExtBus).base >> p(CacheBlockOffsetBits)
  val start_acq = Put(
    client_xact_id = UInt(0),
    addr_block = UInt(busMasterBlock),
    addr_beat = UInt(0),
    data = UInt(1))
  val poll_acq = Get(
    client_xact_id = UInt(0),
    addr_block = UInt(busMasterBlock),
    addr_beat = UInt(0))
  val check_acq = GetBlock(
    client_xact_id = UInt(0),
    addr_block = UInt(memStartBlock))

  val acq = io.mem.head.acquire
  val gnt = io.mem.head.grant

  acq.valid := state.isOneOf(s_req_start, s_req_poll, s_req_check)
  acq.bits := MuxLookup(state, check_acq, Seq(
    s_req_start -> start_acq,
    s_req_poll -> poll_acq))
  gnt.ready := state.isOneOf(s_resp_start, s_resp_poll, s_resp_check)

  val (get_beat, get_done) = Counter(
    state === s_resp_check && gnt.valid, tlDataBeats)

  when (state === s_idle) { state := s_req_start }
  when (state === s_req_start && acq.ready) { state := s_resp_start }
  when (state === s_resp_start && gnt.valid) { state := s_req_poll }
  when (state === s_req_poll && acq.ready) { state := s_resp_poll }
  when (state === s_resp_poll && gnt.valid) {
    when (gnt.bits.data === UInt(0)) {
      state := s_req_check
    } .otherwise { state := s_req_poll }
  }
  when (state === s_req_check && acq.ready) { state := s_resp_check }
  when (get_done) { state := s_done }

  io.status.finished := state === s_done

  assert(state =/= s_resp_check || !gnt.valid ||
         gnt.bits.data === get_beat,
         "BusMasterTest: data does not match")
}
