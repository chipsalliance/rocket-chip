package groundtest.unittests

import Chisel._
import uncore.tilelink._
import uncore.devices._
import cde.Parameters

class BRAMSlaveDriver(implicit val p: Parameters) extends Module
    with HasTileLinkParameters {
  val io = new Bundle {
    val mem = new ClientUncachedTileLinkIO
    val start = Bool(INPUT)
    val finished = Bool(OUTPUT)
  }

  val (s_idle :: s_pf_req :: s_pf_stall :: s_pf_resp ::
       s_put_req :: s_put_stall :: s_put_resp ::
       s_get_req :: s_get_stall :: s_get_resp ::
       s_done :: Nil) = Enum(Bits(), 11)
  val state = Reg(init = s_idle)

  val pf_acquire = PutPrefetch(
    client_xact_id = UInt(0),
    addr_block = UInt(0))

  val (put_beat, put_done) = Counter(
    state === s_put_req && io.mem.acquire.ready, tlDataBeats)
  val put_data = Fill(tlDataBits / tlBeatAddrBits, put_beat)

  val put_acquire = PutBlock(
    client_xact_id = UInt(0),
    addr_block = UInt(0),
    addr_beat = put_beat,
    data = put_data)

  val get_acquire = GetBlock(
    client_xact_id = UInt(0),
    addr_block = UInt(0))

  val (get_beat, get_done) = Counter(
    state === s_get_resp && io.mem.grant.valid, tlDataBeats)
  val get_data = Fill(tlDataBits / tlBeatAddrBits, get_beat)

  val (stall_cnt, stall_done) = Counter(
    state === s_pf_stall || state === s_put_stall || state === s_get_stall, 4)

  io.mem.acquire.valid := (state === s_pf_req) || (state === s_put_req) || (state === s_get_req)
  io.mem.acquire.bits := MuxLookup(state, get_acquire, Seq(
    s_pf_req -> pf_acquire,
    s_put_req -> put_acquire))
  io.mem.grant.ready := (state === s_pf_resp) || (state === s_put_resp) || (state === s_get_resp)

  when (state === s_idle && io.start) { state := s_pf_req }
  when (state === s_pf_req && io.mem.acquire.ready) { state := s_pf_stall }
  when (state === s_pf_stall && stall_done) { state := s_pf_resp }
  when (state === s_pf_resp && io.mem.grant.valid) { state := s_put_req }
  when (state === s_put_req && io.mem.acquire.ready) { state := s_put_stall }
  when (state === s_put_stall && stall_done) { state := s_put_req }
  when (put_done) { state := s_put_resp }
  when (state === s_put_resp && io.mem.grant.valid) { state := s_get_req }
  when (state === s_get_req && io.mem.acquire.ready) { state := s_get_stall }
  when (state === s_get_stall && stall_done) { state := s_get_resp }
  when (state === s_get_resp && io.mem.grant.valid) { state := s_get_stall }
  when (get_done) { state := s_done }

  io.finished := (state === s_done)

  assert(!io.mem.grant.valid || !io.mem.grant.bits.hasData() ||
         io.mem.grant.bits.data === get_data,
         "BRAMSlaveTest: data doesn't match")
}

class BRAMSlaveTest(implicit val p: Parameters) extends UnitTest
    with HasTileLinkParameters {
  val driver = Module(new BRAMSlaveDriver)
  val bram = Module(new BRAMSlave(tlDataBeats))
  driver.io.start := io.start
  io.finished := driver.io.finished
  bram.io <> driver.io.mem
}


