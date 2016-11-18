package groundtest

import Chisel._
import uncore.tilelink._
import uncore.constants._
import uncore.agents._
import util._
import junctions.HasAddrMapParameters
import rocket._
import rocketchip._
import config._

class RegressionIO(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val start = Bool(INPUT)
  val cache = new HellaCacheIO
  val mem = new ClientUncachedTileLinkIO
  val finished = Bool(OUTPUT)
  val errored = Bool(OUTPUT)
}

abstract class Regression(implicit val p: Parameters)
    extends Module with HasTileLinkParameters with HasAddrMapParameters {
  val memStart = p(ExtMem).base
  val memStartBlock = memStart >> p(CacheBlockOffsetBits)
  val io = new RegressionIO

  def disableCache() {
    io.cache.req.valid := Bool(false)
    io.cache.req.bits.addr := UInt(memStart)
    io.cache.req.bits.typ  := UInt(0)
    io.cache.req.bits.cmd  := M_XRD
    io.cache.req.bits.tag  := UInt(0)
    io.cache.req.bits.data := Bits(0)
    io.cache.req.bits.phys := Bool(true)
    io.cache.invalidate_lr := Bool(false)
  }

  def disableMem() {
    io.mem.acquire.valid := Bool(false)
    io.mem.grant.ready := Bool(false)
  }
}

/**
 * This was a bug in which the TileLinkIONarrower logic screwed up
 * when a PutBlock request and a narrow Get request are sent to it at the
 * same time. Repeating this sequence enough times will cause a queue to
 * get filled up and deadlock the system.
 */
class IOGetAfterPutBlockRegression(implicit p: Parameters) extends Regression()(p) {
  val nRuns = 7
  val run = Reg(init = UInt(0, log2Up(nRuns + 1)))

  val (put_beat, put_done) = Counter(
    io.mem.acquire.fire() && io.mem.acquire.bits.hasData(), tlDataBeats)

  val started = Reg(init = Bool(false))
  val put_sent = Reg(init = Bool(false))
  val get_sent = Reg(init = Bool(false))
  val put_acked = Reg(init = Bool(false))
  val get_acked = Reg(init = Bool(false))
  val both_acked = put_acked && get_acked

  when (!started && io.start) { started := Bool(true) }

  io.mem.acquire.valid := !put_sent && started
  io.mem.acquire.bits := PutBlock(
    client_xact_id = UInt(0),
    addr_block = UInt(memStartBlock),
    addr_beat = put_beat,
    data = UInt(0))
  io.mem.grant.ready := Bool(true)

  io.cache.req.valid := !get_sent && started
  io.cache.req.bits.addr := UInt(testRamAddr)
  io.cache.req.bits.typ := MT_WU
  io.cache.req.bits.cmd := M_XRD
  io.cache.req.bits.tag := UInt(0)
  io.cache.invalidate_lr := Bool(false)

  when (put_done) { put_sent := Bool(true) }
  when (io.cache.req.fire()) { get_sent := Bool(true) }
  when (io.mem.grant.fire()) { put_acked := Bool(true) }
  when (io.cache.resp.valid) { get_acked := Bool(true) }

  when (both_acked) {
    when (run < UInt(nRuns - 1)) {
      put_sent := Bool(false)
      get_sent := Bool(false)
    }
    put_acked := Bool(false)
    get_acked := Bool(false)
    run := run + UInt(1)
  }

  io.finished := (run === UInt(nRuns))
}

/* This was a bug with merging two PutBlocks to the same address in the L2.
 * The transactor would start accepting beats of the second transaction but
 * acknowledge both of them when the first one finished.
 * This caused the state to go funky since the next time around it would
 * start the put in the middle */
class PutBlockMergeRegression(implicit p: Parameters)
    extends Regression()(p) with HasTileLinkParameters {
  val s_idle :: s_put :: s_wait :: s_done :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  disableCache()

  val nSets = p(CacheName("L2")).nSets
  val addr_blocks = Vec(Seq(0, 0, nSets).map(num => UInt(num + memStartBlock)))
  val nSteps = addr_blocks.size
  val (acq_beat, acq_done) = Counter(io.mem.acquire.fire(), tlDataBeats)
  val (send_cnt, send_done) = Counter(acq_done, nSteps)
  val (ack_cnt, ack_done) = Counter(io.mem.grant.fire(), nSteps)

  io.mem.acquire.valid := (state === s_put)
  io.mem.acquire.bits := PutBlock(
    client_xact_id = send_cnt,
    addr_block = addr_blocks(send_cnt),
    addr_beat = acq_beat,
    data = Cat(send_cnt, acq_beat))
  io.mem.grant.ready := Bool(true)

  when (state === s_idle && io.start) { state := s_put }
  when (send_done) { state := s_wait }
  when (ack_done) { state := s_done }

  io.finished := (state === s_done)
}

/* Make sure the L2 does "the right thing" when a put is sent no-alloc but
 * the block is already in cache. It should just treat the request as a
 * regular allocating put */
class NoAllocPutHitRegression(implicit p: Parameters) extends Regression()(p) {
  val (s_idle :: s_prefetch :: s_put :: s_get ::
       s_wait :: s_done :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_idle)

  val acq = io.mem.acquire.bits
  val gnt = io.mem.grant.bits

  val (put_beat, put_done) = Counter(io.mem.acquire.fire() && acq.hasData(), tlDataBeats)
  val acked = Reg(init = UInt(0, tlDataBeats + 2))

  val addr_block = UInt(memStartBlock + 2)
  val test_data = UInt(0x3446)

  val prefetch_acq = GetPrefetch(
    client_xact_id = UInt(0),
    addr_block = addr_block)
  val put_acq = PutBlock(
    client_xact_id = UInt(1),
    addr_block = addr_block,
    addr_beat = put_beat,
    data = test_data,
    alloc = Bool(false))
  val get_acq = GetBlock(
    client_xact_id = UInt(2),
    addr_block = addr_block)

  io.mem.acquire.valid := state.isOneOf(s_prefetch, s_get, s_put)
  io.mem.acquire.bits := MuxCase(get_acq, Seq(
    (state === s_prefetch) -> prefetch_acq,
    (state === s_put) -> put_acq))
  io.mem.grant.ready := Bool(true)

  when (state === s_idle && io.start) { state := s_prefetch }
  when (state === s_prefetch && io.mem.acquire.ready) { state := s_put }
  when (put_done) { state := s_get }
  when (state === s_get && io.mem.acquire.ready) { state := s_wait }
  when (state === s_wait && acked.andR) { state := s_done }

  when (io.mem.grant.fire()) {
    switch (gnt.client_xact_id) {
      is (UInt(0)) { acked := acked | UInt(1 << tlDataBeats) }
      is (UInt(1)) { acked := acked | UInt(1 << (tlDataBeats + 1)) }
      is (UInt(2)) { acked := acked | UIntToOH(gnt.addr_beat) }
    }
  }

  val data_mismatch = io.mem.grant.fire() && gnt.hasData() && gnt.data =/= test_data
  assert(!data_mismatch, "NoAllocPutHitRegression: data does not match")

  io.finished := (state === s_done)
  io.errored := data_mismatch

  disableCache()
}

/** Make sure L2 does the right thing when multiple puts are sent for the
 *  same block, but only the first one has the alloc bit set. */
class MixedAllocPutRegression(implicit p: Parameters) extends Regression()(p) {
  val (s_idle :: s_pf_send :: s_pf_wait :: s_put_send :: s_put_wait ::
       s_get_send :: s_get_wait :: s_done :: Nil) = Enum(Bits(), 8)
  val state = Reg(init = s_idle)

  /** We have to test two cases: one when the block is already cached
   *  and one when the block is not yet cached.
   *  We use prefetching to assure the first case. */
  val test_data = Vec(
    UInt("h2222222211111111"),
    UInt("h3333333333333333"),
    UInt("h4444444444444444"),
    UInt("h5555555555555555"))
  val test_alloc = Vec(Bool(false), Bool(false), Bool(true), Bool(false))
  val test_block = Vec(
    Seq.fill(2) { UInt(memStartBlock + 15) } ++
    Seq.fill(2) { UInt(memStartBlock + 16) })
  val test_beat = Vec(UInt(0), UInt(2), UInt(1), UInt(2))

  val (put_acq_id, put_acq_done) = Counter(
    state === s_put_send && io.mem.acquire.ready, test_data.size)
  val (put_gnt_cnt, put_gnt_done) = Counter(
    state === s_put_wait && io.mem.grant.valid, test_data.size)

  val (get_acq_id, get_acq_done) = Counter(
    state === s_get_send && io.mem.acquire.ready, test_data.size)
  val (get_gnt_cnt, get_gnt_done) = Counter(
    state === s_get_wait && io.mem.grant.valid, test_data.size)

  val pf_acquire = PutPrefetch(
    client_xact_id = UInt(0),
    addr_block = UInt(memStartBlock + 15))

  val put_acquire = Put(
    client_xact_id = put_acq_id,
    addr_block = test_block(put_acq_id),
    addr_beat = test_beat(put_acq_id),
    data = test_data(put_acq_id),
    alloc = test_alloc(put_acq_id))

  val get_acquire = Get(
    client_xact_id = get_acq_id,
    addr_block = test_block(get_acq_id),
    addr_beat = test_beat(get_acq_id))

  io.mem.acquire.valid := state.isOneOf(s_pf_send, s_put_send, s_get_send)
  io.mem.acquire.bits := MuxLookup(state, pf_acquire, Seq(
    s_put_send -> put_acquire,
    s_get_send -> get_acquire))
  io.mem.grant.ready := state.isOneOf(s_pf_wait, s_put_wait, s_get_wait)

  when (state === s_idle && io.start) { state := s_pf_send }
  when (state === s_pf_send && io.mem.acquire.ready) { state := s_pf_wait }
  when (state === s_pf_wait && io.mem.grant.valid) { state := s_put_send }
  when (put_acq_done) { state := s_put_wait }
  when (put_gnt_done) { state := s_get_send }
  when (get_acq_done) { state := s_get_wait }
  when (get_gnt_done) { state := s_done }

  io.finished := (state === s_done)

  val data_mismatch = state === s_get_wait && io.mem.grant.fire() &&
    io.mem.grant.bits.data =/= test_data(io.mem.grant.bits.client_xact_id)
  assert(!data_mismatch, "MixedAllocPutRegression: data mismatch")
  io.errored := data_mismatch

  disableCache()
}

/* Make sure each no-alloc put triggers a request to outer memory.
 * Unfortunately, there's no way to verify that this works except by looking
 * at the waveform */
class RepeatedNoAllocPutRegression(implicit p: Parameters) extends Regression()(p) {
  disableCache()

  val nPuts = 2
  val (put_beat, put_done) = Counter(io.mem.acquire.fire(), tlDataBeats)
  val (req_cnt, req_done) = Counter(put_done, nPuts)

  val sending = Reg(init = Bool(false))
  val acked = Reg(init = UInt(0, nPuts))

  when (!sending && io.start) { sending := Bool(true) }
  when (sending && req_done) { sending := Bool(false) }

  io.mem.acquire.valid := sending
  io.mem.acquire.bits := PutBlock(
    client_xact_id = req_cnt,
    addr_block = UInt(memStartBlock + 5),
    addr_beat = put_beat,
    data = Cat(req_cnt, UInt(0, 8)),
    alloc = Bool(false))
  io.mem.grant.ready := Bool(true)

  when (io.mem.grant.fire()) {
    acked := acked | UIntToOH(io.mem.grant.bits.client_xact_id)
  }

  io.finished := acked.andR
}

/* Make sure write masking works properly by writing a block of data
 * piece by piece */
class WriteMaskedPutBlockRegression(implicit p: Parameters) extends Regression()(p) {
  disableCache()

  val (s_idle :: s_put_send :: s_put_ack :: s_stall ::
       s_get_send :: s_get_ack :: s_done :: Nil) = Enum(Bits(), 7)
  val state = Reg(init = s_idle)
  val post_stall_state = Reg(init = s_idle)

  val gnt = io.mem.grant.bits
  val acq = io.mem.acquire.bits

  val stage = Reg(init = UInt(0, 1))

  val (put_beat, put_block_done) = Counter(
    io.mem.acquire.fire() && acq.hasData(), tlDataBeats)
  val put_data = UInt(0x30010040, tlDataBits) + (put_beat << UInt(2))

  val put_acq = PutBlock(
    client_xact_id = UInt(0),
    addr_block = UInt(memStartBlock + 7),
    addr_beat = put_beat,
    data = Mux(put_beat(0) === stage, put_data, UInt(0)),
    wmask = Some(Mux(put_beat(0) === stage, Acquire.fullWriteMask, Bits(0))))

  val get_acq = GetBlock(
    client_xact_id = UInt(0),
    addr_block = UInt(memStartBlock + 6) + stage)

  io.mem.acquire.valid := state.isOneOf(s_put_send, s_get_send)
  io.mem.acquire.bits := Mux(state === s_get_send, get_acq, put_acq)
  io.mem.grant.ready := state.isOneOf(s_put_ack, s_get_ack)

  val (get_cnt, get_done) = Counter(
    io.mem.grant.fire() && gnt.hasData(), tlDataBeats)
  val get_data = UInt(0x30010040, tlDataBits) + (get_cnt << UInt(2))

  val (stall_cnt, stall_done) = Counter(state === s_stall, 16)

  when (state === s_idle && io.start) { state := s_put_send }
  when (put_block_done) { state := s_put_ack }
  when (state === s_put_ack && io.mem.grant.valid) {
    post_stall_state := s_get_send
    state := s_stall
  }
  when (stall_done) { state := post_stall_state }
  when (state === s_get_send && io.mem.acquire.ready) { state := s_get_ack }
  when (get_done) {
    // do a read in-between the two put-blocks to overwrite the data buffer
    when (stage === UInt(0)) {
      stage := stage + UInt(1)
      post_stall_state := s_put_send
      state := s_stall
    } .otherwise { state := s_done }
  }

  io.finished := (state === s_done)

  val data_mismatch = io.mem.grant.fire() && io.mem.grant.bits.hasData() &&
                      stage =/= UInt(0) && io.mem.grant.bits.data =/= get_data
  assert(!data_mismatch, "WriteMaskedPutBlockRegression: data does not match")
  io.errored := data_mismatch
}

/* Make sure a prefetch that hits returns immediately. */
class PrefetchHitRegression(implicit p: Parameters) extends Regression()(p) {
  disableCache()

  val sending = Reg(init = Bool(false))
  val nPrefetches = 2
  val (pf_cnt, pf_done) = Counter(io.mem.acquire.fire(), nPrefetches)
  val acked = Reg(init = UInt(0, nPrefetches))

  val acq_bits = Vec(
    PutPrefetch(client_xact_id = UInt(0), addr_block = UInt(memStartBlock + 12)),
    GetPrefetch(client_xact_id = UInt(1), addr_block = UInt(memStartBlock + 12)))

  io.mem.acquire.valid := sending
  io.mem.acquire.bits := acq_bits(pf_cnt)
  io.mem.grant.ready := Bool(true)

  when (io.mem.grant.fire()) {
    acked := acked | UIntToOH(io.mem.grant.bits.client_xact_id)
  }

  when (!sending && io.start) { sending := Bool(true) }
  when (sending && pf_done) { sending := Bool(false) }

  io.finished := acked.andR
  io.errored := Bool(false)
}

/* This tests the sort of access the pattern that Hwacha uses.
 * Instead of using PutBlock/GetBlock, it uses word-sized puts and gets
 * to the same block.
 * Each request has the same client_xact_id, but there are multiple in flight.
 * The responses therefore must come back in the order they are sent. */
class SequentialSameIdGetRegression(implicit p: Parameters) extends Regression()(p) {
  disableCache()

  val sending = Reg(init = Bool(false))
  val finished = Reg(init = Bool(false))

  val (send_cnt, send_done) = Counter(io.mem.acquire.fire(), tlDataBeats)
  val (recv_cnt, recv_done) = Counter(io.mem.grant.fire(), tlDataBeats)

  when (!sending && io.start) { sending := Bool(true) }
  when (send_done) { sending := Bool(false) }
  when (recv_done) { finished := Bool(true) }

  io.mem.acquire.valid := sending
  io.mem.acquire.bits := Get(
    client_xact_id = UInt(0),
    addr_block = UInt(memStartBlock + 9),
    addr_beat = send_cnt)
  io.mem.grant.ready := !finished

  io.finished := finished

  val beat_mismatch = io.mem.grant.fire() && io.mem.grant.bits.addr_beat =/= recv_cnt
  assert(!beat_mismatch, "SequentialSameIdGetRegression: grant received out of order")
  io.errored := beat_mismatch
}

/* Test that a writeback will occur by writing nWays + 1 blocks to the same
 * set. This assumes that there is only a single cache bank. If we want to
 * test multibank configurations, we'll have to think of some other way to
 * determine which banks are conflicting */
class WritebackRegression(implicit p: Parameters) extends Regression()(p) {
  disableCache()

  val nSets = p(CacheName("L2")).nSets
  val nWays = p(CacheName("L2")).nWays

  val addr_blocks = Vec.tabulate(nWays + 1) { i => UInt(memStartBlock + i * nSets) }
  val data = Vec.tabulate(nWays + 1) { i => UInt((i + 1) * 1423) }

  val (put_beat, put_done) = Counter(
    io.mem.acquire.fire() && io.mem.acquire.bits.hasData(), tlDataBeats)
  val (get_beat, get_done) = Counter(
    io.mem.grant.fire() && io.mem.grant.bits.hasData(), tlDataBeats)
  val (put_cnt, _) = Counter(put_done, nWays + 1)
  val (get_cnt, _) = Counter(
    io.mem.acquire.fire() && !io.mem.acquire.bits.hasData(), nWays + 1)
  val (ack_cnt, ack_done) = Counter(
    io.mem.grant.fire() && !io.mem.grant.bits.hasData() || get_done, nWays + 1)

  val s_idle :: s_put :: s_get :: s_done :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)
  val sending = Reg(init = Bool(false))

  io.mem.acquire.valid := sending
  io.mem.acquire.bits := Mux(state === s_put,
    PutBlock(
      client_xact_id = UInt(0),
      addr_block = addr_blocks(put_cnt),
      addr_beat = put_beat,
      data = data(put_cnt)),
    GetBlock(
      client_xact_id = UInt(0),
      addr_block = addr_blocks(get_cnt)))
  io.mem.grant.ready := !sending

  when (state === s_idle && io.start) { state := s_put; sending := Bool(true) }
  when (put_done || state === s_get && io.mem.acquire.fire()) {
    sending := Bool(false)
  }
  when (get_done && !ack_done || state === s_put && io.mem.grant.fire()) {
    sending := Bool(true)
  }
  when (ack_done) { state := Mux(state === s_put, s_get, s_done) }

  io.finished := (state === s_done)

  val data_mismatch = io.mem.grant.fire() && io.mem.grant.bits.hasData() &&
                      io.mem.grant.bits.data =/= data(ack_cnt)
  assert(!data_mismatch, "WritebackRegression: incorrect data")
  io.errored := data_mismatch
}

class ReleaseRegression(implicit p: Parameters) extends Regression()(p) {
  disableMem()

  val nSets = p(CacheName("L1D")).nSets
  val nWays = p(CacheName("L1D")).nWays
  val blockOffset = p(CacheBlockOffsetBits)

  val startBlock = memStartBlock + 10
  val addr_blocks = Vec.tabulate(nWays + 1) { i => UInt(startBlock + i * nSets) }
  val data = Vec.tabulate(nWays + 1) { i => UInt((i + 1) * 1522) }
  val (req_idx, req_done) = Counter(io.cache.req.fire(), nWays + 1)
  val (resp_idx, resp_done) = Counter(io.cache.resp.valid, nWays + 1)

  val sending = Reg(init = Bool(false))
  val s_idle :: s_write :: s_read :: s_done :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  io.cache.req.valid := sending && state.isOneOf(s_write, s_read)
  io.cache.req.bits.addr := Cat(addr_blocks(req_idx), UInt(0, blockOffset))
  io.cache.req.bits.typ := MT_D
  io.cache.req.bits.cmd := Mux(state === s_write, M_XWR, M_XRD)
  io.cache.req.bits.tag := UInt(0)
  io.cache.req.bits.data := data(req_idx)
  io.cache.req.bits.phys := Bool(true)
  io.cache.invalidate_lr := Bool(false)

  when (state === s_idle && io.start) {
    sending := Bool(true)
    state := s_write
  }

  when (resp_done) { state := Mux(state === s_write, s_read, s_done) }
  when (io.cache.req.fire()) { sending := Bool(false) }
  when (io.cache.resp.valid) { sending := Bool(true) }

  io.finished := (state === s_done)

  val data_mismatch = io.cache.resp.valid && io.cache.resp.bits.has_data &&
                      io.cache.resp.bits.data =/= data(resp_idx)
  assert(!data_mismatch, "ReleaseRegression: data mismatch")
  io.errored := data_mismatch
}

class PutBeforePutBlockRegression(implicit p: Parameters) extends Regression()(p) {
  val (s_idle :: s_put :: s_putblock :: s_wait ::
       s_finished :: Nil) = Enum(Bits(), 5)
  val state = Reg(init = s_idle)

  disableCache()

  val (put_block_beat, put_block_done) = Counter(
    state === s_putblock && io.mem.acquire.ready, tlDataBeats)

  val put_acquire = Put(
    client_xact_id = UInt(0),
    addr_block = UInt(memStartBlock),
    addr_beat = UInt(0),
    data = UInt(0),
    wmask = Some(UInt((1 << 8) - 1)))

  val put_block_acquire = PutBlock(
    client_xact_id = UInt(1),
    addr_block = UInt(memStartBlock + 1),
    addr_beat = put_block_beat,
    data = UInt(0))

  val put_acked = Reg(init = UInt(0, 2))

  val (ack_cnt, all_acked) = Counter(io.mem.grant.fire(), 2)

  io.mem.acquire.valid := state.isOneOf(s_put, s_putblock)
  io.mem.acquire.bits := Mux(state === s_put, put_acquire, put_block_acquire)
  io.mem.grant.ready := (state === s_wait)

  when (state === s_idle && io.start) { state := s_put }
  when (state === s_put && io.mem.acquire.ready) { state := s_putblock }
  when (put_block_done) { state := s_wait }
  when (all_acked) { state := s_finished }

  io.finished := (state === s_finished)
  io.errored := Bool(false)
}

/**
 * Make sure that multiple gets to the same line and beat are merged
 * correctly, even if it is a cache miss.
 */
class MergedGetRegression(implicit p: Parameters) extends Regression()(p) {
  disableCache()

  val nSets = p(CacheName("L2")).nSets
  val nWays = p(CacheName("L2")).nWays

  val (s_idle :: s_put :: s_get :: s_done :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  // Write NWays + 1 different conflicting lines to force an eviction of the first line
  val (put_acq_cnt, put_acq_done) = Counter(state === s_put && io.mem.acquire.fire(), nWays + 1)
  val (put_gnt_cnt, put_gnt_done) = Counter(state === s_put && io.mem.grant.fire(), nWays + 1)
  val put_addr = UInt(memStartBlock) + Cat(put_acq_cnt, UInt(0, log2Up(nSets)))

  val (get_acq_cnt, get_acq_done) = Counter(state === s_get && io.mem.acquire.fire(), 2)
  val (get_gnt_cnt, get_gnt_done) = Counter(state === s_get && io.mem.grant.fire(), 2)
  val sending = Reg(init = Bool(false))

  when (state === s_idle && io.start) { state := s_put; sending := Bool(true) }
  when (state === s_put) {
    when (io.mem.acquire.fire()) { sending := Bool(false) }
    when (io.mem.grant.fire()) { sending := Bool(true) }
    when (put_gnt_done) { state := s_get }
  }
  when (state === s_get) {
    when (get_acq_done) { sending := Bool(false) }
    when (get_gnt_done) { state := s_done }
  }

  io.mem.acquire.valid := sending
  io.mem.acquire.bits := Mux(state === s_put,
    Put(
      client_xact_id = UInt(0),
      addr_block = put_addr,
      addr_beat = UInt(3),
      data = UInt("hdabb9321")),
    Get(
      client_xact_id = get_acq_cnt,
      addr_block = UInt(memStartBlock),
      addr_beat = UInt(3)))
  io.mem.grant.ready := !sending

  val data_mismatch = io.mem.grant.valid && io.mem.grant.bits.hasData() &&
                      io.mem.grant.bits.data =/= UInt("hdabb9321")
  assert(!data_mismatch, "RepeatedGetRegression: wrong data back")

  io.finished := state === s_done
  io.errored := data_mismatch
}

/**
 * Make sure that multiple puts to the same line and beat are merged
 * correctly, even if there is a release from the L1
 */
class MergedPutRegression(implicit p: Parameters) extends Regression()(p)
    with HasTileLinkParameters {
  val (s_idle :: s_cache_req :: s_cache_wait ::
       s_put :: s_get :: s_done :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_idle)

  io.cache.req.valid := (state === s_cache_req)
  io.cache.req.bits.cmd := M_XWR
  io.cache.req.bits.typ := MT_D
  io.cache.req.bits.addr := UInt(memStart)
  io.cache.req.bits.data := UInt(1)
  io.cache.req.bits.tag := UInt(0)

  val sending = Reg(init = Bool(false))
  val delaying = Reg(init = Bool(false))
  val (put_cnt, put_done) = Counter(io.mem.acquire.fire(), tlMaxClientXacts)
  val (delay_cnt, delay_done) = Counter(delaying, 8)
  val put_acked = Reg(UInt(width = 3), init = UInt(0))

  io.mem.acquire.valid := sending && !delaying
  io.mem.acquire.bits := Mux(state === s_put,
    Put(
      client_xact_id = put_cnt,
      addr_block = UInt(memStartBlock),
      addr_beat = UInt(0),
      data = put_cnt + UInt(2)),
    Get(
      client_xact_id = UInt(0),
      addr_block = UInt(memStartBlock),
      addr_beat = UInt(0)))
  io.mem.grant.ready := Bool(true)

  when (state === s_idle && io.start) { state := s_cache_req }
  when (io.cache.req.fire()) { state := s_cache_wait }
  when (io.cache.resp.valid) { state := s_put; sending := Bool(true) }

  when (io.mem.acquire.fire()) {
    delaying := Bool(true)
    when (put_done || state === s_get) { sending := Bool(false) }
  }
  when (delay_done) { delaying := Bool(false) }

  when (io.mem.grant.fire()) {
    when (state === s_put) {
      put_acked := put_acked | UIntToOH(io.mem.grant.bits.client_xact_id)
    }
    when (state === s_get) { state := s_done }
  }

  when (state === s_put && put_acked.andR) {
    state := s_get
    sending := Bool(true)
  }

  val expected_data = UInt(2 + tlMaxClientXacts - 1)
  val data_mismatch = io.mem.grant.valid && io.mem.grant.bits.hasData() &&
    io.mem.grant.bits.data =/= expected_data

  assert(!data_mismatch, "MergedPutRegression: data mismatch")

  io.finished := (state === s_done)
  io.errored := data_mismatch
}

class PutAfterReleaseRegression(implicit p: Parameters) extends Regression()(p) {
  val (s_idle :: s_cache_req :: s_cache_resp ::
       s_write_first_req :: s_delay :: s_write_remaining_req :: s_write_resp ::
       s_read_req :: s_read_resp :: s_finished :: Nil) = Enum(Bits(), 10)
  val state = Reg(init = s_idle)

  val (delay_cnt, delay_done) = Counter(state === s_delay, 100)
  val (write_cnt, write_done) = Counter(
    io.mem.acquire.fire() && io.mem.acquire.bits.hasData(), tlDataBeats)
  val (read_cnt, read_done) = Counter(
    io.mem.grant.fire() && io.mem.grant.bits.hasData(), tlDataBeats)

  when (state === s_idle && io.start) { state := s_cache_req }
  when (io.cache.req.fire()) { state := s_cache_resp }
  when (state === s_cache_resp && io.cache.resp.valid) { state := s_write_first_req }
  when (state === s_write_first_req && io.mem.acquire.ready) { state := s_delay }
  when (delay_done) { state := s_write_remaining_req }
  when (write_done) { state := s_write_resp }
  when (state === s_write_resp && io.mem.grant.valid) { state := s_read_req }
  when (state === s_read_req && io.mem.acquire.ready) { state := s_read_resp }
  when (read_done) { state := s_finished }

  io.finished := state === s_finished

  io.cache.req.valid := state === s_cache_req
  io.cache.req.bits.cmd := M_XWR
  io.cache.req.bits.addr := UInt(memStart)
  io.cache.req.bits.typ := MT_D
  io.cache.req.bits.tag := UInt(0)
  io.cache.req.bits.data := UInt(0)

  io.mem.acquire.valid := state.isOneOf(s_write_first_req, s_write_remaining_req, s_read_req)
  io.mem.acquire.bits := Mux(state === s_read_req,
    GetBlock(
      client_xact_id = UInt(0),
      addr_block = UInt(memStartBlock)),
    PutBlock(
      client_xact_id = UInt(0),
      addr_block = UInt(memStartBlock),
      addr_beat = write_cnt,
      data = write_cnt + UInt(1)))
  io.mem.grant.ready := state.isOneOf(s_write_resp, s_read_resp)

  assert(!io.mem.grant.valid || !io.mem.grant.bits.hasData() ||
         io.mem.grant.bits.data === read_cnt + UInt(1),
         "PutAfterReleaseRegression: data mismatch")
}

object RegressionTests {
  def cacheRegressions(implicit p: Parameters) = Seq(
    Module(new PutBlockMergeRegression),
    Module(new NoAllocPutHitRegression),
    Module(new RepeatedNoAllocPutRegression),
    Module(new WriteMaskedPutBlockRegression),
    Module(new PrefetchHitRegression),
    Module(new SequentialSameIdGetRegression),
    Module(new WritebackRegression),
    Module(new PutBeforePutBlockRegression),
    Module(new MixedAllocPutRegression),
    Module(new ReleaseRegression),
    Module(new MergedGetRegression),
    Module(new MergedPutRegression))
  def broadcastRegressions(implicit p: Parameters) = Seq(
    Module(new IOGetAfterPutBlockRegression),
    Module(new WriteMaskedPutBlockRegression),
    Module(new PutBeforePutBlockRegression),
    Module(new ReleaseRegression),
    Module(new PutAfterReleaseRegression))
}

case object GroundTestRegressions extends Field[Parameters => Seq[Regression]]

class RegressionTest(implicit p: Parameters) extends GroundTest()(p) {
  val regressions = p(GroundTestRegressions)(p)
  val regress_idx = Reg(init = UInt(0, log2Up(regressions.size + 1)))
  val cur_finished = Wire(init = Bool(false))
  val all_done = (regress_idx === UInt(regressions.size))
  val start = Reg(init = Bool(true))

  // default output values
  io.mem.head.acquire.valid := Bool(false)
  io.mem.head.acquire.bits := GetBlock(
    client_xact_id = UInt(0),
    addr_block = UInt(0))
  io.mem.head.grant.ready := Bool(false)
  io.cache.head.req.valid := Bool(false)
  io.cache.head.req.bits.addr := UInt(0)
  io.cache.head.req.bits.typ := UInt(log2Ceil(64 / 8))
  io.cache.head.req.bits.cmd := M_XRD
  io.cache.head.req.bits.tag := UInt(0)
  io.cache.head.req.bits.phys := Bool(true)
  io.cache.head.req.bits.data := UInt(0)
  io.cache.head.invalidate_lr := Bool(false)

  regressions.zipWithIndex.foreach { case (regress, i) =>
    val me = regress_idx === UInt(i)
    regress.io.start := me && start
    regress.io.mem.acquire.ready := io.mem.head.acquire.ready && me
    regress.io.mem.grant.valid   := io.mem.head.grant.valid && me
    regress.io.mem.grant.bits    := io.mem.head.grant.bits
    regress.io.cache.req.ready   := io.cache.head.req.ready && me
    regress.io.cache.resp.valid  := io.cache.head.resp.valid && me
    regress.io.cache.resp.bits   := io.cache.head.resp.bits

    when (me) {
      io.mem.head.acquire.valid := regress.io.mem.acquire.valid
      io.mem.head.acquire.bits := regress.io.mem.acquire.bits
      io.mem.head.grant.ready := regress.io.mem.grant.ready
      io.cache.head.req.valid := regress.io.cache.req.valid
      io.cache.head.req.bits := regress.io.cache.req.bits
      io.cache.head.invalidate_lr := regress.io.cache.invalidate_lr
      io.status.error.valid := regress.io.errored
      io.status.error.bits := UInt(i)
      cur_finished := regress.io.finished
    }

    when (regress.io.start) {
      printf(s"Starting regression ${regress.getClass.getSimpleName}\n")
    }
  }

  when (cur_finished && !all_done) {
    start := Bool(true)
    regress_idx := regress_idx + UInt(1)
  }
  when (start) { start := Bool(false) }

  val timeout = SimpleTimer(5000, start, cur_finished)
  assert(!timeout, "Regression timed out")

  io.status.finished := all_done
  io.status.timeout.valid := timeout
  io.status.timeout.bits := UInt(0)

  assert(!(all_done && io.mem.head.grant.valid),
    "Getting grant after test completion")

  when (all_done) {
    io.status.error.valid := io.mem.head.grant.valid
    io.status.error.bits := UInt(regressions.size)
  }
}
