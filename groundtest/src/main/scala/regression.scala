package groundtest

import Chisel._
import uncore._
import junctions.{MMIOBase, ParameterizedBundle}
import rocket.HellaCacheIO
import cde.{Parameters, Field}

class RegressionIO(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val start = Bool(INPUT)
  val cache = new HellaCacheIO
  val mem = new ClientUncachedTileLinkIO
  val finished = Bool(OUTPUT)
}

abstract class Regression(implicit val p: Parameters)
    extends Module with HasTileLinkParameters {
  val io = new RegressionIO
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
    addr_block = UInt(0),
    addr_beat = put_beat,
    data = UInt(0))
  io.mem.grant.ready := Bool(true)

  io.cache.req.valid := !get_sent && started
  io.cache.req.bits.addr := UInt(p(MMIOBase))
  io.cache.req.bits.typ := MT_W
  io.cache.req.bits.cmd := M_XRD
  io.cache.req.bits.tag := UInt(0)
  io.cache.req.bits.kill := Bool(false)
  io.cache.req.bits.phys := Bool(true)

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

  io.cache.req.valid := Bool(false)

  val l2params = p.alterPartial({ case CacheName => "L2Bank" })
  val nSets = l2params(NSets)
  val addr_blocks = Vec(UInt(0), UInt(0), UInt(nSets))
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

  val addr_block = UInt(2)
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

  io.mem.acquire.valid := (state === s_prefetch) || (state === s_get) || (state === s_put)
  io.mem.acquire.bits := MuxBundle(get_acq, Seq(
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

  assert(!io.mem.grant.valid || !gnt.hasData() || gnt.data === test_data,
    "NoAllocPutHitRegression: data does not match")

  io.finished := (state === s_done)
  io.cache.req.valid := Bool(false)
}

/* Make sure each no-alloc put triggers a request to outer memory.
 * Unfortunately, there's no way to verify that this works except by looking
 * at the waveform */
class RepeatedNoAllocPutRegression(implicit p: Parameters) extends Regression()(p) {
  io.cache.req.valid := Bool(false)

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
    addr_block = UInt(5),
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
  io.cache.req.valid := Bool(false)

  val (s_idle :: s_put_send :: s_put_ack :: s_stall ::
       s_get_send :: s_get_ack :: s_done :: Nil) = Enum(Bits(), 7)
  val state = Reg(init = s_idle)
  val post_stall_state = Reg(init = s_idle)

  val gnt = io.mem.grant.bits
  val acq = io.mem.acquire.bits

  val stage = Reg(init = UInt(0, 1))

  val (put_beat, put_block_done) = Counter(
    io.mem.acquire.fire() && acq.hasData(), tlDataBeats)

  val data_beats = Vec.tabulate(tlDataBeats) {
    i => UInt(0x3001040 + i * 4, tlDataBits)
  }

  val put_acq = PutBlock(
    client_xact_id = UInt(0),
    addr_block = UInt(7),
    addr_beat = put_beat,
    data = Mux(put_beat(0) === stage, data_beats(put_beat), UInt(0)),
    wmask = Mux(put_beat(0) === stage, Acquire.fullWriteMask, Bits(0)))

  val get_acq = GetBlock(
    client_xact_id = UInt(0),
    addr_block = UInt(6) + stage)

  io.mem.acquire.valid := (state === s_put_send || state === s_get_send)
  io.mem.acquire.bits := Mux(state === s_get_send, get_acq, put_acq)
  io.mem.grant.ready := (state === s_put_ack || state === s_get_ack)

  val (get_cnt, get_done) = Counter(
    io.mem.grant.fire() && gnt.hasData(), tlDataBeats)

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

  assert(!io.mem.grant.valid ||
         !io.mem.grant.bits.hasData() ||
         stage === UInt(0) ||
         io.mem.grant.bits.data === data_beats(get_cnt),
         "WriteMaskedPutBlockRegression: data does not match")
}

/* Make sure a prefetch that hits returns immediately. */
class PrefetchHitRegression(implicit p: Parameters) extends Regression()(p) {
  io.cache.req.valid := Bool(false)

  val sending = Reg(init = Bool(false))
  val nPrefetches = 2
  val (pf_cnt, pf_done) = Counter(io.mem.acquire.fire(), nPrefetches)
  val acked = Reg(init = UInt(0, nPrefetches))

  val acq_bits = Vec(
    PutPrefetch(client_xact_id = UInt(0), addr_block = UInt(12)),
    GetPrefetch(client_xact_id = UInt(1), addr_block = UInt(12)))

  io.mem.acquire.valid := sending
  io.mem.acquire.bits := acq_bits(pf_cnt)
  io.mem.grant.ready := Bool(true)

  when (io.mem.grant.fire()) {
    acked := acked | UIntToOH(io.mem.grant.bits.client_xact_id)
  }

  when (!sending && io.start) { sending := Bool(true) }
  when (sending && pf_done) { sending := Bool(false) }

  io.finished := acked.andR
}

/* This tests the sort of access the pattern that Hwacha uses.
 * Instead of using PutBlock/GetBlock, it uses word-sized puts and gets
 * to the same block.
 * Each request has the same client_xact_id, but there are multiple in flight.
 * The responses therefore must come back in the order they are sent. */
class SequentialSameIdGetRegression(implicit p: Parameters) extends Regression()(p) {
  io.cache.req.valid := Bool(false)

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
    addr_block = UInt(9),
    addr_beat = send_cnt)
  io.mem.grant.ready := !finished

  io.finished := finished

  assert(!io.mem.grant.valid || io.mem.grant.bits.addr_beat === recv_cnt,
    "SequentialSameIdGetRegression: grant received out of order")
}

/* Test that a writeback will occur by writing nWays + 1 blocks to the same
 * set. This assumes that there is only a single cache bank. If we want to
 * test multibank configurations, we'll have to think of some other way to
 * determine which banks are conflicting */
class WritebackRegression(implicit p: Parameters) extends Regression()(p) {
  io.cache.req.valid := Bool(false)

  val l2params = p.alterPartial({ case CacheName => "L2Bank" })
  val nSets = l2params(NSets)
  val nWays = l2params(NWays)

  val addr_blocks = Vec.tabulate(nWays + 1) { i => UInt(i * nSets) }
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

  assert(!io.mem.grant.valid || !io.mem.grant.bits.hasData() ||
    io.mem.grant.bits.data === data(ack_cnt),
    "WritebackRegression: incorrect data")
}

object RegressionTests {
  def cacheRegressions(implicit p: Parameters) = Seq(
    Module(new PutBlockMergeRegression),
    Module(new NoAllocPutHitRegression),
    Module(new RepeatedNoAllocPutRegression),
    Module(new WriteMaskedPutBlockRegression),
    Module(new PrefetchHitRegression),
    Module(new SequentialSameIdGetRegression),
    Module(new WritebackRegression))
  def broadcastRegressions(implicit p: Parameters) = Seq(
    Module(new IOGetAfterPutBlockRegression),
    Module(new WriteMaskedPutBlockRegression))
}

case object GroundTestRegressions extends Field[Parameters => Seq[Regression]]

class RegressionTest(implicit p: Parameters) extends GroundTest()(p) {
  disablePorts(mem = false, cache = false)

  val regressions = p(GroundTestRegressions)(p)
  val regressIOs = Vec(regressions.map(_.io))
  val regress_idx = Reg(init = UInt(0, log2Up(regressions.size + 1)))
  val all_done = (regress_idx === UInt(regressions.size))
  val start = Reg(init = Bool(true))

  when (start) { start := Bool(false) }

  regressIOs.zipWithIndex.foreach { case (regress, i) =>
    val me = regress_idx === UInt(i)
    regress.start := me && start
    regress.mem.acquire.ready := io.mem.acquire.ready && me
    regress.mem.grant.valid := io.mem.grant.valid && me
    regress.mem.grant.bits := io.mem.grant.bits
    regress.cache.req.ready := io.cache.req.ready && me
    regress.cache.resp.valid := io.cache.resp.valid && me
  }

  val cur_regression = regressIOs(regress_idx)
  val cur_acquire = cur_regression.mem.acquire
  val cur_grant = cur_regression.mem.grant
  val cur_cache_req = cur_regression.cache.req

  io.mem.acquire.valid := cur_acquire.valid
  io.mem.acquire.bits := cur_acquire.bits
  io.mem.grant.ready := cur_grant.ready
  io.cache.req.valid := cur_cache_req.valid
  io.cache.req.bits := cur_cache_req.bits

  when (cur_regression.finished && !all_done) {
    start := Bool(true)
    regress_idx := regress_idx + UInt(1)
  }

  io.finished := all_done

  val timeout = Timer(5000, start, cur_regression.finished)
  assert(!timeout, "Regression timed out")

  assert(!(all_done && io.mem.grant.valid), "Getting grant after test completion")
}
