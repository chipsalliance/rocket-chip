// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util._

/**
 * This module buffers requests made by the SimpleHellaCacheIF in case they
 * are nacked. Nacked requests must be replayed in order, and no other requests
 * must be allowed to go through until the replayed requests are successfully
 * completed.
 */
class SimpleHellaCacheIFReplayQueue(depth: Int)
    (implicit val p: Parameters) extends Module
    with HasL1HellaCacheParameters {
  val io = new Bundle {
    val req = Decoupled(new HellaCacheReq).flip
    val nack = Valid(Bits(width = coreDCacheReqTagBits)).flip
    val resp = Valid(new HellaCacheResp).flip
    val replay = Decoupled(new HellaCacheReq)
  }

  // Registers to store the sent request
  // When a request is sent the first time,
  // it is stored in one of the reqs registers
  // and the corresponding inflight bit is set.
  // The reqs register will be deallocated once the request is
  // successfully completed.
  val inflight = Reg(init = UInt(0, depth))
  val reqs = Reg(Vec(depth, new HellaCacheReq))

  // The nack queue stores the index of nacked requests (in the reqs vector)
  // in the order that they were nacked. A request is enqueued onto nackq
  // when it is newly nacked (i.e. not a nack for a previous replay).
  // The head of the nack queue will be replayed until it is
  // successfully completed, at which time the request is dequeued.
  // No new requests will be made or other replays attempted until the head
  // of the nackq is successfully completed.
  val nackq = Module(new Queue(UInt(width = log2Up(depth)), depth))
  val replaying = Reg(init = Bool(false))

  val next_inflight_onehot = PriorityEncoderOH(~inflight)
  val next_inflight = OHToUInt(next_inflight_onehot)

  val next_replay = nackq.io.deq.bits
  val next_replay_onehot = UIntToOH(next_replay)
  val next_replay_req = reqs(next_replay)

  // Keep sending the head of the nack queue until it succeeds
  io.replay.valid := nackq.io.deq.valid && !replaying
  io.replay.bits := next_replay_req
  // Don't allow new requests if there is are replays waiting
  // or something being nacked.
  io.req.ready := !inflight.andR && !nackq.io.deq.valid && !io.nack.valid

  // Match on the tags to determine the index of nacks or responses
  val nack_onehot = Cat(reqs.map(_.tag === io.nack.bits).reverse) & inflight
  val resp_onehot = Cat(reqs.map(_.tag === io.resp.bits.tag).reverse) & inflight

  val replay_complete = io.resp.valid && replaying && io.resp.bits.tag === next_replay_req.tag
  val nack_head = io.nack.valid && nackq.io.deq.valid && io.nack.bits === next_replay_req.tag

  // Enqueue to the nack queue if there is a nack that is not in response to
  // the previous replay
  nackq.io.enq.valid := io.nack.valid && !nack_head
  nackq.io.enq.bits := OHToUInt(nack_onehot)
  assert(!nackq.io.enq.valid || nackq.io.enq.ready,
    "SimpleHellaCacheIF: ReplayQueue nack queue overflow")

  // Dequeue from the nack queue if the last replay was successfully completed
  nackq.io.deq.ready := replay_complete
  assert(!nackq.io.deq.ready || nackq.io.deq.valid,
    "SimpleHellaCacheIF: ReplayQueue nack queue underflow")

  // Set inflight bit when a request is made
  // Clear it when it is successfully completed
  inflight := (inflight | Mux(io.req.fire(), next_inflight_onehot, UInt(0))) &
                          ~Mux(io.resp.valid, resp_onehot, UInt(0))

  when (io.req.fire()) {
    reqs(next_inflight) := io.req.bits
  }

  // Only one replay outstanding at a time
  when (io.replay.fire()) { replaying := Bool(true) }
  when (nack_head || replay_complete) { replaying := Bool(false) }
}

// exposes a sane decoupled request interface
class SimpleHellaCacheIF(implicit p: Parameters) extends Module
{
  val io = new Bundle {
    val requestor = new HellaCacheIO().flip
    val cache = new HellaCacheIO
  }

  val replayq = Module(new SimpleHellaCacheIFReplayQueue(2))
  val req_arb = Module(new Arbiter(new HellaCacheReq, 2))

  val req_helper = DecoupledHelper(
    req_arb.io.in(1).ready,
    replayq.io.req.ready,
    io.requestor.req.valid)

  req_arb.io.in(0) <> replayq.io.replay
  req_arb.io.in(1).valid := req_helper.fire(req_arb.io.in(1).ready)
  req_arb.io.in(1).bits := io.requestor.req.bits
  io.requestor.req.ready := req_helper.fire(io.requestor.req.valid)
  replayq.io.req.valid := req_helper.fire(replayq.io.req.ready)
  replayq.io.req.bits := io.requestor.req.bits

  val s0_req_fire = io.cache.req.fire()
  val s1_req_fire = Reg(next = s0_req_fire)
  val s2_req_fire = Reg(next = s1_req_fire)
  val s1_req_tag = Reg(next = io.cache.req.bits.tag)
  val s2_req_tag = Reg(next = s1_req_tag)
  val s2_kill = Reg(next = io.cache.s1_kill)

  io.cache.req <> req_arb.io.out
  io.cache.s1_kill := io.cache.s2_nack
  io.cache.s1_data.data := RegEnable(req_arb.io.out.bits.data, s0_req_fire)
  io.cache.s2_kill := false.B

  replayq.io.nack.valid := (io.cache.s2_nack || s2_kill) && s2_req_fire
  replayq.io.nack.bits := s2_req_tag
  replayq.io.resp := io.cache.resp
  io.requestor.resp := io.cache.resp

  assert(!RegNext(RegNext(io.cache.req.fire())) || !io.cache.s2_xcpt.asUInt.orR,
         "SimpleHellaCacheIF exception")
}
