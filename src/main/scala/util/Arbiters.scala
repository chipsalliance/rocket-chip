// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import Chisel._
import freechips.rocketchip.config.Parameters

/** A generalized locking RR arbiter that addresses the limitations of the
 *  version in the Chisel standard library */
abstract class HellaLockingArbiter[T <: Data](typ: T, arbN: Int, rr: Boolean = false)
    extends Module {

  val io = new Bundle {
    val in = Vec(arbN, Decoupled(typ.cloneType)).flip
    val out = Decoupled(typ.cloneType)
  }

  def rotateLeft[T <: Data](norm: Vec[T], rot: UInt): Vec[T] = {
    val n = norm.size
    Vec.tabulate(n) { i =>
      Mux(rot < UInt(n - i), norm(UInt(i) + rot), norm(rot - UInt(n - i)))
    }
  }

  val lockIdx = Reg(init = UInt(0, log2Up(arbN)))
  val locked = Reg(init = Bool(false))

  val choice = if (rr) {
    PriorityMux(
      rotateLeft(Vec(io.in.map(_.valid)), lockIdx + UInt(1)),
      rotateLeft(Vec((0 until arbN).map(UInt(_))), lockIdx + UInt(1)))
  } else {
    PriorityEncoder(io.in.map(_.valid))
  }

  val chosen = Mux(locked, lockIdx, choice)

  for (i <- 0 until arbN) {
    io.in(i).ready := io.out.ready && chosen === UInt(i)
  }

  io.out.valid := io.in(chosen).valid
  io.out.bits := io.in(chosen).bits
}

/** This locking arbiter determines when it is safe to unlock
 *  by peeking at the data */
class HellaPeekingArbiter[T <: Data](
      typ: T, arbN: Int,
      canUnlock: T => Bool,
      needsLock: Option[T => Bool] = None,
      rr: Boolean = false)
    extends HellaLockingArbiter(typ, arbN, rr) {

  def realNeedsLock(data: T): Bool =
    needsLock.map(_(data)).getOrElse(Bool(true))

  when (io.out.fire()) {
    when (!locked && realNeedsLock(io.out.bits)) {
      lockIdx := choice
      locked := Bool(true)
    }
    // the unlock statement takes precedent
    when (canUnlock(io.out.bits)) {
      locked := Bool(false)
    }
  }
}

/** This arbiter determines when it is safe to unlock by counting transactions */
class HellaCountingArbiter[T <: Data](
      typ: T, arbN: Int, count: Int,
      val needsLock: Option[T => Bool] = None,
      rr: Boolean = false)
    extends HellaLockingArbiter(typ, arbN, rr) {

  def realNeedsLock(data: T): Bool =
    needsLock.map(_(data)).getOrElse(Bool(true))

  // if count is 1, you should use a non-locking arbiter
  require(count > 1, "CountingArbiter cannot have count <= 1")

  val lock_ctr = Counter(count)

  when (io.out.fire()) {
    when (!locked && realNeedsLock(io.out.bits)) {
      lockIdx := choice
      locked := Bool(true)
      lock_ctr.inc()
    }

    when (locked) {
      when (lock_ctr.inc()) { locked := Bool(false) }
    }
  }
}

/** This arbiter preserves the order of responses */
class InOrderArbiter[T <: Data, U <: Data](reqTyp: T, respTyp: U, n: Int)
    (implicit p: Parameters) extends Module {
  val io = new Bundle {
    val in_req = Vec(n, Decoupled(reqTyp)).flip
    val in_resp = Vec(n, Decoupled(respTyp))
    val out_req = Decoupled(reqTyp)
    val out_resp = Decoupled(respTyp).flip
  }

  if (n > 1) {
    val route_q = Module(new Queue(UInt(width = log2Up(n)), 2))
    val req_arb = Module(new RRArbiter(reqTyp, n))
    req_arb.io.in <> io.in_req

    val req_helper = DecoupledHelper(
      req_arb.io.out.valid,
      route_q.io.enq.ready,
      io.out_req.ready)

    io.out_req.bits := req_arb.io.out.bits
    io.out_req.valid := req_helper.fire(io.out_req.ready)

    route_q.io.enq.bits := req_arb.io.chosen
    route_q.io.enq.valid := req_helper.fire(route_q.io.enq.ready)

    req_arb.io.out.ready := req_helper.fire(req_arb.io.out.valid)

    val resp_sel = route_q.io.deq.bits
    val resp_ready = io.in_resp(resp_sel).ready
    val resp_helper = DecoupledHelper(
      resp_ready,
      route_q.io.deq.valid,
      io.out_resp.valid)

    val resp_valid = resp_helper.fire(resp_ready)
    for (i <- 0 until n) {
      io.in_resp(i).bits := io.out_resp.bits
      io.in_resp(i).valid := resp_valid && resp_sel === UInt(i)
    }

    route_q.io.deq.ready := resp_helper.fire(route_q.io.deq.valid)
    io.out_resp.ready := resp_helper.fire(io.out_resp.valid)
  } else {
    io.out_req <> io.in_req.head
    io.in_resp.head <> io.out_resp
  }
}
