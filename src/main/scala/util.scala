/// See LICENSE for license details.
package junctions
import Chisel._
import cde.Parameters

object bigIntPow2 {
  def apply(in: BigInt): Boolean = in > 0 && ((in & (in-1)) == 0)
}

class ParameterizedBundle(implicit p: Parameters) extends Bundle {
  override def cloneType = this.getClass.getConstructors.head.newInstance(p).asInstanceOf[this.type]
}

class HellaFlowQueue[T <: Data](val entries: Int)(data: => T) extends Module {
  val io = new QueueIO(data, entries)
  require(entries > 1)

  val do_flow = Wire(Bool())
  val do_enq = io.enq.fire() && !do_flow
  val do_deq = io.deq.fire() && !do_flow

  val maybe_full = Reg(init=Bool(false))
  val enq_ptr = Counter(do_enq, entries)._1
  val (deq_ptr, deq_done) = Counter(do_deq, entries)
  when (do_enq =/= do_deq) { maybe_full := do_enq }

  val ptr_match = enq_ptr === deq_ptr
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val atLeastTwo = full || enq_ptr - deq_ptr >= UInt(2)
  do_flow := empty && io.deq.ready

  val ram = SeqMem(entries, data)
  when (do_enq) { ram.write(enq_ptr, io.enq.bits) }

  val ren = io.deq.ready && (atLeastTwo || !io.deq.valid && !empty)
  val raddr = Mux(io.deq.valid, Mux(deq_done, UInt(0), deq_ptr + UInt(1)), deq_ptr)
  val ram_out_valid = Reg(next = ren)

  io.deq.valid := Mux(empty, io.enq.valid, ram_out_valid)
  io.enq.ready := !full
  io.deq.bits := Mux(empty, io.enq.bits, ram.read(raddr, ren))
}

class HellaQueue[T <: Data](val entries: Int)(data: => T) extends Module {
  val io = new QueueIO(data, entries)

  val fq = Module(new HellaFlowQueue(entries)(data))
  fq.io.enq <> io.enq
  io.deq <> Queue(fq.io.deq, 1, pipe = true)
}

object HellaQueue {
  def apply[T <: Data](enq: DecoupledIO[T], entries: Int) = {
    val q = Module((new HellaQueue(entries)) { enq.bits })
    q.io.enq.valid := enq.valid // not using <> so that override is allowed
    q.io.enq.bits := enq.bits
    enq.ready := q.io.enq.ready
    q.io.deq
  }
}

/** A generalized locking RR arbiter that addresses the limitations of the
 *  version in the Chisel standard library */
abstract class JunctionsAbstractLockingArbiter[T <: Data](typ: T, arbN: Int)
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

  val choice = PriorityMux(
    rotateLeft(Vec(io.in.map(_.valid)), lockIdx + UInt(1)),
    rotateLeft(Vec((0 until arbN).map(UInt(_))), lockIdx + UInt(1)))

  val chosen = Mux(locked, lockIdx, choice)

  for (i <- 0 until arbN) {
    io.in(i).ready := io.out.ready && chosen === UInt(i)
  }

  io.out.valid := io.in(chosen).valid
  io.out.bits := io.in(chosen).bits
}

/** This locking arbiter determines when it is safe to unlock
 *  by peeking at the data */
class JunctionsPeekingArbiter[T <: Data](
    typ: T, arbN: Int,
    canUnlock: T => Bool,
    needsLock: Option[T => Bool] = None)
    extends JunctionsAbstractLockingArbiter(typ, arbN) {

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
class JunctionsCountingArbiter[T <: Data](
    typ: T, arbN: Int, count: Int,
    val needsLock: Option[T => Bool] = None)
    extends JunctionsAbstractLockingArbiter(typ, arbN) {

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
