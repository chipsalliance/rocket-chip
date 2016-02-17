/// See LICENSE for license details.
package junctions
import Chisel._
import cde.Parameters

object bigIntPow2 {
  def apply(in: BigInt): Boolean = in > 0 && ((in & (in-1)) == 0)
}

class ParameterizedBundle(implicit p: Parameters) extends Bundle {
  override def cloneType = {
    try {
      this.getClass.getConstructors.head.newInstance(p).asInstanceOf[this.type]
    } catch {
      case e: java.lang.IllegalArgumentException =>
        throwException("Unable to use ParamaterizedBundle.cloneType on " +
                       this.getClass + ", probably because " + this.getClass +
                       "() takes more than one argument.  Consider overriding " +
                       "cloneType() on " + this.getClass, e)
    }
  }
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

class ReorderQueueWrite[T <: Data](dType: T, tagWidth: Int) extends Bundle {
  val data = dType.cloneType
  val tag = UInt(width = tagWidth)

  override def cloneType =
    new ReorderQueueWrite(dType, tagWidth).asInstanceOf[this.type]
}

class ReorderEnqueueIO[T <: Data](dType: T, tagWidth: Int)
  extends DecoupledIO(new ReorderQueueWrite(dType, tagWidth)) {

  override def cloneType =
    new ReorderEnqueueIO(dType, tagWidth).asInstanceOf[this.type]
}

class ReorderDequeueIO[T <: Data](dType: T, tagWidth: Int) extends Bundle {
  val valid = Bool(INPUT)
  val tag = UInt(INPUT, tagWidth)
  val data = dType.cloneType.asOutput
  val matches = Bool(OUTPUT)

  override def cloneType =
    new ReorderDequeueIO(dType, tagWidth).asInstanceOf[this.type]
}

class ReorderQueue[T <: Data](dType: T, tagWidth: Int, size: Int)
    extends Module {
  val io = new Bundle {
    val enq = new ReorderEnqueueIO(dType, tagWidth).flip
    val deq = new ReorderDequeueIO(dType, tagWidth)
  }

  val roq_data = Reg(Vec(size, dType.cloneType))
  val roq_tags = Reg(Vec(size, UInt(width = tagWidth)))
  val roq_free = Reg(init = Vec.fill(size)(Bool(true)))

  val roq_enq_addr = PriorityEncoder(roq_free)
  val roq_matches = roq_tags.zip(roq_free)
    .map { case (tag, free) => tag === io.deq.tag && !free }
  val roq_deq_addr = PriorityEncoder(roq_matches)

  io.enq.ready := roq_free.reduce(_ || _)
  io.deq.data := roq_data(roq_deq_addr)
  io.deq.matches := roq_matches.reduce(_ || _)

  when (io.enq.valid && io.enq.ready) {
    roq_data(roq_enq_addr) := io.enq.bits.data
    roq_tags(roq_enq_addr) := io.enq.bits.tag
    roq_free(roq_enq_addr) := Bool(false)
  }

  when (io.deq.valid) {
    roq_free(roq_deq_addr) := Bool(true)
  }
}

object DecoupledHelper {
  def apply(rvs: Bool*) = new DecoupledHelper(rvs)
}

class DecoupledHelper(val rvs: Seq[Bool]) {
  def fire(exclude: Bool, includes: Bool*) = {
    (rvs.filter(_ ne exclude) ++ includes).reduce(_ && _)
  }
}

class MultiWidthFifo(inW: Int, outW: Int, n: Int) extends Module {
  val io = new Bundle {
    val in = Decoupled(Bits(width = inW)).flip
    val out = Decoupled(Bits(width = outW))
    val count = UInt(OUTPUT, log2Up(n + 1))
  }

  if (inW == outW) {
    val q = Module(new Queue(Bits(width = inW), n))
    q.io.enq <> io.in
    io.out <> q.io.deq
    io.count := q.io.count
  } else if (inW > outW) {
    val nBeats = inW / outW

    require(inW % outW == 0, s"MultiWidthFifo: in: $inW not divisible by out: $outW")
    require(n % nBeats == 0, s"Cannot store $n output words when output beats is $nBeats")

    val wdata = Reg(Vec(n / nBeats, Bits(width = inW)))
    val rdata = Vec(wdata.flatMap { indat =>
      (0 until nBeats).map(i => indat(outW * (i + 1) - 1, outW * i)) })

    val head = Reg(init = UInt(0, log2Up(n / nBeats)))
    val tail = Reg(init = UInt(0, log2Up(n)))
    val size = Reg(init = UInt(0, log2Up(n + 1)))

    when (io.in.fire()) {
      wdata(head) := io.in.bits
      head := head + UInt(1)
    }

    when (io.out.fire()) { tail := tail + UInt(1) }

    size := MuxCase(size, Seq(
      (io.in.fire() && io.out.fire()) -> (size + UInt(nBeats - 1)),
      io.in.fire() -> (size + UInt(nBeats)),
      io.out.fire() -> (size - UInt(1))))

    io.out.valid := size > UInt(0)
    io.out.bits := rdata(tail)
    io.in.ready := size < UInt(n)
    io.count := size
  } else {
    val nBeats = outW / inW

    require(outW % inW == 0, s"MultiWidthFifo: out: $outW not divisible by in: $inW")

    val wdata = Reg(Vec(n * nBeats, Bits(width = inW)))
    val rdata = Vec.tabulate(n) { i =>
      Cat(wdata.slice(i * nBeats, (i + 1) * nBeats).reverse)}

    val head = Reg(init = UInt(0, log2Up(n * nBeats)))
    val tail = Reg(init = UInt(0, log2Up(n)))
    val size = Reg(init = UInt(0, log2Up(n * nBeats + 1)))

    when (io.in.fire()) {
      wdata(head) := io.in.bits
      head := head + UInt(1)
    }

    when (io.out.fire()) { tail := tail + UInt(1) }

    size := MuxCase(size, Seq(
      (io.in.fire() && io.out.fire()) -> (size - UInt(nBeats - 1)),
      io.in.fire() -> (size + UInt(1)),
      io.out.fire() -> (size - UInt(nBeats))))

    io.count := size >> UInt(log2Up(nBeats))
    io.out.valid := io.count > UInt(0)
    io.out.bits := rdata(tail)
    io.in.ready := size < UInt(n * nBeats)
  }
}
