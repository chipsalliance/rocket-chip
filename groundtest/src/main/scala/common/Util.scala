package groundtest.common

import Chisel._

// ============
// Static timer
// ============

// Timer with a statically-specified period.
// Can take multiple inflight start-stop events with ID
// Will continue to count down as long as at least one event is inflight

class Timer(initCount: Int, maxInflight: Int) extends Module {
  val io = new Bundle {
    val start = Valid(UInt(width = log2Up(maxInflight))).flip
    val stop = Valid(UInt(width = log2Up(maxInflight))).flip
    val timeout = Valid(UInt(width = log2Up(maxInflight)))
  }

  val inflight = Reg(init = Vec.fill(maxInflight) { Bool(false) })
  val countdown = Reg(UInt(width = log2Up(initCount)))
  val active = inflight.reduce(_ || _)

  when (active) {
    countdown := countdown - UInt(1)
  }

  when (io.start.valid) {
    inflight(io.start.bits) := Bool(true)
    countdown := UInt(initCount - 1)
  }
  when (io.stop.valid) {
    inflight(io.stop.bits) := Bool(false)
  }

  io.timeout.valid := countdown === UInt(0) && active
  io.timeout.bits := PriorityEncoder(inflight)

  assert(!io.stop.valid || inflight(io.stop.bits),
         "Timer stop for transaction that's not inflight")
}

object Timer {
  def apply(initCount: Int, start: Bool, stop: Bool): Bool = {
    val timer = Module(new Timer(initCount, 1))
    timer.io.start.valid := start
    timer.io.start.bits  := UInt(0)
    timer.io.stop.valid  := stop
    timer.io.stop.bits   := UInt(0)
    timer.io.timeout.valid
  }
}

// =============
// Dynamic timer
// =============

// Timer with a dynamically-settable period.

class DynamicTimer(w: Int) extends Module {
  val io = new Bundle {
    val start   = Bool(INPUT)
    val period  = UInt(INPUT, w)
    val stop    = Bool(INPUT)
    val timeout = Bool(OUTPUT)
  }

  val countdown = Reg(init = UInt(0, w))
  val active = Reg(init = Bool(false))

  when (io.start) {
    countdown := io.period
    active := Bool(true)
  } .elsewhen (io.stop || countdown === UInt(0)) {
    active := Bool(false)
  } .elsewhen (active) {
    countdown := countdown - UInt(1)
  }

  io.timeout := countdown === UInt(0) && active
}

// ============
// LCG16 module
// ============

// A 16-bit psuedo-random generator based on a linear conguential
// generator (LCG).  The state is stored in an unitialised register.
// When using the C++ backend, it is straigtforward to arrange a
// random initial value for each uninitialised register, effectively
// seeding each LCG16 instance with a different seed.

class LCG16 extends Module { 
  val io = new Bundle { 
    val out = UInt(OUTPUT, 16) 
    val inc = Bool(INPUT)
  } 
  val state = Reg(UInt(width = 32))
  when (io.inc) {
    state := state * UInt(1103515245, 32) + UInt(12345, 32)
  }
  io.out := state(30, 15)
} 
 
// ==========
// LCG module
// ==========

// An n-bit psuedo-random generator made from many instances of a
// 16-bit LCG.  Parameter 'width' must be larger than 0.

class LCG(val w: Int) extends Module {
  val io = new Bundle { 
    val out = UInt(OUTPUT, w) 
    val inc = Bool(INPUT)
  } 
  require(w > 0)
  val numLCG16s : Int = (w+15)/16
  val outs = Seq.fill(numLCG16s) { LCG16(io.inc) }
  io.out := Cat(outs)
}

object LCG16 {
  def apply(inc: Bool = Bool(true)): UInt = {
    val lcg = Module(new LCG16)
    lcg.io.inc := inc
    lcg.io.out
  }
}

object LCG {
  def apply(w: Int, inc: Bool = Bool(true)): UInt = {
    val lcg = Module(new LCG(w))
    lcg.io.inc := inc
    lcg.io.out
  }
}

// ======================
// Frequency distribution
// ======================

// Given a list of (frequency, value) pairs, return a random value
// according to the frequency distribution.  The sum of the
// frequencies in the distribution must be a power of two.

object Frequency {
  def apply(dist : List[(Int, Bits)]) : Bits = {
    // Distribution must be non-empty
    require(dist.length > 0)

    // Require that the frequencies sum to a power of two
    val (freqs, vals) = dist.unzip
    val total = freqs.sum
    require(isPow2(total))

    // First item in the distribution
    val (firstFreq, firstVal) = dist.head

    // Result wire
    val result = Wire(Bits(width = firstVal.getWidth))
    result := UInt(0)

    // Random value
    val randVal = LCG(log2Up(total))

    // Pick return value
    var count = firstFreq
    var select = when (randVal < UInt(firstFreq)) { result := firstVal }
    for (p <- dist.drop(1)) {
      count = count + p._1
      select = select.elsewhen(randVal < UInt(count)) { result := p._2 }
    }

    return result
  }
}

object ValidMux {
  def apply[T <: Data](v1: ValidIO[T], v2: ValidIO[T]*): ValidIO[T] = {
    apply(v1 +: v2.toSeq)
  }
  def apply[T <: Data](valids: Seq[ValidIO[T]]): ValidIO[T] = {
    val out = Wire(Valid(valids.head.bits))
    out.valid := valids.map(_.valid).reduce(_ || _)
    out.bits := MuxCase(valids.head.bits,
      valids.map(v => (v.valid -> v.bits)))
    out
  }
}

object DebugCombiner {
  def apply(debugs: Seq[GroundTestStatus]): GroundTestStatus = {
    val out = Wire(new GroundTestStatus)
    out.finished := debugs.map(_.finished).reduce(_ || _)
    out.timeout  := ValidMux(debugs.map(_.timeout))
    out.error    := ValidMux(debugs.map(_.error))
    out
  }
}

/**
 * Takes in data on one decoupled interface and broadcasts it to
 * N decoupled output interfaces
 */
class Broadcaster[T <: Data](typ: T, n: Int) extends Module {
  val io = new Bundle {
    val in = Decoupled(typ).flip
    val out = Vec(n, Decoupled(typ))
  }

  require (n > 0)

  if (n == 1) {
    io.out.head <> io.in
  } else {
    val idx = Reg(init = UInt(0, log2Up(n)))
    val save = Reg(typ)

    io.out.head.valid := idx === UInt(0) && io.in.valid
    io.out.head.bits := io.in.bits
    for (i <- 1 until n) {
      io.out(i).valid := idx === UInt(i)
      io.out(i).bits := save
    }
    io.in.ready := io.out.head.ready && idx === UInt(0)

    when (io.in.fire()) { save := io.in.bits }

    when (io.out(idx).fire()) {
      when (idx === UInt(n - 1)) { idx := UInt(0) }
      .otherwise { idx := idx + UInt(1) }
    }
  }
}

object Broadcaster {
  def apply[T <: Data](in: DecoupledIO[T], n: Int): Vec[DecoupledIO[T]] = {
    val split = Module(new Broadcaster(in.bits, n))
    split.io.in <> in
    split.io.out
  }
}
