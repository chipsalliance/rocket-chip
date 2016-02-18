package groundtest

import Chisel._

// ============
// Static timer
// ============

// Timer with a statically-specified period.

class Timer(initCount: Int) extends Module {
  val io = new Bundle {
    val start = Bool(INPUT)
    val stop = Bool(INPUT)
    val timeout = Bool(OUTPUT)
  }

  val countdown = Reg(UInt(width = log2Up(initCount)))
  val active = Reg(init = Bool(false))

  when (io.start) {
    countdown := UInt(initCount - 1)
    active := Bool(true)
  }

  when (io.stop) {
    active := Bool(false)
  }

  when (active) {
    countdown := countdown - UInt(1)
  }

  io.timeout := countdown === UInt(0) && active
}

object Timer {
  def apply(initCount: Int, start: Bool, stop: Bool): Bool = {
    val timer = Module(new Timer(initCount))
    timer.io.start := start
    timer.io.stop  := stop
    timer.io.timeout
  }
}

// =============
// Dynamic timer
// =============

// Timer with a dynamically-settable period.

class DynamicTimer(width: Int) extends Module {
  val io = new Bundle {
    val start   = Bool(INPUT)
    val period  = UInt(INPUT, width)
    val stop    = Bool(INPUT)
    val timeout = Bool(OUTPUT)
  }

  val countdown = Reg(init = UInt(0, width))
  val active = Reg(init = Bool(false))

  when (io.start) {
    countdown := io.period
    active := Bool(true)
  }
  .elsewhen (active) {
    countdown := countdown - UInt(1)
  }

  when (io.stop) {
    active := Bool(false)
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
  } 
  val state = Reg(UInt(width = 32))
  state := state * UInt(1103515245, 32) + UInt(12345, 32)
  io.out := state(30, 15)
} 
 
// ==========
// LCG module
// ==========

// An n-bit psuedo-random generator made from many instances of a
// 16-bit LCG.  Parameter 'width' must be larger than 0.

class LCG(val width : Int) extends Module {
  val io = new Bundle { 
    val out = UInt(OUTPUT, width) 
  } 
  require(width > 0)
  val numLCG16s : Int = (width+15)/16
  val outs = List.fill(numLCG16s)(Module(new LCG16()).io.out)
  io.out := Cat( outs(0)((width%16)-1, 0)
               , outs.drop(1) : _*)
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
    val randVal = Module(new LCG(log2Up(total))).io.out

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
