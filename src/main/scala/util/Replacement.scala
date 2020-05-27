// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import freechips.rocketchip.util.property.cover

abstract class ReplacementPolicy {
  def nBits: Int
  def way: UInt
  def miss: Unit
  def hit: Unit
  def access(way: UInt): Unit
  def access(ways: Seq[Valid[UInt]]): Unit
  def state_read: UInt
  def get_next_state(state: UInt, way: UInt): UInt
  def get_next_state(state: UInt, ways: Seq[Valid[UInt]]): UInt = {
    ways.foldLeft(state)((prev, way) => Mux(way.valid, get_next_state(prev, way.bits), prev))
  }
  def get_replace_way(state: UInt): UInt
}

object ReplacementPolicy {
  def fromString(s: String, ways: Int): ReplacementPolicy = s.toLowerCase match {
    case "random" => new RandomReplacement(ways)
    case "lru"    => new TrueLRU(ways)
    case "plru"   => new PseudoLRU(ways)
    case t => throw new IllegalArgumentException(s"unknown Replacement Policy type $t")
  }
}

class RandomReplacement(ways: Int) extends ReplacementPolicy {
  private val replace = Wire(Bool())
  replace := false.B
  def nBits = 16
  private val lfsr = LFSR(nBits, replace)
  def state_read = WireDefault(lfsr)

  def way = Random(ways, lfsr)
  def miss = replace := true.B
  def hit = {}
  def access(way: UInt) = {}
  def access(ways: Seq[Valid[UInt]]) = {}
  def get_next_state(state: UInt, way: UInt) = 0.U //DontCare
  def get_replace_way(state: UInt) = way
}

abstract class SeqReplacementPolicy {
  def access(set: UInt): Unit
  def update(valid: Bool, hit: Bool, set: UInt, way: UInt): Unit
  def way: UInt
}

class SeqRandom(n_ways: Int) extends SeqReplacementPolicy {
  val logic = new RandomReplacement(n_ways)
  def access(set: UInt) = { }
  def update(valid: Bool, hit: Bool, set: UInt, way: UInt) = {
    when (valid && !hit) { logic.miss }
  }
  def way = logic.way
}

class TrueLRU(n_ways: Int) extends ReplacementPolicy {
  // True LRU replacement policy, using a triangular matrix to track which sets are more recently used than others.
  // The matrix is packed into a single UInt (or Bits).  Example 4-way (6-bits):
  // [5] - 3 more recent than 2
  // [4] - 3 more recent than 1
  // [3] - 2 more recent than 1
  // [2] - 3 more recent than 0
  // [1] - 2 more recent than 0
  // [0] - 1 more recent than 0
  def nBits = (n_ways * (n_ways-1)) / 2
  private val state_reg = RegInit(0.U(nBits.W))
  def state_read = WireDefault(state_reg)

  private def extractMRUVec(state: UInt): Seq[UInt] = {
    // Extract per-way information about which higher-indexed ways are more recently used
    val moreRecentVec = Wire(Vec(n_ways-1, UInt(n_ways.W)))
    var lsb = 0
    for (i <- 0 until n_ways-1) {
      moreRecentVec(i) := Cat(state(lsb+n_ways-i-2,lsb), 0.U((i+1).W))
      lsb = lsb + (n_ways - i - 1)
    }
    moreRecentVec
  }

  def get_next_state(state: UInt, way: UInt): UInt = {
    val nextState     = Wire(Vec(n_ways-1, UInt(n_ways.W)))
    val moreRecentVec = extractMRUVec(state)  // reconstruct lower triangular matrix
    val wayDec        = UIntToOH(way, n_ways)

    // Compute next value of triangular matrix
    // set the referenced way as more recent than every other way
    nextState.zipWithIndex.map { case (e, i) =>
      e := Mux(i.U === way, 0.U(n_ways.W), moreRecentVec(i) | wayDec)
    }

    nextState.zipWithIndex.tail.foldLeft((nextState.head.apply(n_ways-1,1),0)) { case ((pe,pi),(ce,ci)) => (Cat(ce.apply(n_ways-1,ci+1), pe), ci) }._1
  }

  def access(way: UInt) {
    state_reg := get_next_state(state_reg, way)
  }
  def access(ways: Seq[Valid[UInt]]) {
    when (ways.map(_.valid).orR) {
      state_reg := get_next_state(state_reg, ways)
    }
    for (i <- 1 until ways.size) {
      cover(PopCount(ways.map(_.valid)) === i.U, s"LRU_UpdateCount$i", s"LRU Update $i simultaneous")
    }
  }

  def get_replace_way(state: UInt): UInt = {
    val moreRecentVec = extractMRUVec(state)  // reconstruct lower triangular matrix
    // For each way, determine if all other ways are more recent
    val mruWayDec     = (0 until n_ways).map { i =>
      val upperMoreRecent = (if (i == n_ways-1) true.B else moreRecentVec(i).apply(n_ways-1,i+1).andR)
      val lowerMoreRecent = (if (i == 0)        true.B else moreRecentVec.map(e => !e(i)).reduce(_ && _))
      upperMoreRecent && lowerMoreRecent
    }
    OHToUInt(mruWayDec)
  }

  def way = get_replace_way(state_reg)
  def miss = access(way)
  def hit = {}
}

class PseudoLRU(n_ways: Int) extends ReplacementPolicy {
  //example bits storage format for 4-way PLRU:
  // [2] = ways 3-2 older than ways 1-0
  // [1] = way 3 older than way 2
  // [0] = way 1 older than way 0
  def nBits = n_ways - 1
  private val state_reg = Reg(UInt(nBits.W))
  def state_read = WireDefault(state_reg)

  def access(way: UInt) {
    state_reg := get_next_state(state_reg, way)
  }
  def access(ways: Seq[Valid[UInt]]) {
    when (ways.map(_.valid).orR) {
      state_reg := get_next_state(state_reg, ways)
    }
    for (i <- 1 until ways.size) {
      cover(PopCount(ways.map(_.valid)) === i.U, s"PLRU_UpdateCount$i", s"PLRU Update $i simultaneous")
    }
  }

  def get_next_state(state: UInt, way: UInt, this_ways: Int): UInt = {
    require(state.getWidth == (this_ways-1), s"wrong state bits width ${state.getWidth} for $this_ways ways")
    require(way.getWidth == log2Ceil(this_ways), s"wrong encoded way width ${way.getWidth} for $this_ways ways")
    if (this_ways > 2) {
      val half_ways: Int = 1 << (log2Ceil(this_ways) - 1)
      if (this_ways > 3) {
        Cat(!way(log2Ceil(this_ways)-1),
            Mux(way(log2Ceil(this_ways)-1),
                get_next_state(state(this_ways-3,half_ways-1), way(log2Ceil(this_ways-half_ways)-1,0), this_ways-half_ways),
                state(this_ways-3,half_ways-1)),
            Mux(way(log2Ceil(this_ways)-1),
                state(half_ways-2,0),
                get_next_state(state(half_ways-2,0), way(log2Ceil(half_ways)-1,0), half_ways)))
      } else {  // this_ways == 3
        Cat(!way(log2Ceil(this_ways)-1),
            Mux(way(log2Ceil(this_ways)-1),
                state(half_ways-2,0),
                get_next_state(state(half_ways-2,0), way(log2Ceil(half_ways)-1,0), half_ways)))
      }
    } else {  // this_ways <= 2
      !way(0)
    }
  }
  def get_next_state(state: UInt, way: UInt): UInt = get_next_state(state, way, n_ways)

  def get_replace_way(state: UInt, this_ways: Int): UInt = {
    require(state.getWidth == (this_ways-1), s"wrong state bits width ${state.getWidth} for $this_ways ways")
    if (this_ways > 2) {
      val half_ways: Int = 1 << (log2Ceil(this_ways) - 1)
      Cat(state(this_ways-2),
          Mux(state(this_ways-2),
              if (this_ways > 3) get_replace_way(state(this_ways-3,half_ways-1), this_ways-half_ways) else 0.U((log2Ceil(this_ways)-1).W),
              get_replace_way(state(half_ways-2,0), half_ways)))
    } else {  // this_ways <= 2
      state(0)
    }
  }
  def get_replace_way(state: UInt): UInt = get_replace_way(state, n_ways)

  def way = get_replace_way(state_reg)
  def miss = access(way)
  def hit = {}
}

class SeqPLRU(n_sets: Int, n_ways: Int) extends SeqReplacementPolicy {
  val logic = new PseudoLRU(n_ways)
  val state = SyncReadMem(n_sets, UInt(logic.nBits.W))
  val current_state = Wire(UInt(logic.nBits.W))
  val next_state    = Wire(UInt(logic.nBits.W))
  val plru_way = logic.get_replace_way(current_state)

  def access(set: UInt) = {
    current_state := state.read(set)
  }

  def update(valid: Bool, hit: Bool, set: UInt, way: UInt) = {
    val update_way = Mux(hit, way, plru_way)
    next_state := logic.get_next_state(current_state, update_way)
    when (valid) { state.write(set, next_state) }
  }

  def way = plru_way
}

/** Synthesizeable unit tests */
import freechips.rocketchip.unittest._

class PLRUTest(n_ways: Int, timeout: Int = 500) extends UnitTest(timeout) {
  val plru = new PseudoLRU(n_ways)

  // step
  io.finished := RegNext(true.B, false.B)

  val get_replace_ways = (0 until (1 << (n_ways-1))).map(state =>
    plru.get_replace_way(state = state.U((n_ways-1).W)))
  val get_next_states  = (0 until (1 << (n_ways-1))).map(state => (0 until n_ways).map(way =>
    plru.get_next_state (state = state.U((n_ways-1).W), way = way.U(log2Ceil(n_ways).W))))

  n_ways match {
    case 2 => {
      assert(get_replace_ways(0) === 0.U(log2Ceil(n_ways).W), s"get_replace_way state=0: expected=0 actual=%d", get_replace_ways(0))
      assert(get_replace_ways(1) === 1.U(log2Ceil(n_ways).W), s"get_replace_way state=1: expected=1 actual=%d", get_replace_ways(1))
      assert(get_next_states(0)(0) === 1.U(plru.nBits.W), s"get_next_state state=0 way=0: expected=1 actual=%d", get_next_states(0)(0))
      assert(get_next_states(0)(1) === 0.U(plru.nBits.W), s"get_next_state state=0 way=1: expected=0 actual=%d", get_next_states(0)(1))
      assert(get_next_states(1)(0) === 1.U(plru.nBits.W), s"get_next_state state=1 way=0: expected=1 actual=%d", get_next_states(1)(0))
      assert(get_next_states(1)(1) === 0.U(plru.nBits.W), s"get_next_state state=1 way=1: expected=0 actual=%d", get_next_states(1)(1))
    }
    case 3 => {
      assert(get_replace_ways(0) === 0.U(log2Ceil(n_ways).W), s"get_replace_way state=0: expected=0 actual=%d", get_replace_ways(0))
      assert(get_replace_ways(1) === 1.U(log2Ceil(n_ways).W), s"get_replace_way state=1: expected=1 actual=%d", get_replace_ways(1))
      assert(get_replace_ways(2) === 2.U(log2Ceil(n_ways).W), s"get_replace_way state=2: expected=2 actual=%d", get_replace_ways(2))
      assert(get_replace_ways(3) === 2.U(log2Ceil(n_ways).W), s"get_replace_way state=3: expected=2 actual=%d", get_replace_ways(3))
      assert(get_next_states(0)(0) === 3.U(plru.nBits.W), s"get_next_state state=0 way=0: expected=3 actual=%d", get_next_states(0)(0))
      assert(get_next_states(0)(1) === 2.U(plru.nBits.W), s"get_next_state state=0 way=1: expected=2 actual=%d", get_next_states(0)(1))
      assert(get_next_states(0)(2) === 0.U(plru.nBits.W), s"get_next_state state=0 way=2: expected=0 actual=%d", get_next_states(0)(2))
      assert(get_next_states(1)(0) === 3.U(plru.nBits.W), s"get_next_state state=1 way=0: expected=3 actual=%d", get_next_states(1)(0))
      assert(get_next_states(1)(1) === 2.U(plru.nBits.W), s"get_next_state state=1 way=1: expected=2 actual=%d", get_next_states(1)(1))
      assert(get_next_states(1)(2) === 1.U(plru.nBits.W), s"get_next_state state=1 way=2: expected=1 actual=%d", get_next_states(1)(2))
      assert(get_next_states(2)(0) === 3.U(plru.nBits.W), s"get_next_state state=2 way=0: expected=3 actual=%d", get_next_states(2)(0))
      assert(get_next_states(2)(1) === 2.U(plru.nBits.W), s"get_next_state state=2 way=1: expected=2 actual=%d", get_next_states(2)(1))
      assert(get_next_states(2)(2) === 0.U(plru.nBits.W), s"get_next_state state=2 way=2: expected=0 actual=%d", get_next_states(2)(2))
      assert(get_next_states(3)(0) === 3.U(plru.nBits.W), s"get_next_state state=3 way=0: expected=3 actual=%d", get_next_states(3)(0))
      assert(get_next_states(3)(1) === 2.U(plru.nBits.W), s"get_next_state state=3 way=1: expected=2 actual=%d", get_next_states(3)(1))
      assert(get_next_states(3)(2) === 1.U(plru.nBits.W), s"get_next_state state=3 way=2: expected=1 actual=%d", get_next_states(3)(2))
    }
    case 4 => {
      assert(get_replace_ways(0) === 0.U(log2Ceil(n_ways).W), s"get_replace_way state=0: expected=0 actual=%d", get_replace_ways(0))
      assert(get_replace_ways(1) === 1.U(log2Ceil(n_ways).W), s"get_replace_way state=1: expected=1 actual=%d", get_replace_ways(1))
      assert(get_replace_ways(2) === 0.U(log2Ceil(n_ways).W), s"get_replace_way state=2: expected=0 actual=%d", get_replace_ways(2))
      assert(get_replace_ways(3) === 1.U(log2Ceil(n_ways).W), s"get_replace_way state=3: expected=1 actual=%d", get_replace_ways(3))
      assert(get_replace_ways(4) === 2.U(log2Ceil(n_ways).W), s"get_replace_way state=4: expected=2 actual=%d", get_replace_ways(4))
      assert(get_replace_ways(5) === 2.U(log2Ceil(n_ways).W), s"get_replace_way state=5: expected=2 actual=%d", get_replace_ways(5))
      assert(get_replace_ways(6) === 3.U(log2Ceil(n_ways).W), s"get_replace_way state=6: expected=3 actual=%d", get_replace_ways(6))
      assert(get_replace_ways(7) === 3.U(log2Ceil(n_ways).W), s"get_replace_way state=7: expected=3 actual=%d", get_replace_ways(7))
      assert(get_next_states(0)(0) === 5.U(plru.nBits.W), s"get_next_state state=0 way=0: expected=5 actual=%d", get_next_states(0)(0))
      assert(get_next_states(0)(1) === 4.U(plru.nBits.W), s"get_next_state state=0 way=1: expected=4 actual=%d", get_next_states(0)(1))
      assert(get_next_states(0)(2) === 2.U(plru.nBits.W), s"get_next_state state=0 way=2: expected=2 actual=%d", get_next_states(0)(2))
      assert(get_next_states(0)(3) === 0.U(plru.nBits.W), s"get_next_state state=0 way=3: expected=0 actual=%d", get_next_states(0)(3))
      assert(get_next_states(1)(0) === 5.U(plru.nBits.W), s"get_next_state state=1 way=0: expected=5 actual=%d", get_next_states(1)(0))
      assert(get_next_states(1)(1) === 4.U(plru.nBits.W), s"get_next_state state=1 way=1: expected=4 actual=%d", get_next_states(1)(1))
      assert(get_next_states(1)(2) === 3.U(plru.nBits.W), s"get_next_state state=1 way=2: expected=3 actual=%d", get_next_states(1)(2))
      assert(get_next_states(1)(3) === 1.U(plru.nBits.W), s"get_next_state state=1 way=3: expected=1 actual=%d", get_next_states(1)(3))
      assert(get_next_states(2)(0) === 7.U(plru.nBits.W), s"get_next_state state=2 way=0: expected=7 actual=%d", get_next_states(2)(0))
      assert(get_next_states(2)(1) === 6.U(plru.nBits.W), s"get_next_state state=2 way=1: expected=6 actual=%d", get_next_states(2)(1))
      assert(get_next_states(2)(2) === 2.U(plru.nBits.W), s"get_next_state state=2 way=2: expected=2 actual=%d", get_next_states(2)(2))
      assert(get_next_states(2)(3) === 0.U(plru.nBits.W), s"get_next_state state=2 way=3: expected=0 actual=%d", get_next_states(2)(3))
      assert(get_next_states(3)(0) === 7.U(plru.nBits.W), s"get_next_state state=3 way=0: expected=7 actual=%d", get_next_states(3)(0))
      assert(get_next_states(3)(1) === 6.U(plru.nBits.W), s"get_next_state state=3 way=1: expected=6 actual=%d", get_next_states(3)(1))
      assert(get_next_states(3)(2) === 3.U(plru.nBits.W), s"get_next_state state=3 way=2: expected=3 actual=%d", get_next_states(3)(2))
      assert(get_next_states(3)(3) === 1.U(plru.nBits.W), s"get_next_state state=3 way=3: expected=1 actual=%d", get_next_states(3)(3))
      assert(get_next_states(4)(0) === 5.U(plru.nBits.W), s"get_next_state state=4 way=0: expected=5 actual=%d", get_next_states(4)(0))
      assert(get_next_states(4)(1) === 4.U(plru.nBits.W), s"get_next_state state=4 way=1: expected=4 actual=%d", get_next_states(4)(1))
      assert(get_next_states(4)(2) === 2.U(plru.nBits.W), s"get_next_state state=4 way=2: expected=2 actual=%d", get_next_states(4)(2))
      assert(get_next_states(4)(3) === 0.U(plru.nBits.W), s"get_next_state state=4 way=3: expected=0 actual=%d", get_next_states(4)(3))
      assert(get_next_states(5)(0) === 5.U(plru.nBits.W), s"get_next_state state=5 way=0: expected=5 actual=%d", get_next_states(5)(0))
      assert(get_next_states(5)(1) === 4.U(plru.nBits.W), s"get_next_state state=5 way=1: expected=4 actual=%d", get_next_states(5)(1))
      assert(get_next_states(5)(2) === 3.U(plru.nBits.W), s"get_next_state state=5 way=2: expected=3 actual=%d", get_next_states(5)(2))
      assert(get_next_states(5)(3) === 1.U(plru.nBits.W), s"get_next_state state=5 way=3: expected=1 actual=%d", get_next_states(5)(3))
      assert(get_next_states(6)(0) === 7.U(plru.nBits.W), s"get_next_state state=6 way=0: expected=7 actual=%d", get_next_states(6)(0))
      assert(get_next_states(6)(1) === 6.U(plru.nBits.W), s"get_next_state state=6 way=1: expected=6 actual=%d", get_next_states(6)(1))
      assert(get_next_states(6)(2) === 2.U(plru.nBits.W), s"get_next_state state=6 way=2: expected=2 actual=%d", get_next_states(6)(2))
      assert(get_next_states(6)(3) === 0.U(plru.nBits.W), s"get_next_state state=6 way=3: expected=0 actual=%d", get_next_states(6)(3))
      assert(get_next_states(7)(0) === 7.U(plru.nBits.W), s"get_next_state state=7 way=0: expected=7 actual=%d", get_next_states(7)(0))
      assert(get_next_states(7)(1) === 6.U(plru.nBits.W), s"get_next_state state=7 way=5: expected=6 actual=%d", get_next_states(7)(1))
      assert(get_next_states(7)(2) === 3.U(plru.nBits.W), s"get_next_state state=7 way=2: expected=3 actual=%d", get_next_states(7)(2))
      assert(get_next_states(7)(3) === 1.U(plru.nBits.W), s"get_next_state state=7 way=3: expected=1 actual=%d", get_next_states(7)(3))
    }
    case 6 => {
      assert(get_replace_ways( 0) === 0.U(log2Ceil(n_ways).W), s"get_replace_way state=00: expected=0 actual=%d", get_replace_ways( 0))
      assert(get_replace_ways( 1) === 1.U(log2Ceil(n_ways).W), s"get_replace_way state=01: expected=1 actual=%d", get_replace_ways( 1))
      assert(get_replace_ways( 2) === 0.U(log2Ceil(n_ways).W), s"get_replace_way state=02: expected=0 actual=%d", get_replace_ways( 2))
      assert(get_replace_ways( 3) === 1.U(log2Ceil(n_ways).W), s"get_replace_way state=03: expected=1 actual=%d", get_replace_ways( 3))
      assert(get_replace_ways( 4) === 2.U(log2Ceil(n_ways).W), s"get_replace_way state=04: expected=2 actual=%d", get_replace_ways( 4))
      assert(get_replace_ways( 5) === 2.U(log2Ceil(n_ways).W), s"get_replace_way state=05: expected=2 actual=%d", get_replace_ways( 5))
      assert(get_replace_ways( 6) === 3.U(log2Ceil(n_ways).W), s"get_replace_way state=06: expected=3 actual=%d", get_replace_ways( 6))
      assert(get_replace_ways( 7) === 3.U(log2Ceil(n_ways).W), s"get_replace_way state=07: expected=3 actual=%d", get_replace_ways( 7))
      assert(get_replace_ways( 8) === 0.U(log2Ceil(n_ways).W), s"get_replace_way state=08: expected=0 actual=%d", get_replace_ways( 8))
      assert(get_replace_ways( 9) === 1.U(log2Ceil(n_ways).W), s"get_replace_way state=09: expected=1 actual=%d", get_replace_ways( 9))
      assert(get_replace_ways(10) === 0.U(log2Ceil(n_ways).W), s"get_replace_way state=10: expected=0 actual=%d", get_replace_ways(10))
      assert(get_replace_ways(11) === 1.U(log2Ceil(n_ways).W), s"get_replace_way state=11: expected=1 actual=%d", get_replace_ways(11))
      assert(get_replace_ways(12) === 2.U(log2Ceil(n_ways).W), s"get_replace_way state=12: expected=2 actual=%d", get_replace_ways(12))
      assert(get_replace_ways(13) === 2.U(log2Ceil(n_ways).W), s"get_replace_way state=13: expected=2 actual=%d", get_replace_ways(13))
      assert(get_replace_ways(14) === 3.U(log2Ceil(n_ways).W), s"get_replace_way state=14: expected=3 actual=%d", get_replace_ways(14))
      assert(get_replace_ways(15) === 3.U(log2Ceil(n_ways).W), s"get_replace_way state=15: expected=3 actual=%d", get_replace_ways(15))
      assert(get_replace_ways(16) === 4.U(log2Ceil(n_ways).W), s"get_replace_way state=16: expected=4 actual=%d", get_replace_ways(16))
      assert(get_replace_ways(17) === 4.U(log2Ceil(n_ways).W), s"get_replace_way state=17: expected=4 actual=%d", get_replace_ways(17))
      assert(get_replace_ways(18) === 4.U(log2Ceil(n_ways).W), s"get_replace_way state=18: expected=4 actual=%d", get_replace_ways(18))
      assert(get_replace_ways(19) === 4.U(log2Ceil(n_ways).W), s"get_replace_way state=19: expected=4 actual=%d", get_replace_ways(19))
      assert(get_replace_ways(20) === 4.U(log2Ceil(n_ways).W), s"get_replace_way state=20: expected=4 actual=%d", get_replace_ways(20))
      assert(get_replace_ways(21) === 4.U(log2Ceil(n_ways).W), s"get_replace_way state=21: expected=4 actual=%d", get_replace_ways(21))
      assert(get_replace_ways(22) === 4.U(log2Ceil(n_ways).W), s"get_replace_way state=22: expected=4 actual=%d", get_replace_ways(22))
      assert(get_replace_ways(23) === 4.U(log2Ceil(n_ways).W), s"get_replace_way state=23: expected=4 actual=%d", get_replace_ways(23))
      assert(get_replace_ways(24) === 5.U(log2Ceil(n_ways).W), s"get_replace_way state=24: expected=5 actual=%d", get_replace_ways(24))
      assert(get_replace_ways(25) === 5.U(log2Ceil(n_ways).W), s"get_replace_way state=25: expected=5 actual=%d", get_replace_ways(25))
      assert(get_replace_ways(26) === 5.U(log2Ceil(n_ways).W), s"get_replace_way state=26: expected=5 actual=%d", get_replace_ways(26))
      assert(get_replace_ways(27) === 5.U(log2Ceil(n_ways).W), s"get_replace_way state=27: expected=5 actual=%d", get_replace_ways(27))
      assert(get_replace_ways(28) === 5.U(log2Ceil(n_ways).W), s"get_replace_way state=28: expected=5 actual=%d", get_replace_ways(28))
      assert(get_replace_ways(29) === 5.U(log2Ceil(n_ways).W), s"get_replace_way state=29: expected=5 actual=%d", get_replace_ways(29))
      assert(get_replace_ways(30) === 5.U(log2Ceil(n_ways).W), s"get_replace_way state=30: expected=5 actual=%d", get_replace_ways(30))
      assert(get_replace_ways(31) === 5.U(log2Ceil(n_ways).W), s"get_replace_way state=31: expected=5 actual=%d", get_replace_ways(31))
    }
    case _ => throw new IllegalArgumentException(s"no test pattern found for n_ways=$n_ways")
  }
}
