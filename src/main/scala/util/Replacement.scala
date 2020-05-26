// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import chisel3.util.random.LFSR

abstract class ReplacementPolicy {
  def way: UInt
  def miss: Unit
  def hit: Unit
}

class RandomReplacement(ways: Int) extends ReplacementPolicy {
  private val replace = Wire(Bool())
  replace := Bool(false)
  val lfsr = LFSR(16, replace)

  def way = Random(ways, lfsr)
  def miss = replace := Bool(true)
  def hit = {}
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

class PseudoLRU(n: Int)
{
  //example bits storage format for 4-way PLRU:
  // bit[2] = way 3 older than way 2
  // bit[1] = way 1 older than way 0
  // bit[0] = ways 3-2 older than ways 1-0
  private val state_reg = Reg(UInt(width = n-1))
  def access(way: UInt) {
    state_reg := get_next_state(state_reg,way)
  }
  def access(ways: Seq[ValidIO[UInt]]) {
    state_reg := ways.foldLeft(state_reg)((prev, way) => Mux(way.valid, get_next_state(prev, way.bits), prev))
  }
  def get_next_state(state: UInt, way: UInt) = {
    var next_state = state << 1
    var idx = UInt(1,1)
    for (i <- log2Up(n)-1 to 0 by -1) {
      val bit = way(i)
      next_state = next_state.bitSet(idx, !bit)
      idx = Cat(idx, bit)
    }
    next_state.extract(n-1, 1)
  }
  def replace = get_replace_way(state_reg)
  def get_replace_way(state: UInt) = {
    val shifted_state = state << 1
    var idx = UInt(1,1)
    for (i <- log2Up(n)-1 to 0 by -1) {
      val in_bounds = Cat(idx, UInt(BigInt(1) << i))(log2Up(n)-1, 0) < UInt(n)
      idx = Cat(idx, in_bounds && shifted_state(idx))
    }
    idx(log2Up(n)-1,0)
  }
}

class SeqPLRU(n_sets: Int, n_ways: Int) extends SeqReplacementPolicy {
  val state = SeqMem(n_sets, UInt(width = n_ways-1))
  val logic = new PseudoLRU(n_ways)
  val current_state = Wire(UInt())
  val plru_way = logic.get_replace_way(current_state)
  val next_state = Wire(UInt())

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
  io.finished := RegNext(true.B, Bool(false))

  val get_replace_ways = (0 until (1 << (n_ways-1))).map(state =>
    plru.get_replace_way(state = UInt(state, width=n_ways-1)))
  val get_next_states  = (0 until (1 << (n_ways-1))).map(state => (0 until n_ways).map(way =>
    plru.get_next_state (state = UInt(state, width=n_ways-1), way = UInt(way, width=log2Ceil(n_ways)))))

  n_ways match {
    case 2 => {
      assert(get_replace_ways(0) === UInt(0, width=log2Ceil(n_ways)), s"get_replace_way state=0: expected=0 actual=%d", get_replace_ways(0))
      assert(get_replace_ways(1) === UInt(1, width=log2Ceil(n_ways)), s"get_replace_way state=1: expected=1 actual=%d", get_replace_ways(1))
      assert(get_next_states(0)(0) === UInt(1, width=n_ways-1), s"get_next_state state=0 way=0: expected=1 actual=%d", get_next_states(0)(0))
      assert(get_next_states(0)(1) === UInt(0, width=n_ways-1), s"get_next_state state=0 way=1: expected=0 actual=%d", get_next_states(0)(1))
      assert(get_next_states(1)(0) === UInt(1, width=n_ways-1), s"get_next_state state=1 way=0: expected=1 actual=%d", get_next_states(1)(0))
      assert(get_next_states(1)(1) === UInt(0, width=n_ways-1), s"get_next_state state=1 way=1: expected=0 actual=%d", get_next_states(1)(1))
    }
    case 4 => {
      assert(get_replace_ways(0) === UInt(0, width=log2Ceil(n_ways)), s"get_replace_way state=0: expected=0 actual=%d", get_replace_ways(0))
      assert(get_replace_ways(1) === UInt(2, width=log2Ceil(n_ways)), s"get_replace_way state=1: expected=2 actual=%d", get_replace_ways(1))
      assert(get_replace_ways(2) === UInt(1, width=log2Ceil(n_ways)), s"get_replace_way state=2: expected=1 actual=%d", get_replace_ways(2))
      assert(get_replace_ways(3) === UInt(2, width=log2Ceil(n_ways)), s"get_replace_way state=3: expected=2 actual=%d", get_replace_ways(3))
      assert(get_replace_ways(4) === UInt(0, width=log2Ceil(n_ways)), s"get_replace_way state=4: expected=0 actual=%d", get_replace_ways(4))
      assert(get_replace_ways(5) === UInt(3, width=log2Ceil(n_ways)), s"get_replace_way state=5: expected=3 actual=%d", get_replace_ways(5))
      assert(get_replace_ways(6) === UInt(1, width=log2Ceil(n_ways)), s"get_replace_way state=6: expected=1 actual=%d", get_replace_ways(6))
      assert(get_replace_ways(7) === UInt(3, width=log2Ceil(n_ways)), s"get_replace_way state=7: expected=3 actual=%d", get_replace_ways(7))
      assert(get_next_states(0)(0) === UInt(3, width=n_ways-1), s"get_next_state state=0 way=0: expected=3 actual=%d", get_next_states(0)(0))
      assert(get_next_states(0)(1) === UInt(1, width=n_ways-1), s"get_next_state state=0 way=1: expected=1 actual=%d", get_next_states(0)(1))
      assert(get_next_states(0)(2) === UInt(4, width=n_ways-1), s"get_next_state state=0 way=2: expected=4 actual=%d", get_next_states(0)(2))
      assert(get_next_states(0)(3) === UInt(0, width=n_ways-1), s"get_next_state state=0 way=3: expected=0 actual=%d", get_next_states(0)(3))
      assert(get_next_states(1)(0) === UInt(3, width=n_ways-1), s"get_next_state state=1 way=0: expected=3 actual=%d", get_next_states(1)(0))
      assert(get_next_states(1)(1) === UInt(1, width=n_ways-1), s"get_next_state state=1 way=1: expected=1 actual=%d", get_next_states(1)(1))
      assert(get_next_states(1)(2) === UInt(4, width=n_ways-1), s"get_next_state state=1 way=2: expected=4 actual=%d", get_next_states(1)(2))
      assert(get_next_states(1)(3) === UInt(0, width=n_ways-1), s"get_next_state state=1 way=3: expected=0 actual=%d", get_next_states(1)(3))
      assert(get_next_states(2)(0) === UInt(3, width=n_ways-1), s"get_next_state state=2 way=0: expected=3 actual=%d", get_next_states(2)(0))
      assert(get_next_states(2)(1) === UInt(1, width=n_ways-1), s"get_next_state state=2 way=1: expected=1 actual=%d", get_next_states(2)(1))
      assert(get_next_states(2)(2) === UInt(6, width=n_ways-1), s"get_next_state state=2 way=2: expected=6 actual=%d", get_next_states(2)(2))
      assert(get_next_states(2)(3) === UInt(2, width=n_ways-1), s"get_next_state state=2 way=3: expected=2 actual=%d", get_next_states(2)(3))
      assert(get_next_states(3)(0) === UInt(3, width=n_ways-1), s"get_next_state state=3 way=0: expected=3 actual=%d", get_next_states(3)(0))
      assert(get_next_states(3)(1) === UInt(1, width=n_ways-1), s"get_next_state state=3 way=1: expected=1 actual=%d", get_next_states(3)(1))
      assert(get_next_states(3)(2) === UInt(6, width=n_ways-1), s"get_next_state state=3 way=2: expected=6 actual=%d", get_next_states(3)(2))
      assert(get_next_states(3)(3) === UInt(2, width=n_ways-1), s"get_next_state state=3 way=3: expected=2 actual=%d", get_next_states(3)(3))
      assert(get_next_states(4)(0) === UInt(7, width=n_ways-1), s"get_next_state state=4 way=0: expected=7 actual=%d", get_next_states(4)(0))
      assert(get_next_states(4)(1) === UInt(5, width=n_ways-1), s"get_next_state state=4 way=1: expected=5 actual=%d", get_next_states(4)(1))
      assert(get_next_states(4)(2) === UInt(4, width=n_ways-1), s"get_next_state state=4 way=2: expected=4 actual=%d", get_next_states(4)(2))
      assert(get_next_states(4)(3) === UInt(0, width=n_ways-1), s"get_next_state state=4 way=3: expected=0 actual=%d", get_next_states(4)(3))
      assert(get_next_states(5)(0) === UInt(7, width=n_ways-1), s"get_next_state state=5 way=0: expected=7 actual=%d", get_next_states(5)(0))
      assert(get_next_states(5)(1) === UInt(5, width=n_ways-1), s"get_next_state state=5 way=1: expected=5 actual=%d", get_next_states(5)(1))
      assert(get_next_states(5)(2) === UInt(4, width=n_ways-1), s"get_next_state state=5 way=2: expected=4 actual=%d", get_next_states(5)(2))
      assert(get_next_states(5)(3) === UInt(0, width=n_ways-1), s"get_next_state state=5 way=3: expected=0 actual=%d", get_next_states(5)(3))
      assert(get_next_states(6)(0) === UInt(7, width=n_ways-1), s"get_next_state state=6 way=0: expected=7 actual=%d", get_next_states(6)(0))
      assert(get_next_states(6)(1) === UInt(5, width=n_ways-1), s"get_next_state state=6 way=1: expected=5 actual=%d", get_next_states(6)(1))
      assert(get_next_states(6)(2) === UInt(6, width=n_ways-1), s"get_next_state state=6 way=2: expected=6 actual=%d", get_next_states(6)(2))
      assert(get_next_states(6)(3) === UInt(2, width=n_ways-1), s"get_next_state state=6 way=3: expected=2 actual=%d", get_next_states(6)(3))
      assert(get_next_states(7)(0) === UInt(7, width=n_ways-1), s"get_next_state state=7 way=0: expected=7 actual=%d", get_next_states(7)(0))
      assert(get_next_states(7)(1) === UInt(5, width=n_ways-1), s"get_next_state state=7 way=5: expected=1 actual=%d", get_next_states(7)(1))
      assert(get_next_states(7)(2) === UInt(6, width=n_ways-1), s"get_next_state state=7 way=2: expected=6 actual=%d", get_next_states(7)(2))
      assert(get_next_states(7)(3) === UInt(2, width=n_ways-1), s"get_next_state state=7 way=3: expected=2 actual=%d", get_next_states(7)(3))
    }
    case _ => throw new IllegalArgumentException(s"no test pattern found for n_ways=$n_ways")
  }
}
