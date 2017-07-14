// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._

abstract class ReplacementPolicy {
  def way: UInt
  def miss: Unit
  def hit: Unit
}

class RandomReplacement(ways: Int) extends ReplacementPolicy {
  private val replace = Wire(Bool())
  replace := Bool(false)
  val lfsr = LFSR16(replace)

  def way = if(ways == 1) UInt(0) else lfsr(log2Up(ways)-1,0)
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
  require(isPow2(n))
  val state_reg = Reg(Bits(width = n))
  def access(way: UInt) {
    state_reg := get_next_state(state_reg,way)
  }
  def get_next_state(state: UInt, way: UInt) = {
    var next_state = state
    var idx = UInt(1,1)
    for (i <- log2Up(n)-1 to 0 by -1) {
      val bit = way(i)
      next_state = next_state.bitSet(idx, !bit)
      idx = Cat(idx, bit)
    }
    next_state
  }
  def replace = get_replace_way(state_reg)
  def get_replace_way(state: Bits) = {
    var idx = UInt(1,1)
    for (i <- 0 until log2Up(n))
      idx = Cat(idx, state(idx))
    idx(log2Up(n)-1,0)
  }
}

class SeqPLRU(n_sets: Int, n_ways: Int) extends SeqReplacementPolicy {
  val state = SeqMem(n_sets, Bits(width = n_ways-1))
  val logic = new PseudoLRU(n_ways)
  val current_state = Wire(Bits())
  val plru_way = logic.get_replace_way(current_state)
  val next_state = Wire(Bits())

  def access(set: UInt) = {
    current_state := Cat(state.read(set), Bits(0, width = 1))
  }

  def update(valid: Bool, hit: Bool, set: UInt, way: UInt) = {
    val update_way = Mux(hit, way, plru_way)
    next_state := logic.get_next_state(current_state, update_way)
    when (valid) { state.write(set, next_state(n_ways-1,1)) }
  }

  def way = plru_way
}
