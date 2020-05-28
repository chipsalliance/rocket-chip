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
  def access(touch_way: UInt): Unit
  def access(touch_ways: Seq[Valid[UInt]]): Unit
  def state_read: UInt
  def get_next_state(state: UInt, touch_way: UInt): UInt
  def get_next_state(state: UInt, touch_ways: Seq[Valid[UInt]]): UInt = {
    touch_ways.foldLeft(state)((prev, touch_way) => Mux(touch_way.valid, get_next_state(prev, touch_way.bits), prev))
  }
  def get_replace_way(state: UInt): UInt
}

object ReplacementPolicy {
  def fromString(s: String, n_ways: Int): ReplacementPolicy = s.toLowerCase match {
    case "random" => new RandomReplacement(n_ways)
    case "lru"    => new TrueLRU(n_ways)
    case "plru"   => new PseudoLRU(n_ways)
    case t => throw new IllegalArgumentException(s"unknown Replacement Policy type $t")
  }
}

class RandomReplacement(n_ways: Int) extends ReplacementPolicy {
  private val replace = Wire(Bool())
  replace := false.B
  def nBits = 16
  private val lfsr = LFSR(nBits, replace)
  def state_read = WireDefault(lfsr)

  def way = Random(n_ways, lfsr)
  def miss = replace := true.B
  def hit = {}
  def access(touch_way: UInt) = {}
  def access(touch_ways: Seq[Valid[UInt]]) = {}
  def get_next_state(state: UInt, touch_way: UInt) = 0.U //DontCare
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

  def get_next_state(state: UInt, touch_way: UInt): UInt = {
    val nextState     = Wire(Vec(n_ways-1, UInt(n_ways.W)))
    val moreRecentVec = extractMRUVec(state)  // reconstruct lower triangular matrix
    val wayDec        = UIntToOH(touch_way, n_ways)

    // Compute next value of triangular matrix
    // set the touched way as more recent than every other way
    nextState.zipWithIndex.map { case (e, i) =>
      e := Mux(i.U === touch_way, 0.U(n_ways.W), moreRecentVec(i) | wayDec)
    }

    nextState.zipWithIndex.tail.foldLeft((nextState.head.apply(n_ways-1,1),0)) { case ((pe,pi),(ce,ci)) => (Cat(ce.apply(n_ways-1,ci+1), pe), ci) }._1
  }

  def access(touch_way: UInt) {
    state_reg := get_next_state(state_reg, touch_way)
  }
  def access(touch_ways: Seq[Valid[UInt]]) {
    when (touch_ways.map(_.valid).orR) {
      state_reg := get_next_state(state_reg, touch_ways)
    }
    for (i <- 1 until touch_ways.size) {
      cover(PopCount(touch_ways.map(_.valid)) === i.U, s"LRU_UpdateCount$i", s"LRU Update $i simultaneous")
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
  // Pseudo-LRU tree algorithm: https://en.wikipedia.org/wiki/Pseudo-LRU#Tree-PLRU
  //
  //
  // - bits storage example for 4-way PLRU binary tree:
  //                  bit[2]: ways 3+2 older than ways 1+0
  //                  /                                  \
  //     bit[1]: way 3 older than way 2    bit[0]: way 1 older than way 0
  //
  //
  // - bits storage example for 3-way PLRU binary tree:
  //                  bit[1]: way 2 older than ways 1+0
  //                                                  \
  //                                       bit[0]: way 1 older than way 0
  //
  //
  // - bits storage example for 8-way PLRU binary tree:
  //                      bit[6]: ways 7-4 older than ways 3-0
  //                      /                                  \
  //            bit[5]: ways 7+6 > 5+4                bit[2]: ways 3+2 > 1+0
  //            /                    \                /                    \
  //     bit[4]: way 7>6    bit[3]: way 5>4    bit[1]: way 3>2    bit[0]: way 1>0

  def nBits = n_ways - 1
  private val state_reg = Reg(UInt(nBits.W))
  def state_read = WireDefault(state_reg)

  def access(touch_way: UInt) {
    state_reg := get_next_state(state_reg, touch_way)
  }
  def access(touch_ways: Seq[Valid[UInt]]) {
    when (touch_ways.map(_.valid).orR) {
      state_reg := get_next_state(state_reg, touch_ways)
    }
    for (i <- 1 until touch_ways.size) {
      cover(PopCount(touch_ways.map(_.valid)) === i.U, s"PLRU_UpdateCount$i", s"PLRU Update $i simultaneous")
    }
  }


  /** @param state state_reg bits for this sub-tree
    * @param touch_way touched way encoded value bits for this sub-tree
    * @param tree_nways number of ways in this sub-tree
    */
  def get_next_state(state: UInt, touch_way: UInt, tree_nways: Int): UInt = {
    require(state.getWidth == (tree_nways-1),                   s"wrong state bits width ${state.getWidth} for $tree_nways ways")
    require(touch_way.getWidth == (log2Ceil(tree_nways) max 1), s"wrong encoded way width ${touch_way.getWidth} for $tree_nways ways")

    if (tree_nways > 3) {
      // we are at a branching node in the tree with both left and right sub-trees, so recurse both sub-trees
      val right_nways: Int = 1 << (log2Ceil(tree_nways) - 1)  // number of ways in the right sub-tree
      val left_nways:  Int = tree_nways - right_nways         // number of ways in the left sub-tree
      val set_left_older      = !touch_way(log2Ceil(tree_nways)-1)
      val left_subtree_state  = state(tree_nways-3, right_nways-1)
      val right_subtree_state = state(right_nways-2, 0)

      Cat(set_left_older,
          Mux(set_left_older,
              left_subtree_state,  // if setting left sub-tree as older, do NOT recurse into left sub-tree
              get_next_state(left_subtree_state, touch_way(log2Ceil(left_nways)-1,0), left_nways)),  // recurse left if newer
          Mux(set_left_older,
              get_next_state(right_subtree_state, touch_way(log2Ceil(right_nways)-1,0), right_nways),  // recurse right if newer
              right_subtree_state))  // if setting right sub-tree as older, do NOT recurse into right sub-tree

    } else if (tree_nways == 3) {
      // we are at a branching node in the tree with only a right sub-tree, so recurse only right sub-tree
      val right_nways: Int = 1 << (log2Ceil(tree_nways) - 1)  // number of ways in the right sub-tree
      val set_left_older      = !touch_way(log2Ceil(tree_nways)-1)
      val right_subtree_state = state(right_nways-2, 0)

      Cat(set_left_older,
          Mux(set_left_older,
              get_next_state(right_subtree_state, touch_way(log2Ceil(right_nways)-1,0), right_nways),  // recurse right if newer
              right_subtree_state))  // if setting right sub-tree as older, do NOT recurse into right sub-tree

    } else if (tree_nways == 2) {
      // we are at a leaf node at the end of the tree, so set the single state bit opposite of the lsb of the touched way encoded value
      !touch_way(0)

    } else {  // tree_nways <= 1
      // we are at an empty node in an empty tree for 1 way, so return single zero bit for Chisel (no zero-width wires)
      0.U(1.W)
    }
  }

  def get_next_state(state: UInt, touch_way: UInt): UInt = get_next_state(state, touch_way, n_ways)


  /** @param state state_reg bits for this sub-tree
    * @param tree_nways number of ways in this sub-tree
    */
  def get_replace_way(state: UInt, tree_nways: Int): UInt = {
    require(state.getWidth == (tree_nways-1), s"wrong state bits width ${state.getWidth} for $tree_nways ways")

    // this algorithm recursively descends the binary tree, filling in the way-to-replace encoded value from msb to lsb
    if (tree_nways > 3) {
      // we are at a branching node in the tree with both left and right sub-trees, so recurse both sub-trees
      val right_nways: Int = 1 << (log2Ceil(tree_nways) - 1)  // number of ways in the right sub-tree
      val left_nways:  Int = tree_nways - right_nways         // number of ways in the left sub-tree
      val left_subtree_older  = state(tree_nways-2)
      val left_subtree_state  = state(tree_nways-3, right_nways-1)
      val right_subtree_state = state(right_nways-2, 0)

      Cat(left_subtree_older,      // return the top state bit (current tree node) as msb of the way-to-replace encoded value
          Mux(left_subtree_older,  // if left sub-tree is older, recurse left, else recurse right
              get_replace_way(left_subtree_state,  left_nways),    // recurse left
              get_replace_way(right_subtree_state, right_nways)))  // recurse right

    } else if (tree_nways == 3) {
      // we are at a branching node in the tree with only a right sub-tree, so recurse only right sub-tree
      val right_nways: Int = 1 << (log2Ceil(tree_nways) - 1)  // number of ways in the right sub-tree
      val left_subtree_older  = state(tree_nways-2)
      val right_subtree_state = state(right_nways-2, 0)

      Cat(left_subtree_older,      // return the top state bit (current tree node) as msb of the way-to-replace encoded value
          Mux(left_subtree_older,  // if left sub-tree is older, return and do not recurse right
              0.U(1.W),
              get_replace_way(right_subtree_state, right_nways)))  // recurse right

    } else if (tree_nways == 2) {
      // we are at a leaf node at the end of the tree, so just return the single state bit as lsb of the way-to-replace encoded value
      state(0)

    } else {  // tree_nways <= 1
      // we are at an empty node in an unbalanced tree for non-power-of-2 ways, so return single zero bit as lsb of the way-to-replace encoded value
      0.U(1.W)
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
    plru.get_next_state (state = state.U((n_ways-1).W), touch_way = way.U(log2Ceil(n_ways).W))))

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
