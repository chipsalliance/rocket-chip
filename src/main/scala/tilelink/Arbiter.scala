// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util._

object TLArbiter
{
  // (valids, select) => readys
  type Policy = (Integer, UInt, Bool) => UInt

  val lowestIndexFirst: Policy = (width, valids, select) => ~(leftOR(valids) << 1)(width-1, 0)

  val highestIndexFirst: Policy = (width, valids, select) => ~((rightOR(valids) >> 1).pad(width))

  val roundRobin: Policy = (width, valids, select) => if (width == 1) 1.U(1.W) else {
    val valid = valids(width-1, 0)
    assert (valid === valids)
    val mask = RegInit(((BigInt(1) << width)-1).U(width-1,0))
    val filter = Cat(valid & ~mask, valid)
    val unready = (rightOR(filter, width*2, width) >> 1) | (mask << width)
    val readys = ~((unready >> width) & unready(width-1, 0))
    when (select && valid.orR) {
      mask := leftOR(readys & valid, width)
    }
    readys(width-1, 0)
  }

  def lowestFromSeq[T <: TLChannel](edge: TLEdge, sink: DecoupledIO[T], sources: Seq[DecoupledIO[T]]): Unit = {
    apply(lowestIndexFirst)(sink, sources.map(s => (edge.numBeats1(s.bits), s)):_*)
  }

  def lowest[T <: TLChannel](edge: TLEdge, sink: DecoupledIO[T], sources: DecoupledIO[T]*): Unit = {
    apply(lowestIndexFirst)(sink, sources.toList.map(s => (edge.numBeats1(s.bits), s)):_*)
  }

  def highest[T <: TLChannel](edge: TLEdge, sink: DecoupledIO[T], sources: DecoupledIO[T]*): Unit = {
    apply(highestIndexFirst)(sink, sources.toList.map(s => (edge.numBeats1(s.bits), s)):_*)
  }

  def robin[T <: TLChannel](edge: TLEdge, sink: DecoupledIO[T], sources: DecoupledIO[T]*): Unit = {
    apply(roundRobin)(sink, sources.toList.map(s => (edge.numBeats1(s.bits), s)):_*)
  }

  def apply[T <: Data](policy: Policy)(sink: DecoupledIO[T], sources: (UInt, DecoupledIO[T])*): Unit = {
    if (sources.isEmpty) {
      sink.bits       := DontCare
    } else if (sources.size == 1) {
      sink :<>= sources.head._2
    } else {
      val pairs = sources.toList
      val beatsIn = pairs.map(_._1)
      val sourcesIn = pairs.map(_._2)

      // The number of beats which remain to be sent
      val beatsLeft = RegInit(0.U)
      val idle = beatsLeft === 0.U
      val latch = idle && sink.ready // winner (if any) claims sink

      // Who wants access to the sink?
      val valids = sourcesIn.map(_.valid)

      // Arbitrate amongst the requests
      val readys = VecInit(policy(valids.size, Cat(valids.reverse), latch).asBools)

      // Which request wins arbitration?
      val winner = VecInit((readys zip valids) map { case (r,v) => r&&v })

      // Confirm the policy works properly
      require (readys.size == valids.size)
      // Never two winners
      val prefixOR = winner.scanLeft(false.B)(_||_).init
      assert((prefixOR zip winner) map { case (p,w) => !p || !w } reduce {_ && _})
      // If there was any request, there is a winner
      assert (!valids.reduce(_||_) || winner.reduce(_||_))

      // Track remaining beats
      val maskedBeats = (winner zip beatsIn) map { case (w,b) => Mux(w, b, 0.U) }

      val initBeats = maskedBeats.reduce(_ | _) // no winner => 0 beats
      beatsLeft := Mux(latch, initBeats, beatsLeft - sink.fire)

      // The one-hot source granted access in the previous cycle
      val state = RegInit(VecInit(Seq.fill(sources.size)(false.B)))
      val muxState = Mux(idle, winner, state)
      state := muxState

      val allowed = Mux(idle, readys, state)
      (sourcesIn zip allowed) foreach { case (s, r) =>
        s.ready := sink.ready && r
      }
      sink.valid := Mux(idle, valids.reduce(_||_), Mux1H(state, valids))
      sink.bits :<= Mux1H(muxState, sourcesIn.map(_.bits))
    }
  }
}

// Synthesizable unit tests
import freechips.rocketchip.unittest._

abstract class DecoupledArbiterTest(
    policy: TLArbiter.Policy,
    txns: Int,
    timeout: Int,
    val numSources: Int,
    beatsLeftFromIdx: Int => UInt)
    (implicit p: Parameters) extends UnitTest(timeout)
{
  val sources = Wire(Vec(numSources, DecoupledIO(UInt(log2Ceil(numSources).W))))
  dontTouch(sources.suggestName("sources"))
  val sink = Wire(DecoupledIO(UInt(log2Ceil(numSources).W)))
  dontTouch(sink.suggestName("sink"))
  val count = RegInit(0.U(log2Ceil(txns).W))
  val lfsr = LFSR(16, true.B)

  sources.zipWithIndex.map { case (z, i) => z.bits := i.U }

  TLArbiter(policy)(sink, sources.zipWithIndex.map {
    case (z, i) => (beatsLeftFromIdx(i), z)
  }:_*)

  count := count + 1.U
  io.finished := count >= txns.U
}

/** This tests that when a specific pattern of source valids are driven,
  * a new index from amongst that pattern is always selected,
  * unless one of those sources takes multiple beats,
  * in which case the same index should be selected until the arbiter goes idle.
  */
class TLDecoupledArbiterRobinTest(txns: Int = 128, timeout: Int = 500000, print: Boolean = false)
                        (implicit p: Parameters)
    extends DecoupledArbiterTest(TLArbiter.roundRobin, txns, timeout, 6, i => i.U)
{
  val lastWinner = RegInit((numSources+1).U)
  val beatsLeft  = RegInit(0.U(log2Ceil(numSources).W))
  val first = lastWinner > numSources.U
  val valid = lfsr(0)
  val ready = lfsr(15)
  sink.ready := ready
  sources.zipWithIndex.map { // pattern: every even-indexed valid is driven the same random way
    case (s, i) => s.valid := (if (i % 2 == 1) false.B else valid)
  }

  when (sink.fire) {
    if (print) { printf("TestRobin: %d\n", sink.bits) }
    when (beatsLeft === 0.U) {
      assert(lastWinner =/= sink.bits, "Round robin did not pick a new idx despite one being valid.")
      lastWinner := sink.bits
      beatsLeft := sink.bits
    } .otherwise {
      assert(lastWinner === sink.bits, "Round robin did not pick the same index over multiple beats")
      beatsLeft := beatsLeft - 1.U
    }
  }
  if (print) {
    when (!sink.fire) { printf("TestRobin: idle (%d %d)\n", valid, ready) }
  }
}
/** This tests that the lowest index is always selected across random single cycle transactions. */
class TLDecoupledArbiterLowestTest(txns: Int = 128, timeout: Int = 500000)(implicit p: Parameters)
    extends DecoupledArbiterTest(TLArbiter.lowestIndexFirst, txns, timeout, 15, _ => 0.U)
{
  def assertLowest(id: Int): Unit = {
    when (sources(id).valid) {
      assert((numSources-1 until id by -1).map(!sources(_).fire).foldLeft(true.B)(_&&_), s"$id was valid but a higher valid source was granted ready.")
    }
  }

  sources.zipWithIndex.map { case (s, i) => s.valid := lfsr(i) }
  sink.ready := lfsr(15)
  when (sink.fire) { (0 until numSources).foreach(assertLowest(_)) }
}

/** This tests that the highest index is always selected across random single cycle transactions. */
class TLDecoupledArbiterHighestTest(txns: Int = 128, timeout: Int = 500000)(implicit p: Parameters)
    extends DecoupledArbiterTest(TLArbiter.highestIndexFirst, txns, timeout, 15, _ => 0.U)
{
  def assertHighest(id: Int): Unit = {
    when (sources(id).valid) {
      assert((0 until id).map(!sources(_).fire).foldLeft(true.B)(_&&_), s"$id was valid but a lower valid source was granted ready.")
    }
  }

  sources.zipWithIndex.map { case (s, i) => s.valid := lfsr(i) }
  sink.ready := lfsr(15)
  when (sink.fire) { (0 until numSources).foreach(assertHighest(_)) }
}
