// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

object TLArbiter
{
  // (valids, select) => readys
  type Policy = (Integer, UInt, Bool) => UInt

  val lowestIndexFirst: Policy = (width, valids, select) => ~(leftOR(valids) << 1)(width-1, 0)

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

  def lowestFromSeq[T <: TLChannel](edge: TLEdge, sink: DecoupledIO[T], sources: Seq[DecoupledIO[T]]) {
    apply(lowestIndexFirst)(sink, sources.map(s => (edge.numBeats1(s.bits), s)):_*)
  }

  def lowestFromSeq[T <: TLChannel](edge: TLEdge, sink: ReadyValidCancel[T], sources: Seq[ReadyValidCancel[T]]) {
    applyCancel(lowestIndexFirst)(sink, sources.map(s => (edge.numBeats1(s.bits), s)):_*)
  }

  def lowest[T <: TLChannel](edge: TLEdge, sink: DecoupledIO[T], sources: DecoupledIO[T]*) {
    apply(lowestIndexFirst)(sink, sources.toList.map(s => (edge.numBeats1(s.bits), s)):_*)
  }

  def lowest[T <: TLChannel](edge: TLEdge, sink: ReadyValidCancel[T], sources: ReadyValidCancel[T]*) {
    applyCancel(lowestIndexFirst)(sink, sources.toList.map(s => (edge.numBeats1(s.bits), s)):_*)
  }

  def robin[T <: TLChannel](edge: TLEdge, sink: DecoupledIO[T], sources: DecoupledIO[T]*) {
    apply(roundRobin)(sink, sources.toList.map(s => (edge.numBeats1(s.bits), s)):_*)
  }

  def robin[T <: TLChannel](edge: TLEdge, sink: ReadyValidCancel[T], sources: ReadyValidCancel[T]*) {
    applyCancel(roundRobin)(sink, sources.toList.map(s => (edge.numBeats1(s.bits), s)):_*)
  }

  def apply[T <: Data](policy: Policy)(sink: DecoupledIO[T], sources: (UInt, DecoupledIO[T])*) {
    val sink_ACancel = Wire(new ReadyValidCancel(chiselTypeOf(sink.bits)))
    val sources_ACancel = sources.map(s => (s._1, ReadyValidCancel(s._2)))
    applyCancel(policy = policy)(
      sink = sink_ACancel,
      sources = sources_ACancel:_*)
    sink :<> sink_ACancel.asDecoupled()
  }

  def applyCancel[T <: Data](policy: Policy)(sink: ReadyValidCancel[T], sources: (UInt, ReadyValidCancel[T])*) {
    if (sources.isEmpty) {
      sink.earlyValid := false.B
      sink.lateCancel := DontCare
      sink.bits       := DontCare
    } else if (sources.size == 1) {
      sink :<> sources.head._2
    } else {
      val pairs = sources.toList
      val beatsIn = pairs.map(_._1)
      val sourcesIn = pairs.map(_._2)

      // The number of beats which remain to be sent
      val beatsLeft = RegInit(0.U)
      val idle = beatsLeft === 0.U
      val latch = idle && sink.ready // winner (if any) claims sink

      // Who wants access to the sink?
      val earlyValids = sourcesIn.map(_.earlyValid)
      val validQuals  = sourcesIn.map(_.validQual)
      // Arbitrate amongst the requests
      val readys = VecInit(policy(earlyValids.size, Cat(earlyValids.reverse), latch).asBools)
      // Which request wins arbitration?
      val earlyWinner = VecInit((readys zip earlyValids) map { case (r,v) => r&&v })
      val winnerQual  = VecInit((readys zip validQuals)  map { case (r,v) => r&&v })

      // Confirm the policy works properly
      require (readys.size == earlyValids.size)
      require (readys.size == validQuals.size)
      // Never two winners
      val prefixOR = earlyWinner.scanLeft(false.B)(_||_).init
      assert((prefixOR zip earlyWinner) map { case (p,w) => !p || !w } reduce {_ && _})
      // If there was any request, there is a winner
      assert (!earlyValids.reduce(_||_) || earlyWinner.reduce(_||_))
      assert (!validQuals .reduce(_||_) || validQuals .reduce(_||_))

      // Track remaining beats
      val maskedBeats = (winnerQual zip beatsIn) map { case (w,b) => Mux(w, b, 0.U) }
      val initBeats = maskedBeats.reduce(_ | _) // no winner => 0 beats
      beatsLeft := Mux(latch, initBeats, beatsLeft - sink.fire())

      // The one-hot source granted access in the previous cycle
      val state = RegInit(VecInit(Seq.fill(sources.size)(false.B)))
      val muxStateEarly = Mux(idle, earlyWinner, state)
      val muxStateQual  = Mux(idle, winnerQual,  state)
      state := muxStateQual

      val allowed = Mux(idle, readys, state)
      (sourcesIn zip allowed) foreach { case (s, r) =>
        s.ready := sink.ready && r
      }
      sink.earlyValid := Mux(idle, earlyValids.reduce(_||_), Mux1H(state, earlyValids))
      sink.lateCancel := Mux1H(muxStateEarly, sourcesIn.map(_.lateCancel))
      sink.bits      :<= Mux1H(muxStateEarly, sourcesIn.map(_.bits))
    }
  }
}

/** Synthesizeable unit tests */
import freechips.rocketchip.unittest._

class TestRobin(txns: Int = 128, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val sources = Wire(Vec(6, DecoupledIO(UInt(3.W))))
  val sink = Wire(DecoupledIO(UInt(3.W)))
  val count = RegInit(0.U(8.W))

  val lfsr = LFSR(16, true.B)
  val valid = lfsr(0)
  val ready = lfsr(15)

  sources.zipWithIndex.map { case (z, i) => z.bits := i.U }
  sources(0).valid := valid
  sources(1).valid := false.B
  sources(2).valid := valid
  sources(3).valid := valid
  sources(4).valid := false.B
  sources(5).valid := valid
  sink.ready := ready

  TLArbiter(TLArbiter.roundRobin)(sink, sources.zipWithIndex.map { case (z, i) => (i.U, z) }:_*)
  when (sink.fire()) { printf("TestRobin: %d\n", sink.bits) }
  when (!sink.fire()) { printf("TestRobin: idle (%d %d)\n", valid, ready) }

  count := count + 1.U
  io.finished := count >= txns.U
}
