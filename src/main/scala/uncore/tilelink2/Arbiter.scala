// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import diplomacy._

object TLArbiter
{
  // (valids, granted) => readys
  type Policy = (Seq[Bool], Bool) => Seq[Bool]

  val lowestIndexFirst: Policy = (valids, granted) =>
    valids.scanLeft(Bool(true))(_ && !_).init

  def lowestFromSeq[T <: TLChannel](edge: TLEdge, sink: DecoupledIO[T], sources: Seq[DecoupledIO[T]]) {
    apply(lowestIndexFirst)(sink, sources.map(s => (edge.numBeats1(s.bits), s)):_*)
  }

  def lowest[T <: TLChannel](edge: TLEdge, sink: DecoupledIO[T], sources: DecoupledIO[T]*) {
    apply(lowestIndexFirst)(sink, sources.toList.map(s => (edge.numBeats1(s.bits), s)):_*)
  }

  def apply[T <: Data](policy: Policy)(sink: DecoupledIO[T], sources: (UInt, DecoupledIO[T])*) {
    if (sources.isEmpty) {
      sink.valid := Bool(false)
    } else {
      val pairs = sources.toList
      val beatsIn = pairs.map(_._1)
      val sourcesIn = pairs.map(_._2)

      // The number of beats which remain to be sent
      val beatsLeft = RegInit(UInt(0))
      val idle = beatsLeft === UInt(0)
      val latch = idle && sink.ready // winner (if any) claims sink

      // Who wants access to the sink?
      val valids = sourcesIn.map(_.valid)
      // Arbitrate amongst the requests
      val readys = Vec(policy(valids, latch))
      // Which request wins arbitration?
      val winner = Vec((readys zip valids) map { case (r,v) => r&&v })

      // Confirm the policy works properly
      require (readys.size == valids.size)
      // Never two winner
      val prefixOR = winner.scanLeft(Bool(false))(_||_).init
      assert((prefixOR zip winner) map { case (p,w) => !p || !w } reduce {_ && _})
      // If there was any request, there is a winner
      assert (!valids.reduce(_||_) || winner.reduce(_||_))

      // Track remaining beats
      val maskedBeats = (winner zip beatsIn) map { case (w,b) => Mux(w, b, UInt(0)) }
      val initBeats = maskedBeats.reduce(_ | _) // no winner => 0 beats
      beatsLeft := Mux(latch, initBeats, beatsLeft - sink.fire())

      // The one-hot source granted access in the previous cycle
      val state = RegInit(Vec.fill(sources.size)(Bool(false)))
      val muxState = Mux(idle, winner, state)
      state := muxState

      if (sources.size > 1) {
        val allowed = Mux(idle, readys, state)
        (sourcesIn zip allowed) foreach { case (s, r) =>
          s.ready := sink.ready && r
        }
      } else {
        sourcesIn(0).ready := sink.ready
      }

      sink.valid := Mux(idle, valids.reduce(_||_), Mux1H(state, valids))
      sink.bits := Mux1H(muxState, sourcesIn.map(_.bits))
    }
  }
}
