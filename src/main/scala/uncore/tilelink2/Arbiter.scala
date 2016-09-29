// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.util.IrrevocableIO

object TLArbiter
{
  // (valids, idle) => readys
  type Policy = (Seq[Bool], Bool) => Seq[Bool]

  val lowestIndexFirst: Policy = (valids, idle) =>
    valids.scanLeft(Bool(true))(_ && !_).init

  def apply[T <: Data](policy: Policy)(sink: IrrevocableIO[T], sources: (UInt, IrrevocableIO[T])*) {
    if (sources.isEmpty) {
      sink.valid := Bool(false)
    } else {
      val pairs = sources.toList
      val beatsIn = pairs.map(_._1)
      val sourcesIn = pairs.map(_._2)

      // The number of beats which remain to be sent
      val beatsLeft = RegInit(UInt(0))
      val idle = beatsLeft === UInt(0)

      // Who wants access to the sink?
      val valids = sourcesIn.map(_.valid)
      // Arbitrate amongst the requests
      val readys = Vec(policy(valids, idle))
      // Which request wins arbitration?
      val winners = Vec((readys zip valids) map { case (r,v) => r&&v })

      // Confirm the policy works properly
      require (readys.size == valids.size)
      // Never two winners
      val prefixOR = winners.scanLeft(Bool(false))(_||_).init
      assert((prefixOR zip winners) map { case (p,w) => !p || !w } reduce {_ && _})
      // If there was any request, there is a winner
      assert (!valids.reduce(_||_) || winners.reduce(_||_))

      // Track remaining beats
      val maskedBeats = (winners zip beatsIn) map { case (w,b) => Mux(w, b, UInt(0)) }
      val initBeats = maskedBeats.reduce(_ | _) // no winner => 0 beats
      val todoBeats = Mux(idle, initBeats, beatsLeft)
      beatsLeft := todoBeats - sink.fire()
      assert (!sink.fire() || todoBeats =/= UInt(0)) // underflow is impoosible

      // The one-hot source granted access in the previous cycle
      val state = RegInit(Vec.fill(sources.size)(Bool(false)))
      val muxState = Mux(idle, winners, state)
      state := muxState

      val ones = Vec.fill(sources.size)(Bool(true))
      val picked = Mux(idle, ones, state)
      sink.valid := Mux1H(picked, valids)

      if (sources.size > 1) {
        val allowed = Mux(idle, readys, state)
        (sourcesIn zip allowed) foreach { case (s, r) =>
          s.ready := sink.ready && r
        }
      } else {
        sourcesIn(0).ready := sink.ready
      }

      sink.bits := Mux1H(muxState, sourcesIn.map(_.bits))
    }
  }
}
