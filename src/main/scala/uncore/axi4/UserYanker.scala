// See LICENSE.SiFive for license details.

package uncore.axi4

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._
import uncore.tilelink2.UIntToOH1

class AXI4UserYanker(capMaxFlight: Option[Int] = None)(implicit p: Parameters) extends LazyModule
{
  // !!! make maxFlightPerId a cap and maxFlight a per AXI4 Master parameter
  val maxFlightPerId = capMaxFlight.getOrElse(8)
  require (maxFlightPerId >= 1)

  val node = AXI4AdapterNode(
    masterFn = { mp => mp.copy(maxFlight = maxFlightPerId, userBits = 0) },
    slaveFn = { sp => sp })

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      val bits = edgeIn.bundle.userBits
      val need_bypass = edgeOut.slave.minLatency < 1
      require (bits > 0) // useless UserYanker!

      val rqueues = Seq.fill(edgeIn.master.endId) { Module(new Queue(UInt(width = bits), maxFlightPerId, flow=need_bypass)) }
      val wqueues = Seq.fill(edgeIn.master.endId) { Module(new Queue(UInt(width = bits), maxFlightPerId, flow=need_bypass)) }

      val arid = in.ar.bits.id
      val ar_ready = Vec(rqueues.map(_.io.enq.ready))(arid)
      in .ar.ready := out.ar.ready && ar_ready
      out.ar.valid := in .ar.valid && ar_ready
      out.ar.bits  := in .ar.bits

      val rid = out.r.bits.id
      val r_valid = Vec(rqueues.map(_.io.deq.valid))(rid)
      val r_bits = Vec(rqueues.map(_.io.deq.bits))(rid)
      assert (!out.r.valid || r_valid) // Q must be ready faster than the response
      in.r <> out.r
      in.r.bits.user.get := r_bits

      val arsel = UIntToOH(arid, edgeIn.master.endId).toBools
      val rsel  = UIntToOH(rid,  edgeIn.master.endId).toBools
      (rqueues zip (arsel zip rsel)) foreach { case (q, (ar, r)) =>
        q.io.deq.ready := out.r .valid && in .r .ready && r && out.r.bits.last
        q.io.enq.valid := in .ar.valid && out.ar.ready && ar
        q.io.enq.bits  := in.ar.bits.user.get
      }

      val awid = in.aw.bits.id
      val aw_ready = Vec(wqueues.map(_.io.enq.ready))(awid)
      in .aw.ready := out.aw.ready && aw_ready
      out.aw.valid := in .aw.valid && aw_ready
      out.aw.bits  := in .aw.bits

      val bid = out.b.bits.id
      val b_valid = Vec(wqueues.map(_.io.deq.valid))(bid)
      val b_bits = Vec(wqueues.map(_.io.deq.bits))(bid)
      assert (!out.b.valid || b_valid) // Q must be ready faster than the response
      in.b <> out.b
      in.b.bits.user.get := b_bits

      val awsel = UIntToOH(awid, edgeIn.master.endId).toBools
      val bsel  = UIntToOH(bid,  edgeIn.master.endId).toBools
      (wqueues zip (awsel zip bsel)) foreach { case (q, (aw, b)) =>
        q.io.deq.ready := out.b .valid && in .b .ready && b
        q.io.enq.valid := in .aw.valid && out.aw.ready && aw
        q.io.enq.bits  := in.aw.bits.user.get
      }

      out.w <> in.w
    }
  }
}

object AXI4UserYanker
{
  // applied to the AXI4 source node; y.node := AXI4UserYanker(idBits, maxFlight)(x.node)
  def apply(capMaxFlight: Option[Int] = None)(x: AXI4OutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): AXI4OutwardNode = {
    val yanker = LazyModule(new AXI4UserYanker(capMaxFlight))
    yanker.node := x
    yanker.node
  }
}
