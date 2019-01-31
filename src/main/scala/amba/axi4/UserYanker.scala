// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.UIntToOH1

class AXI4UserYanker(capMaxFlight: Option[Int] = None)(implicit p: Parameters) extends LazyModule
{
  val node = AXI4AdapterNode(
    masterFn = { mp => mp.copy(
      userBits = 0,
      masters = mp.masters.map { m => m.copy(
        maxFlight = (m.maxFlight, capMaxFlight) match {
          case (Some(x), Some(y)) => Some(x min y)
          case (Some(x), None)    => Some(x)
          case (None,    Some(y)) => Some(y)
          case (None,    None)    => None })})},
    slaveFn = { sp => sp })

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val bits = edgeIn.bundle.userBits
      val need_bypass = edgeOut.slave.minLatency < 1
      require (bits > 0) // useless UserYanker!

      edgeOut.master.masters.foreach { m =>
        require (m.maxFlight.isDefined, "UserYanker needs a flight cap on each ID")
      }

      def queue(id: Int) = {
        val depth = edgeOut.master.masters.find(_.id.contains(id)).flatMap(_.maxFlight).getOrElse(0)
        if (depth == 0) {
          Wire(new QueueIO(UInt(width = bits), 1)) // unused ID => undefined value
        } else {
          Module(new Queue(UInt(width = bits), depth, flow=need_bypass)).io
        }
      }

      val rqueues = Seq.tabulate(edgeIn.master.endId) { i => queue(i) }
      val wqueues = Seq.tabulate(edgeIn.master.endId) { i => queue(i) }

      val arid = in.ar.bits.id
      val ar_ready = Vec(rqueues.map(_.enq.ready))(arid)
      in .ar.ready := out.ar.ready && ar_ready
      out.ar.valid := in .ar.valid && ar_ready
      out.ar.bits  := in .ar.bits

      val rid = out.r.bits.id
      val r_valid = Vec(rqueues.map(_.deq.valid))(rid)
      val r_bits = Vec(rqueues.map(_.deq.bits))(rid)
      assert (!out.r.valid || r_valid) // Q must be ready faster than the response
      in.r <> out.r
      in.r.bits.user.get := r_bits

      val arsel = UIntToOH(arid, edgeIn.master.endId).asBools
      val rsel  = UIntToOH(rid,  edgeIn.master.endId).asBools
      (rqueues zip (arsel zip rsel)) foreach { case (q, (ar, r)) =>
        q.deq.ready := out.r .valid && in .r .ready && r && out.r.bits.last
        q.enq.valid := in .ar.valid && out.ar.ready && ar
        q.enq.bits  := in.ar.bits.user.get
      }

      val awid = in.aw.bits.id
      val aw_ready = Vec(wqueues.map(_.enq.ready))(awid)
      in .aw.ready := out.aw.ready && aw_ready
      out.aw.valid := in .aw.valid && aw_ready
      out.aw.bits  := in .aw.bits

      val bid = out.b.bits.id
      val b_valid = Vec(wqueues.map(_.deq.valid))(bid)
      val b_bits = Vec(wqueues.map(_.deq.bits))(bid)
      assert (!out.b.valid || b_valid) // Q must be ready faster than the response
      in.b <> out.b
      in.b.bits.user.get := b_bits

      val awsel = UIntToOH(awid, edgeIn.master.endId).asBools
      val bsel  = UIntToOH(bid,  edgeIn.master.endId).asBools
      (wqueues zip (awsel zip bsel)) foreach { case (q, (aw, b)) =>
        q.deq.ready := out.b .valid && in .b .ready && b
        q.enq.valid := in .aw.valid && out.aw.ready && aw
        q.enq.bits  := in.aw.bits.user.get
      }

      out.w <> in.w
    }
  }
}

object AXI4UserYanker
{
  def apply(capMaxFlight: Option[Int] = None)(implicit p: Parameters): AXI4Node =
  {
    val axi4yank = LazyModule(new AXI4UserYanker(capMaxFlight))
    axi4yank.node
  }
}
