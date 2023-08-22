// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

/** This adapter prunes all user bit fields of the echo type from request messages,
  * storing them in queues and echoing them back when matching response messages are received.
  *
  * It also optionally rate limits the number of transactions that can be in flight simultaneously
  * per FIFO domain / A[W|R]ID.
  *
  * @param capMaxFlight is an optional maximum number of transactions that can be in flight per A[W|R]ID.
  */
class AXI4UserYanker(capMaxFlight: Option[Int] = None)(implicit p: Parameters) extends LazyModule
{
  val node = AXI4AdapterNode(
    masterFn = { mp => mp.copy(
      masters = mp.masters.map { m => m.copy(
        maxFlight = (m.maxFlight, capMaxFlight) match {
          case (Some(x), Some(y)) => Some(x min y)
          case (Some(x), None)    => Some(x)
          case (None,    Some(y)) => Some(y)
          case (None,    None)    => None })},
      echoFields = Nil)},
    slaveFn = { sp => sp })

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      // Which fields are we stripping?
      val echoFields = edgeIn.master.echoFields
      val need_bypass = edgeOut.slave.minLatency < 1

      edgeOut.master.masters.foreach { m =>
        require (m.maxFlight.isDefined, "UserYanker needs a flight cap on each ID")
      }

      def queue(id: Int) = {
        val depth = edgeOut.master.masters.find(_.id.contains(id)).flatMap(_.maxFlight).getOrElse(0)
        if (depth == 0) {
          Wire(new QueueIO(BundleMap(echoFields), 1)) // unused ID => undefined value
        } else {
          Module(new Queue(BundleMap(echoFields), depth, flow=need_bypass)).io
        }
      }

      val rqueues = Seq.tabulate(edgeIn.master.endId) { i => queue(i) }
      val wqueues = Seq.tabulate(edgeIn.master.endId) { i => queue(i) }

      val arid = in.ar.bits.id
      val ar_ready = VecInit(rqueues.map(_.enq.ready))(arid)
      in .ar.ready := out.ar.ready && ar_ready
      out.ar.valid := in .ar.valid && ar_ready
      Connectable.waiveUnmatched(out.ar.bits, in.ar.bits) match {
        case (lhs, rhs) => lhs :<= rhs
      }

      val rid = out.r.bits.id
      val r_valid = VecInit(rqueues.map(_.deq.valid))(rid)
      val r_bits = VecInit(rqueues.map(_.deq.bits))(rid)
      assert (!out.r.valid || r_valid) // Q must be ready faster than the response
      Connectable.waiveUnmatched(in.r, out.r) match {
        case (lhs, rhs) => lhs :<>= rhs
      }
      in.r.bits.echo :<= r_bits

      val arsel = UIntToOH(arid, edgeIn.master.endId).asBools
      val rsel  = UIntToOH(rid,  edgeIn.master.endId).asBools
      (rqueues zip (arsel zip rsel)) foreach { case (q, (ar, r)) =>
        q.deq.ready := out.r .valid && in .r .ready && r && out.r.bits.last
        q.deq.valid := DontCare
        q.deq.bits := DontCare
        q.enq.valid := in .ar.valid && out.ar.ready && ar
        q.enq.ready := DontCare
        q.enq.bits :<>= in.ar.bits.echo
        q.count := DontCare
      }

      val awid = in.aw.bits.id
      val aw_ready = VecInit(wqueues.map(_.enq.ready))(awid)
      in .aw.ready := out.aw.ready && aw_ready
      out.aw.valid := in .aw.valid && aw_ready
      Connectable.waiveUnmatched(out.aw.bits, in.aw.bits) match {
        case (lhs, rhs) => lhs :<>= rhs
      }

      val bid = out.b.bits.id
      val b_valid = VecInit(wqueues.map(_.deq.valid))(bid)
      val b_bits = VecInit(wqueues.map(_.deq.bits))(bid)
      assert (!out.b.valid || b_valid) // Q must be ready faster than the response
      Connectable.waiveUnmatched(in.b, out.b) match {
        case (lhs, rhs) => lhs :<>= rhs
      }
      in.b.bits.echo :<>= b_bits

      val awsel = UIntToOH(awid, edgeIn.master.endId).asBools
      val bsel  = UIntToOH(bid,  edgeIn.master.endId).asBools
      (wqueues zip (awsel zip bsel)) foreach { case (q, (aw, b)) =>
        q.deq.ready := out.b .valid && in .b .ready && b
        q.deq.valid := DontCare
        q.deq.bits := DontCare
        q.enq.valid := in .aw.valid && out.aw.ready && aw
        q.enq.ready := DontCare
        q.enq.bits :<>= in.aw.bits.echo
        q.count := DontCare
      }

      out.w :<>= in.w
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
