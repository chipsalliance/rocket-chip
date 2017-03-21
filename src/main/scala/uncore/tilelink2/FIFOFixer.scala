// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._
import scala.math.max

class TLFIFOFixer(implicit p: Parameters) extends LazyModule
{
  // We request downstream FIFO so we can use the existing fifoId
  val node = TLAdapterNode(
    clientFn  = { cp => cp.copy(clients  = cp.clients .map(c => c.copy(requestFifo = !c.supportsProbe))) },
    managerFn = { mp => mp.copy(managers = mp.managers.map(m => m.copy(fifoId = Some(0)))) })

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      val maxId = edgeOut.manager.managers.flatMap(_.fifoId).foldLeft(0)(max)
      val a_id = edgeOut.manager.findFifoIdFast(in.a.bits.address)
      val a_nid = a_id === UInt(0) // no id = not FIFO

      val a_first = edgeIn.first(in.a)
      val d_first = edgeOut.first(out.d) && out.d.bits.opcode =/= TLMessages.ReleaseAck

      val stalls = edgeIn.client.clients.filter(c => c.requestFifo && c.sourceId.size > 1).map { c =>
        val a_sel = c.sourceId.contains(in.a.bits.source)
        val d_sel = c.sourceId.contains(in.d.bits.source)
        val id    = RegInit(UInt(0, width = log2Ceil(maxId+1)))
        val count = RegInit(UInt(0, width = log2Ceil(c.sourceId.size+1)))

        val a_inc = in.a.fire() && a_first && a_sel
        val d_dec = in.d.fire() && d_first && d_sel
        count := count + a_inc.asUInt - d_dec.asUInt
        when (in.a.fire() && a_sel) { id := a_id }

        a_sel && a_first && count =/= UInt(0) && (a_nid || id =/= a_id)
      }

      val stall = stalls.foldLeft(Bool(false))(_||_)

      out.a <> in.a
      in.d <> out.d
      out.a.valid := in.a.valid && !stall
      in.a.ready := out.a.ready && !stall

      if (edgeOut.manager.anySupportAcquireB && edgeOut.client.anySupportProbe) {
        in .b <> out.b
        out.c <> in .c
        out.e <> in .e
      } else {
        in.b.valid := Bool(false)
        in.c.ready := Bool(true)
        in.e.ready := Bool(true)
        out.b.ready := Bool(true)
        out.c.valid := Bool(false)
        out.e.valid := Bool(false)
      }
    }
  }
}

object TLFIFOFixer
{
  // applied to the TL source node; y.node := TLFIFOFixer()(x.node)
  def apply()(x: TLOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): TLOutwardNode = {
    val fixer = LazyModule(new TLFIFOFixer)
    fixer.node := x
    fixer.node
  }
}

