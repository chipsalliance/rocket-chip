// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._
import uncore.apb._
import scala.math.{min, max}
import APBParameters._

case class TLToAPBNode() extends MixedAdapterNode(TLImp, APBImp)(
  dFn = { case TLClientPortParameters(clients, unsafeAtomics, minLatency) =>
    val masters = clients.map { case c => APBMasterParameters(nodePath = c.nodePath) }
    APBMasterPortParameters(masters)
  },
  uFn = { case APBSlavePortParameters(slaves, beatBytes) =>
    val managers = slaves.map { case s =>
      TLManagerParameters(
        address            = s.address,
        resources          = s.resources,
        regionType         = s.regionType,
        executable         = s.executable,
        nodePath           = s.nodePath,
        supportsGet        = if (s.supportsRead)  TransferSizes(1, beatBytes) else TransferSizes.none,
        supportsPutPartial = if (s.supportsWrite) TransferSizes(1, beatBytes) else TransferSizes.none,
        supportsPutFull    = if (s.supportsWrite) TransferSizes(1, beatBytes) else TransferSizes.none,
        fifoId             = Some(0)) // a common FIFO domain
    }
    TLManagerPortParameters(managers, beatBytes, 1, 0)
  })

class TLToAPB(combinational: Boolean = true)(implicit p: Parameters) extends LazyModule
{
  val node = TLToAPBNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
      val out = node.bundleOut
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      val beatBytes = edgeOut.slave.beatBytes
      val lgBytes = log2Ceil(beatBytes)

      // APB has no cache coherence
      in.b.valid := Bool(false)
      in.c.ready := Bool(true)
      in.e.ready := Bool(true)

      // We need a skidpad to capture D output:
      // We cannot know if the D response will be accepted until we have
      // presented it on D as valid.  We also can't back-pressure APB in the
      // data phase.  Therefore, we must have enough space to save the data
      // phase result.  Whenever we have a queued response, we can not allow
      // APB to present new responses, so we must quash the address phase.
      val d = Wire(in.d)
      in.d <> Queue(d, 1, flow = true)

      // We need an irrevocable input for APB to stall
      val a = Queue(in.a, 1, flow = combinational, pipe = !combinational)

      val a_enable = RegInit(Bool(false))
      val a_sel    = a.valid && RegNext(!in.d.valid || in.d.ready)
      val a_write  = edgeIn.hasData(a.bits)

      when (a_sel)    { a_enable := Bool(true) }
      when (d.fire()) { a_enable := Bool(false) }

      out.psel    := a_sel
      out.penable := a_enable
      out.pwrite  := a_write
      out.paddr   := a.bits.address
      out.pprot   := PROT_DEFAULT
      out.pwdata  := a.bits.data
      out.pstrb   := Mux(a_write, a.bits.mask, UInt(0))

      a.ready := a_enable && out.pready
      d.valid := a_enable && out.pready
      assert (!d.valid || d.ready)

      d.bits := edgeIn.AccessAck(a.bits, UInt(0), out.prdata, out.pslverr)
      d.bits.opcode := Mux(a_write, TLMessages.AccessAck, TLMessages.AccessAckData)
    }
  }
}

object TLToAPB
{
  // applied to the TL source node; y.node := TLToAPB()(x.node)
  def apply(combinational: Boolean = true)(x: TLOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): APBOutwardNode = {
    val apb = LazyModule(new TLToAPB(combinational))
    apb.node := x
    apb.node
  }
}
