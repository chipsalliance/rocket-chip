// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.amba._
import APBParameters._
import chisel3.util._

case class TLToAPBNode()(implicit valName: ValName) extends MixedAdapterNode(TLImp, APBImp)(
  dFn = { cp =>
    APBMasterPortParameters(
      masters = cp.clients.map { case c => APBMasterParameters(name = c.name, nodePath = c.nodePath) },
      requestFields = cp.requestFields.filter(!_.isInstanceOf[AMBAProtField]),
      responseKeys  = cp.responseKeys)
  },
  uFn = { sp =>
    val managers = sp.slaves.map { case s =>
      TLSlaveParameters.v1(
        address            = s.address,
        resources          = s.resources,
        regionType         = s.regionType,
        executable         = s.executable,
        nodePath           = s.nodePath,
        supportsGet        = if (s.supportsRead)  TransferSizes(1, sp.beatBytes) else TransferSizes.none,
        supportsPutPartial = if (s.supportsWrite) TransferSizes(1, sp.beatBytes) else TransferSizes.none,
        supportsPutFull    = if (s.supportsWrite) TransferSizes(1, sp.beatBytes) else TransferSizes.none,
        fifoId             = Some(0), // a common FIFO domain
        mayDenyPut         = true)
    }
    TLSlavePortParameters.v1(
      managers   = managers,
      beatBytes  = sp.beatBytes,
      endSinkId  = 0,
      minLatency = 1,
      responseFields = sp.responseFields,
      requestKeys    = AMBAProt +: sp.requestKeys)
  })

// The input side has either a flow queue (aFlow=true) or a pipe queue (aFlow=false)
// The output side always has a flow queue
class TLToAPB(val aFlow: Boolean = true)(implicit p: Parameters) extends LazyModule
{
  val node = TLToAPBNode()

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val beatBytes = edgeOut.slave.beatBytes
      val lgBytes = log2Ceil(beatBytes)

      // APB has no cache coherence
      in.b.valid := false.B
      in.c.ready := true.B
      in.e.ready := true.B

      // We need a skidpad to capture D output:
      // We cannot know if the D response will be accepted until we have
      // presented it on D as valid.  We also can't back-pressure APB in the
      // data phase.  Therefore, we must have enough space to save the data
      // phase result.  Whenever we have a queued response, we can not allow
      // APB to present new responses, so we must quash the address phase.
      val d = Wire(Decoupled(new TLBundleD(edgeIn.bundle)))
      in.d :<> Queue(d, 1, flow = true)

      // We need an irrevocable input for APB to stall
      val a = Queue(in.a, 1, flow = aFlow, pipe = !aFlow)

      val a_enable = RegInit(false.B)
      val a_sel    = a.valid && RegNext(!in.d.valid || in.d.ready)
      val a_write  = edgeIn.hasData(a.bits)

      val enable_d = a_sel && !a_enable
      val d_write  = RegEnable(a_write,       enable_d)
      val d_source = RegEnable(a.bits.source, enable_d)
      val d_size   = RegEnable(a.bits.size,   enable_d)
      val d_echo   = RegEnable(a.bits.echo,   enable_d)

      when (a_sel)    { a_enable := true.B }
      when (d.fire()) { a_enable := false.B }

      out.psel    := a_sel
      out.penable := a_enable
      out.pwrite  := a_write
      out.paddr   := a.bits.address
      out.pprot   := PROT_DEFAULT
      out.pwdata  := a.bits.data
      out.pstrb   := Mux(a_write, a.bits.mask, 0.U)
      out.pauser :<= a.bits.user
      a.bits.user.lift(AMBAProt).foreach { x =>
        val pprot = Wire(Vec(3, Bool()))
        pprot(0) :=  x.privileged
        pprot(1) := !x.secure
        pprot(2) :=  x.fetch
        out.pprot := Cat(pprot.reverse)
      }

      a.ready := a_enable && out.pready
      d.valid := a_enable && out.pready
      assert (!d.valid || d.ready)

      d.bits.opcode  := Mux(d_write, TLMessages.AccessAck, TLMessages.AccessAckData)
      d.bits.param   := 0.U
      d.bits.size    := d_size
      d.bits.source  := d_source
      d.bits.sink    := 0.U
      d.bits.denied  :=  d_write && out.pslverr
      d.bits.data    := out.prdata
      d.bits.corrupt := !d_write && out.pslverr
      d.bits.user    :<= out.pduser
      d.bits.echo    :<= d_echo
    }
  }
}

object TLToAPB
{
  def apply(aFlow: Boolean = true)(implicit p: Parameters) =
  {
    val tl2apb = LazyModule(new TLToAPB(aFlow))
    tl2apb.node
  }
}
