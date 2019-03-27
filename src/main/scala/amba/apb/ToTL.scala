// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.MaskGen

case class APBToTLNode()(implicit valName: ValName) extends MixedAdapterNode(APBImp, TLImp)(
  dFn = { case APBMasterPortParameters(masters) =>
    TLClientPortParameters(clients = masters.map { m =>
      TLClientParameters(name = m.name, nodePath = m.nodePath)
    })
  },
  uFn = { mp => APBSlavePortParameters(
    slaves = mp.managers.map { m =>
      APBSlaveParameters(
        address       = m.address,
        resources     = m.resources,
        regionType    = m.regionType,
        executable    = m.executable,
        nodePath      = m.nodePath,
        //TODO why and how should I do this adjust for APB?
        // Also, shouldn't I use supportsPutPartial because of PSTRB?
        supportsWrite = m.supportsPutFull,//adjust(m.supportsPutFull),
        supportsRead  = m.supportsGet)},//adjust(m.supportsGet))},
    beatBytes = mp.beatBytes)
  }) {
  //TODO: Shouldn't there be a requirement that TL side is wider than APB side?
}

class APBToTL()(implicit p: Parameters) extends LazyModule
{
  val node = APBToTLNode()

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val beatBytes = edgeOut.manager.beatBytes

      //-----------------------------------------------
      // Flow Control
      out.a.valid := in.psel && in.penable
      in.pready := in.psel & in.penable & out.d.valid
      out.d.ready := in.psel && in.penable // No backpressure to APB Master

      // Write
      // Question -- do I want PutPartial here or PutFull?
      // I was thinking PutPartial because of the pstrb masking.
      out.a.bits.opcode  := Mux(in.pwrite, TLMessages.PutPartialData, TLMessages.Get)
      out.a.bits.param   := UInt(0)
      out.a.bits.size    := UInt(log2Ceil(in.params.dataBits/8))
      out.a.bits.source  := UInt(0)
      out.a.bits.address := in.paddr
      out.a.bits.data    := in.pwdata
      val pstrbFill = beatBytes / (in.params.dataBits/8)
      out.a.bits.mask    := MaskGen(out.a.bits.address, out.a.bits.size, beatBytes) | Cat(Seq.fill(pstrbFill)(in.pstrb).reverse)
      out.a.bits.corrupt := Bool(false)
      assert (in.pprot === 0.U, "Can't support APB PPROT != 0 in APB2TL conversion")
      // Read
      // TODO: do I need to slice this? What if they aren't same width?
      in.prdata := out.d.bits.data

      // Error
      in.pslverr := out.d.bits.corrupt || out.d.bits.denied

      // Unused channels
      out.b.ready := Bool(true)
      out.c.valid := Bool(false)
      out.e.valid := Bool(false)
    }
  }
}

object APBToTL
{
  def apply()(implicit p: Parameters) =
  {
    val apb2tl = LazyModule(new APBToTL)
    apb2tl.node
  }
}
