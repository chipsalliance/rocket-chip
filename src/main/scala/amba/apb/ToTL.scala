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
        supportsWrite = m.supportsPutPartial.intersect(TransferSizes(1, mp.beatBytes)),
        supportsRead  = m.supportsGet.intersect(TransferSizes(1, mp.beatBytes)))},
    beatBytes = mp.beatBytes)
  })

class APBToTL()(implicit p: Parameters) extends LazyModule
{
  val node = APBToTLNode()

  
  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val beatBytes = edgeOut.manager.beatBytes

      //-----------------------------------------------
      // Flow Control
      // We don't want to handle the case where the data is returned immediately
      // on the same cycle that we send the transaction. Require and verify.
      require (edgeOut.manager.minLatency >= 1)
      assert (!(in.psel && !in.penable && out.d.valid))

      val in_flight_reg = RegInit(false.B)
      out.a.valid := in.psel && !in_flight_reg
      in.pready := out.d.valid
      out.d.ready := true.B

      when (out.a.fire()){in_flight_reg := true.B}
      when (out.d.fire()){in_flight_reg := false.B}

      // Write
      // PutPartial because of pstrb masking.
      out.a.bits.opcode  := Mux(in.pwrite, TLMessages.PutPartialData, TLMessages.Get)
      out.a.bits.param   := UInt(0)
      out.a.bits.size    := UInt(log2Ceil(in.params.dataBits/8))
      out.a.bits.source  := UInt(0)
      // TL requires addresses be aligned to their size.
      out.a.bits.address := ~(~in.paddr | (beatBytes-1).U)
      // The double negative above is to work around Chisel's broken implementation of widening ~x.
      assert(in.paddr === out.a.bits.address, "Do not expect to have to perform alignment in APB2TL Conversion")
      out.a.bits.data    := in.pwdata
      out.a.bits.mask    := Mux(in.pwrite, in.pstrb, ~0.U(beatBytes.W))
      out.a.bits.corrupt := Bool(false)
      // Note: we ignore in.pprot
      // Read
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
