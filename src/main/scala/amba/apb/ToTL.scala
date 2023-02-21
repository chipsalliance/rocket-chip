// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class APBToTLNode()(implicit valName: ValName) extends MixedAdapterNode(APBImp, TLImp)(
  dFn = { mp =>
    TLMasterPortParameters.v1(
      clients = mp.masters.map { m =>
        TLMasterParameters.v1(name = m.name, nodePath = m.nodePath)
      },
      requestFields = AMBAProtField() +: mp.requestFields,
      responseKeys  = mp.responseKeys)
  },
  uFn = { mp => APBSlavePortParameters(
    slaves = mp.managers.map { m =>
      APBSlaveParameters(
        address       = m.address,
        resources     = m.resources,
        regionType    = m.regionType,
        executable    = m.executable,
        nodePath      = m.nodePath,
        supportsWrite = m.supportsPutPartial.intersect(TransferSizes(1, mp.beatBytes)),
        supportsRead  = m.supportsGet.intersect(TransferSizes(1, mp.beatBytes)))},
    beatBytes = mp.beatBytes,
    responseFields = mp.responseFields,
    requestKeys    = mp.requestKeys.filter(_ != AMBAProt))
  })

class APBToTL()(implicit p: Parameters) extends LazyModule
{
  val node = APBToTLNode()

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val beatBytes = edgeOut.manager.beatBytes

      //-----------------------------------------------
      // Flow Control
      // We don't want to handle the case where the data is returned immediately
      // on the same cycle that we send the transaction. Require and verify.
      require (edgeOut.manager.minLatency >= 1)
      assert (!(in.psel && !in.penable && out.d.valid))


      val beat = TransferSizes(beatBytes, beatBytes)
      //TODO: The double negative here is to work around Chisel's broken implementation of widening ~x.
      val aligned_addr =  ~in.paddr
      require(beatBytes == in.params.dataBits/8,
              s"TL beatBytes(${beatBytes}) doesn't match expected APB data width(${in.params.dataBits})")
      val data_size = (log2Ceil(beatBytes)).U
      
      // Is this access allowed? Illegal addresses are a violation of tile link protocol.
      // If an illegal address is provided, return an error instead of sending over tile link.
      val a_legal =
        Mux(in.pwrite,
          edgeOut.manager.supportsPutPartialSafe(aligned_addr, data_size, Some(beat)),
          edgeOut.manager.supportsGetSafe       (aligned_addr, data_size, Some(beat)))

      val in_flight_reg = RegInit(false.B)
      val error_in_flight_reg = RegInit(false.B)
      val in_flight = in_flight_reg || error_in_flight_reg

      out.a.valid := in.psel && !in_flight && a_legal
      in.pready := out.d.valid  || error_in_flight_reg
      out.d.ready := true.B

      when (out.a.fire){in_flight_reg := true.B}
      when (out.d.fire){in_flight_reg := false.B}

      // Default to false except when actually returning error.
      error_in_flight_reg := false.B
      when (in.psel && !in_flight) {error_in_flight_reg := !a_legal}
      
      // Write
      // PutPartial because of pstrb masking.
      out.a.bits.opcode  := Mux(in.pwrite, TLMessages.PutPartialData, TLMessages.Get)
      out.a.bits.param   := 0.U
      out.a.bits.size    := data_size
      out.a.bits.source  := 0.U
      // TL requires addresses be aligned to their size.
      out.a.bits.address := aligned_addr
      out.a.bits.user :<= in.pauser
      out.a.bits.user.lift(AMBAProt).foreach { prot =>
        prot.privileged :=  in.pprot(0)
        prot.secure     := !in.pprot(1)
        prot.fetch      :=  in.pprot(2)
        prot.bufferable :=  true.B
        prot.modifiable :=  true.B
        prot.readalloc  :=  true.B
        prot.writealloc :=  true.B
      }
      when (out.a.fire) {
        assert(in.paddr === out.a.bits.address, "Do not expect to have to perform alignment in APB2TL Conversion")
      }
      out.a.bits.data    := in.pwdata
      out.a.bits.mask    := Mux(in.pwrite, in.pstrb, ~0.U(beatBytes.W))
      out.a.bits.corrupt := false.B
      // Note: we ignore in.pprot
      // Read
      in.prdata := out.d.bits.data
      in.pduser :<= out.d.bits.user

      // Error
      in.pslverr := out.d.bits.corrupt || out.d.bits.denied || error_in_flight_reg

      // Unused channels
      out.b.ready := true.B
      out.c.valid := false.B
      out.e.valid := false.B
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
