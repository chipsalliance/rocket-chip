// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class AHBToTLNode()(implicit valName: ValName) extends MixedAdapterNode(AHBImpSlave, TLImp)(
  dFn = { case mp =>
    TLMasterPortParameters.v2(
      masters = mp.masters.map { m =>
        // This value should be constrained by a data width parameter that flows from masters to slaves
        // AHB fixed length transfer size maximum is 16384 = 1024 * 16 bits, hsize is capped at 111 = 1024 bit transfer size and hburst is capped at 111 = 16 beat burst
          TLMasterParameters.v2(
            name     = m.name,
            nodePath = m.nodePath,
            emits    = TLMasterToSlaveTransferSizes(
                       get     = TransferSizes(1, 2048),
                       putFull = TransferSizes(1, 2048)
                       )
          )
      },
      requestFields = AMBAProtField() +: mp.requestFields,
      responseKeys  = mp.responseKeys)
  },
  uFn = { mp => AHBSlavePortParameters(
    slaves = mp.managers.map { m =>
      def adjust(x: TransferSizes) = {
        if (x.contains(mp.beatBytes)) {
          TransferSizes(x.min, m.minAlignment.min(mp.beatBytes * AHBParameters.maxTransfer).toInt)
        } else { // larger than beatBytes requires beatBytes if misaligned
          x.intersect(TransferSizes(1, mp.beatBytes))
        }
      }

      AHBSlaveParameters(
        address       = m.address,
        resources     = m.resources,
        regionType    = m.regionType,
        executable    = m.executable,
        nodePath      = m.nodePath,
        supportsWrite = adjust(m.supportsPutFull),
        supportsRead  = adjust(m.supportsGet))},
    beatBytes  = mp.beatBytes,
    lite       = true,
    responseFields = mp.responseFields,
    requestKeys    = mp.requestKeys.filter(_ != AMBAProt))
  })

class AHBToTL()(implicit p: Parameters) extends LazyModule
{
  val node = AHBToTLNode()

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val beatBytes = edgeOut.manager.beatBytes

      val d_send  = RegInit(false.B)
      val d_recv  = RegInit(false.B)
      val d_pause = RegInit(true.B)
      val d_fail  = RegInit(false.B)
      val d_write = RegInit(false.B)
      val d_addr  = Reg(UInt(edgeIn.bundle.addrBits.W))
      val d_size  = Reg(UInt(edgeOut.bundle.sizeBits.W))
      val d_user  = Reg(BundleMap(edgeOut.bundle.requestFields))

      when (out.d.valid) { d_recv  := false.B }
      when (out.a.ready) { d_send  := false.B }
      when (in.hresp(0)) { d_pause := false.B }

      val a_count  = RegInit(0.U(4.W))
      val a_first  = a_count === 0.U
      val d_last   = a_first

      val burst_sizes = Seq(1, 1, 4, 4, 8, 8, 16, 16)
      val a_burst_size = VecInit.tabulate(burst_sizes.length)(burst_sizes.map(beats => log2Ceil(beats * beatBytes).U))(in.hburst)
      val a_burst_mask = VecInit.tabulate(burst_sizes.length)(burst_sizes.map(beats => (beats * beatBytes - 1).U))(in.hburst)

      val a_burst_ok =
        in.htrans === AHBParameters.TRANS_NONSEQ && // only start burst on first AHB beat
        in.hsize  === (log2Ceil(beatBytes)).U  && // not a narrow burst
        (in.haddr & a_burst_mask) === 0.U    && // address aligned to burst size
        in.hburst =/= AHBParameters.BURST_INCR   && // we know the burst length a priori
        Mux(in.hwrite,                              // target device supports the burst
          edgeOut.manager.supportsPutFullSafe(in.haddr, a_burst_size),
          edgeOut.manager.supportsGetSafe    (in.haddr, a_burst_size))

      val beat = TransferSizes(1, beatBytes)
      val a_legal = // Is the single-beat access allowed?
        Mux(in.hwrite,
          edgeOut.manager.supportsPutFullSafe(in.haddr, in.hsize, Some(beat)),
          edgeOut.manager.supportsGetSafe    (in.haddr, in.hsize, Some(beat)))

      val a_access = in.htrans === AHBParameters.TRANS_NONSEQ || in.htrans === AHBParameters.TRANS_SEQ
      val a_accept = in.hready && in.hsel && a_access

      when (a_accept) {
        a_count := a_count - 1.U
        when ( in.hwrite) { d_send := true.B }
        when (!in.hwrite) { d_recv := true.B }
        when (a_first) {
          a_count := Mux(a_burst_ok, a_burst_mask >> log2Ceil(beatBytes), 0.U)
          d_send  := a_legal
          d_recv  := a_legal
          d_pause := true.B
          d_write := in.hwrite
          d_addr  := in.haddr
          d_size  := Mux(a_burst_ok, a_burst_size, in.hsize)
          d_user  :<= in.hauser
          d_user.lift(AMBAProt).foreach { x =>
            x.fetch      := !in.hprot(0)
            x.privileged :=  in.hprot(1)
            x.bufferable :=  in.hprot(2)
            x.modifiable :=  in.hprot(3)
            x.secure     := false.B
            x.readalloc  :=  in.hprot(3)
            x.writealloc :=  in.hprot(3)
          }
        }
      }

      out.a.valid        := d_send
      out.a.bits.opcode  := Mux(d_write, TLMessages.PutFullData, TLMessages.Get)
      out.a.bits.param   := 0.U
      out.a.bits.size    := d_size
      out.a.bits.source  := 0.U
      out.a.bits.address := d_addr
      out.a.bits.data    := in.hwdata
      out.a.bits.mask    := MaskGen(d_addr, d_size, beatBytes)
      out.a.bits.corrupt := false.B
      out.a.bits.user    :<= d_user

      out.d.ready  := d_recv // backpressure AccessAckData arriving faster than AHB beats

      // AHB failed reads take two cycles.
      // We must accept the D-channel beat on the first cycle, as otherwise
      // the failure might be legally retracted on the second cycle.
      // Although the AHB spec says:
      //   "A slave only has to provide valid data when a transfer completes with
      //    an OKAY response. ERROR responses do not require valid read data."
      // We choose, nevertheless, to provide the read data for the failed request.
      // Unfortunately, this comes at the cost of a bus-wide register.
      in.hrdata := out.d.bits.data holdUnless d_recv
      in.hduser :<= out.d.bits.user

      // Double-check that the above register has the intended effect since
      // this is an additional requirement not tested by third-party VIP.
      // hresp(0) && !hreadyout => next cycle has same hrdata value
      assert (!RegNext(in.hresp(0) && !in.hreadyout, false.B) || RegNext(in.hrdata) === in.hrdata)

      // In a perfect world, we'd use these signals
      val hresp = d_fail || (out.d.valid && (out.d.bits.denied || out.d.bits.corrupt))
      val hreadyout = Mux(d_write, (!d_send || out.a.ready) && (!d_last || !d_recv || out.d.valid), out.d.valid || !d_recv)

      // Make the failure persistent (and defer it to the last beat--otherwise AHB can cancel the burst!)
      d_fail :=
        (hresp && !(a_first && in.hready)) || // clear failure when a new beat starts
        (a_accept && !a_legal)                // failure if the address requested is illegal

      // When we report a failure, we need to be hreadyout LOW for one cycle
      in.hresp     := hreadyout &&  (hresp && d_last)
      in.hreadyout := hreadyout && !(hresp && d_last && d_pause)

      // Unused channels
      out.b.ready := true.B
      out.c.valid := false.B
      out.e.valid := false.B
    }
  }
}

object AHBToTL
{
  def apply()(implicit p: Parameters) =
  {
    val ahb2tl = LazyModule(new AHBToTL)
    ahb2tl.node
  }
}
