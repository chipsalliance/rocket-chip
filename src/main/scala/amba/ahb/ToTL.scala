// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.MaskGen

case class AHBToTLNode()(implicit valName: ValName) extends MixedAdapterNode(AHBImpSlave, TLImp)(
  dFn = { case AHBMasterPortParameters(masters) =>
    TLClientPortParameters(clients = masters.map { m =>
      TLClientParameters(name = m.name, nodePath = m.nodePath)
    })
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
    beatBytes = mp.beatBytes)
  })

class AHBToTL()(implicit p: Parameters) extends LazyModule
{
  val node = AHBToTLNode()

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val beatBytes = edgeOut.manager.beatBytes

      val d_send  = RegInit(Bool(false))
      val d_recv  = RegInit(Bool(false))
      val d_pause = RegInit(Bool(true))
      val d_fail  = RegInit(Bool(false))
      val d_write = RegInit(Bool(false))
      val d_addr  = Reg(in.haddr)
      val d_size  = Reg(in.hsize)

      when (out.d.valid) { d_recv  := Bool(false) }
      when (out.a.ready) { d_send  := Bool(false) }
      when (in.hresp)    { d_pause := Bool(false) }

      val a_count  = RegInit(UInt(0, width = 4))
      val a_first  = a_count === UInt(0)
      val d_last   = a_first

      val burst_sizes = Seq(1, 1, 4, 4, 8, 8, 16, 16)
      val a_burst_size = Vec(burst_sizes.map(beats => UInt(log2Ceil(beats * beatBytes))))(in.hburst)
      val a_burst_mask = Vec(burst_sizes.map(beats => UInt(beats * beatBytes - 1)))(in.hburst)

      val a_burst_ok =
        in.htrans === AHBParameters.TRANS_NONSEQ && // only start burst on first AHB beat
        in.hsize  === UInt(log2Ceil(beatBytes))  && // not a narrow burst
        (in.haddr & a_burst_mask) === UInt(0)    && // address aligned to burst size
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
        a_count := a_count - UInt(1)
        when ( in.hwrite) { d_send := Bool(true) }
        when (!in.hwrite) { d_recv := Bool(true) }
        when (a_first) {
          a_count := Mux(a_burst_ok, a_burst_mask >> log2Ceil(beatBytes), UInt(0))
          d_send  := a_legal
          d_recv  := a_legal
          d_pause := Bool(true)
          d_write := in.hwrite
          d_addr  := in.haddr
          d_size  := Mux(a_burst_ok, a_burst_size, in.hsize)
        }
      }

      out.a.valid        := d_send
      out.a.bits.opcode  := Mux(d_write, TLMessages.PutFullData, TLMessages.Get)
      out.a.bits.param   := UInt(0)
      out.a.bits.size    := d_size
      out.a.bits.source  := UInt(0)
      out.a.bits.address := d_addr
      out.a.bits.data    := in.hwdata
      out.a.bits.mask    := MaskGen(d_addr, d_size, beatBytes)
      out.a.bits.corrupt := Bool(false)

      out.d.ready  := d_recv // backpressure AccessAckData arriving faster than AHB beats

      // NOTE: on failure, we present the read result on the hreadyout LOW cycle
      // This means that if you latch hrdata from a failure, the result is garbage.
      // To fix this would require a bus-wide register, and the AHB spec says this:
      // "A slave only has to provide valid data when a transfer completes with an OKAY
      //  response. ERROR responses do not require valid read data."
      // Therefore, we choose to accept this slight TL-AHB infidelity.
      in.hrdata := out.d.bits.data

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
      out.b.ready := Bool(true)
      out.c.valid := Bool(false)
      out.e.valid := Bool(false)
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
