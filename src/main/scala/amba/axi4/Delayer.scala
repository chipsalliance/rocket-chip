// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink.LFSRNoiseMaker

/**
  * Delay AXI4 requests randomly
  *
  * @param q the probability to delay a request
  */
class AXI4Delayer(q: Double)(implicit p: Parameters) extends LazyModule
{
  val node = AXI4AdapterNode()
  require (0.0 <= q && q < 1)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    def feed[T <: Data](sink: IrrevocableIO[T], source: IrrevocableIO[T], noise: T): Unit = {
      // irrevocable requires that we not lower valid
      val hold = RegInit(false.B)
      when (sink.valid)  { hold := true.B }
      when (sink.fire) { hold := false.B }

      val allow = hold || ((q * 65535.0).toInt).U <= LFSRNoiseMaker(16, source.valid)
      sink.valid := source.valid && allow
      source.ready := sink.ready && allow
      sink.bits := source.bits
      when (!sink.valid) { sink.bits := noise }
    }

    def anoise[T <: AXI4BundleA](bits: T): Unit = {
      bits.id    := LFSRNoiseMaker(bits.params.idBits)
      bits.addr  := LFSRNoiseMaker(bits.params.addrBits)
      bits.len   := LFSRNoiseMaker(bits.params.lenBits)
      bits.size  := LFSRNoiseMaker(bits.params.sizeBits)
      bits.burst := LFSRNoiseMaker(bits.params.burstBits)
      bits.lock  := LFSRNoiseMaker(bits.params.lockBits)
      bits.cache := LFSRNoiseMaker(bits.params.cacheBits)
      bits.prot  := LFSRNoiseMaker(bits.params.protBits)
      bits.qos   := LFSRNoiseMaker(bits.params.qosBits)
    }

    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val arnoise = Wire(new AXI4BundleAR(edgeIn.bundle))
      val awnoise = Wire(new AXI4BundleAW(edgeIn.bundle))
      val wnoise  = Wire(new  AXI4BundleW(edgeIn.bundle))
      val rnoise  = Wire(new  AXI4BundleR(edgeIn.bundle))
      val bnoise  = Wire(new  AXI4BundleB(edgeIn.bundle))

      arnoise := DontCare
      awnoise := DontCare
      wnoise := DontCare
      rnoise := DontCare
      bnoise := DontCare

      anoise(arnoise)
      anoise(awnoise)

      wnoise.data := LFSRNoiseMaker(wnoise.params.dataBits)
      wnoise.strb := LFSRNoiseMaker(wnoise.params.dataBits/8)
      wnoise.last := LFSRNoiseMaker(1)(0)

      rnoise.id   := LFSRNoiseMaker(rnoise.params.idBits)
      rnoise.data := LFSRNoiseMaker(rnoise.params.dataBits)
      rnoise.resp := LFSRNoiseMaker(rnoise.params.respBits)
      rnoise.last := LFSRNoiseMaker(1)(0)

      bnoise.id   := LFSRNoiseMaker(bnoise.params.idBits)
      bnoise.resp := LFSRNoiseMaker(bnoise.params.respBits)

      feed(out.ar, in.ar, arnoise)
      feed(out.aw, in.aw, awnoise)
      feed(out.w,  in.w,   wnoise)
      feed(in.b,   out.b,  bnoise)
      feed(in.r,   out.r,  rnoise)
    }
  }
}

object AXI4Delayer
{
  def apply(q: Double)(implicit p: Parameters): AXI4Node =
  {
    val axi4delay = LazyModule(new AXI4Delayer(q))
    axi4delay.node
  }
}
