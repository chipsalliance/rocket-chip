// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import chisel3.util.IrrevocableIO
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink.LFSRNoiseMaker

// q is the probability to delay a request
class AXI4Delayer(q: Double)(implicit p: Parameters) extends LazyModule
{
  val node = AXI4AdapterNode()
  require (0.0 <= q && q < 1)

  lazy val module = new LazyModuleImp(this) {
    def feed[T <: Data](sink: IrrevocableIO[T], source: IrrevocableIO[T], noise: T) {
      // irrevocable requires that we not lower valid
      val hold = RegInit(Bool(false))
      when (sink.valid)  { hold := Bool(true) }
      when (sink.fire()) { hold := Bool(false) }

      val allow = hold || UInt((q * 65535.0).toInt) <= LFSRNoiseMaker(16, source.valid)
      sink.valid := source.valid && allow
      source.ready := sink.ready && allow
      sink.bits := source.bits
      when (!sink.valid) { sink.bits := noise }
    }

    def anoise[T <: AXI4BundleA](bits: T) {
      bits.id    := LFSRNoiseMaker(bits.params.idBits)
      bits.addr  := LFSRNoiseMaker(bits.params.addrBits)
      bits.len   := LFSRNoiseMaker(bits.params.lenBits)
      bits.size  := LFSRNoiseMaker(bits.params.sizeBits)
      bits.burst := LFSRNoiseMaker(bits.params.burstBits)
      bits.lock  := LFSRNoiseMaker(bits.params.lockBits)
      bits.cache := LFSRNoiseMaker(bits.params.cacheBits)
      bits.prot  := LFSRNoiseMaker(bits.params.protBits)
      bits.qos   := LFSRNoiseMaker(bits.params.qosBits)
      if (bits.params.userBits > 0)
        bits.user.get := LFSRNoiseMaker(bits.params.userBits)
    }

    (node.in zip node.out) foreach { case ((in, _), (out, _)) =>
      val arnoise = Wire(in.ar.bits)
      val awnoise = Wire(in.aw.bits)
      val wnoise  = Wire(in.w .bits)
      val rnoise  = Wire(in.r .bits)
      val bnoise  = Wire(in.b .bits)

      anoise(arnoise)
      anoise(awnoise)

      wnoise.data := LFSRNoiseMaker(wnoise.params.dataBits)
      wnoise.strb := LFSRNoiseMaker(wnoise.params.dataBits/8)
      wnoise.last := LFSRNoiseMaker(1)(0)

      rnoise.id   := LFSRNoiseMaker(rnoise.params.idBits)
      rnoise.data := LFSRNoiseMaker(rnoise.params.dataBits)
      rnoise.resp := LFSRNoiseMaker(rnoise.params.respBits)
      rnoise.last := LFSRNoiseMaker(1)(0)
      if (rnoise.params.userBits > 0)
        rnoise.user.get := LFSRNoiseMaker(rnoise.params.userBits)

      bnoise.id   := LFSRNoiseMaker(bnoise.params.idBits)
      bnoise.resp := LFSRNoiseMaker(bnoise.params.respBits)
      if (bnoise.params.userBits > 0)
        bnoise.user.get := LFSRNoiseMaker(bnoise.params.userBits)

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
