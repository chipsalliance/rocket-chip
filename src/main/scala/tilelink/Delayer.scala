// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

// q is the probability to delay a request
class TLDelayer(q: Double)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode()
  require (0.0 <= q && q < 1)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    def feed[T <: Data](sink: DecoupledIO[T], source: DecoupledIO[T], noise: T): Unit = {
      val allow = ((q * 65535.0).toInt).U <= LFSRNoiseMaker(16, source.valid)
      sink.valid := source.valid && allow
      source.ready := sink.ready && allow
      sink.bits := source.bits
      when (!sink.valid) { sink.bits := noise }
    }

    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val anoise = Wire(new TLBundleA(edgeIn.bundle))
      anoise.opcode  := LFSRNoiseMaker(3)
      anoise.param   := LFSRNoiseMaker(3)
      anoise.size    := LFSRNoiseMaker(anoise.params.sizeBits)
      anoise.source  := LFSRNoiseMaker(anoise.params.sourceBits)
      anoise.address := LFSRNoiseMaker(anoise.params.addressBits)
      anoise.user    := DontCare
      anoise.echo    := DontCare
      anoise.mask    := LFSRNoiseMaker(anoise.params.dataBits/8)
      anoise.data    := LFSRNoiseMaker(anoise.params.dataBits)
      anoise.corrupt := LFSRNoiseMaker(1)

      val bnoise = Wire(new TLBundleB(edgeOut.bundle))
      bnoise.opcode  := LFSRNoiseMaker(3)
      bnoise.param   := LFSRNoiseMaker(3)
      bnoise.size    := LFSRNoiseMaker(bnoise.params.sizeBits)
      bnoise.source  := LFSRNoiseMaker(bnoise.params.sourceBits)
      bnoise.address := LFSRNoiseMaker(bnoise.params.addressBits)
      bnoise.mask    := LFSRNoiseMaker(bnoise.params.dataBits/8)
      bnoise.data    := LFSRNoiseMaker(bnoise.params.dataBits)
      bnoise.corrupt := LFSRNoiseMaker(1)

      val cnoise = Wire(new TLBundleC(edgeIn.bundle))
      cnoise.opcode  := LFSRNoiseMaker(3)
      cnoise.param   := LFSRNoiseMaker(3)
      cnoise.size    := LFSRNoiseMaker(cnoise.params.sizeBits)
      cnoise.source  := LFSRNoiseMaker(cnoise.params.sourceBits)
      cnoise.address := LFSRNoiseMaker(cnoise.params.addressBits)
      cnoise.user    := DontCare
      cnoise.echo    := DontCare
      cnoise.data    := LFSRNoiseMaker(cnoise.params.dataBits)
      cnoise.corrupt := LFSRNoiseMaker(1)(0)

      val dnoise = Wire(new TLBundleD(edgeOut.bundle))
      dnoise.opcode  := LFSRNoiseMaker(3)
      dnoise.param   := LFSRNoiseMaker(3)
      dnoise.size    := LFSRNoiseMaker(dnoise.params.sizeBits)
      dnoise.source  := LFSRNoiseMaker(dnoise.params.sourceBits)
      dnoise.sink    := LFSRNoiseMaker(dnoise.params.sinkBits)
      dnoise.denied  := LFSRNoiseMaker(1)(0)
      dnoise.user    := DontCare
      dnoise.echo    := DontCare
      dnoise.data    := LFSRNoiseMaker(dnoise.params.dataBits)
      dnoise.corrupt := LFSRNoiseMaker(1)(0)

      val enoise = Wire(new TLBundleE(edgeIn.bundle))
      enoise.sink := LFSRNoiseMaker(enoise.params.sinkBits)

      feed(out.a, in.a, anoise)
      feed(out.c, in.c, cnoise)
      feed(out.e, in.e, enoise)
      feed(in.b, out.b, bnoise)
      feed(in.d, out.d, dnoise)
    }
  }
}

object TLDelayer
{
  def apply(q: Double)(implicit p: Parameters): TLNode =
  {
    val delayer = LazyModule(new TLDelayer(q))
    delayer.node
  }
}
