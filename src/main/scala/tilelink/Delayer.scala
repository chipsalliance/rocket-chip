// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

// q is the probability to delay a request
class TLDelayer(q: Double)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode()
  require (0.0 <= q && q < 1)

  lazy val module = new LazyModuleImp(this) {
    def feed[T <: Data](sink: DecoupledIO[T], source: DecoupledIO[T], noise: T) {
      val allow = UInt((q * 65535.0).toInt) <= LFSRNoiseMaker(16, source.valid)
      sink.valid := source.valid && allow
      source.ready := sink.ready && allow
      sink.bits := source.bits
      when (!sink.valid) { sink.bits := noise }
    }

    (node.in zip node.out) foreach { case ((in, _), (out, _)) =>
      val anoise = Wire(in.a.bits)
      anoise.opcode  := LFSRNoiseMaker(3)
      anoise.param   := LFSRNoiseMaker(3)
      anoise.size    := LFSRNoiseMaker(anoise.params.sizeBits)
      anoise.source  := LFSRNoiseMaker(anoise.params.sourceBits)
      anoise.address := LFSRNoiseMaker(anoise.params.addressBits)
      anoise.mask    := LFSRNoiseMaker(anoise.params.dataBits/8)
      anoise.data    := LFSRNoiseMaker(anoise.params.dataBits)

      val bnoise = Wire(out.b.bits)
      bnoise.opcode  := LFSRNoiseMaker(3)
      bnoise.param   := LFSRNoiseMaker(3)
      bnoise.size    := LFSRNoiseMaker(bnoise.params.sizeBits)
      bnoise.source  := LFSRNoiseMaker(bnoise.params.sourceBits)
      bnoise.address := LFSRNoiseMaker(bnoise.params.addressBits)
      bnoise.mask    := LFSRNoiseMaker(bnoise.params.dataBits/8)
      bnoise.data    := LFSRNoiseMaker(bnoise.params.dataBits)

      val cnoise = Wire(in.c.bits)
      cnoise.opcode  := LFSRNoiseMaker(3)
      cnoise.param   := LFSRNoiseMaker(3)
      cnoise.size    := LFSRNoiseMaker(cnoise.params.sizeBits)
      cnoise.source  := LFSRNoiseMaker(cnoise.params.sourceBits)
      cnoise.address := LFSRNoiseMaker(cnoise.params.addressBits)
      cnoise.data    := LFSRNoiseMaker(cnoise.params.dataBits)
      cnoise.corrupt := LFSRNoiseMaker(1)(0)

      val dnoise = Wire(out.d.bits)
      dnoise.opcode  := LFSRNoiseMaker(3)
      dnoise.param   := LFSRNoiseMaker(3)
      dnoise.size    := LFSRNoiseMaker(dnoise.params.sizeBits)
      dnoise.source  := LFSRNoiseMaker(dnoise.params.sourceBits)
      dnoise.sink    := LFSRNoiseMaker(dnoise.params.sinkBits)
      dnoise.denied  := LFSRNoiseMaker(1)(0)
      dnoise.data    := LFSRNoiseMaker(dnoise.params.dataBits)
      dnoise.corrupt := LFSRNoiseMaker(1)(0)

      val enoise = Wire(in.e.bits)
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
