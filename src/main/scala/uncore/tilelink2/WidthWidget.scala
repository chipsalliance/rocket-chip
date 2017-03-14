// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._
import scala.math.{min,max}

// innBeatBytes => the new client-facing bus width
class TLWidthWidget(innerBeatBytes: Int)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode(
    clientFn  = { case c => c },
    managerFn = { case m => m.copy(beatBytes = innerBeatBytes) })

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    def merge[T <: TLDataChannel](edgeIn: TLEdge, in: DecoupledIO[T], edgeOut: TLEdge, out: DecoupledIO[T]) = {
      val inBytes = edgeIn.manager.beatBytes
      val outBytes = edgeOut.manager.beatBytes
      val ratio = outBytes / inBytes

      val rdata = Reg(UInt(width = (ratio-1)*inBytes*8))
      val rmask = Reg(UInt(width = (ratio-1)*inBytes))
      val data = Cat(edgeIn.data(in.bits), rdata)
      val mask = Cat(edgeIn.mask(in.bits), rmask)
      val address = edgeIn.address(in.bits)
      val size = edgeIn.size(in.bits)
      val hasData = edgeIn.hasData(in.bits)

      val count = RegInit(UInt(0, width = log2Ceil(ratio)))
      val first = count === UInt(0)
      val limit = UIntToOH1(size, log2Ceil(outBytes)) >> log2Ceil(inBytes)
      val last = count === limit || !hasData

      when (in.fire()) {
        rdata := data >> inBytes*8
        rmask := mask >> inBytes
        count := count + UInt(1)
        when (last) { count := UInt(0) }
      }

      val cases = Seq.tabulate(log2Ceil(ratio)+1) { i =>
        val high = outBytes
        val take = (1 << i)*inBytes
        (Fill(1 << (log2Ceil(ratio)-i), data(high*8-1, (high-take)*8)),
         Fill(1 << (log2Ceil(ratio)-i), mask(high  -1, (high-take))))
      }
      val dataMux = Vec.tabulate(log2Ceil(edgeIn.maxTransfer)+1) { lgSize =>
        cases(min(max(lgSize - log2Ceil(inBytes), 0), log2Ceil(ratio)))._1
      }
      val maskMux = Vec.tabulate(log2Ceil(edgeIn.maxTransfer)+1) { lgSize =>
        cases(min(max(lgSize - log2Ceil(inBytes), 0), log2Ceil(ratio)))._2
      }

      val dataOut = if (edgeIn.staticHasData(in.bits) == Some(false)) UInt(0) else dataMux(size)
      lazy val maskFull = edgeOut.mask(address, size)
      lazy val maskOut = Mux(hasData, maskMux(size) & maskFull, maskFull)

      in.ready := out.ready || !last
      out.valid := in.valid && last
      out.bits := in.bits
      edgeOut.data(out.bits) := dataOut

      out.bits match {
        case a: TLBundleA => a.mask := maskOut
        case b: TLBundleB => b.mask := maskOut
        case c: TLBundleC => ()
        case d: TLBundleD => ()
          // addr_lo gets padded with 0s on D channel, the only lossy transform in this core
          // this should be safe, because we only care about addr_lo on D to determine which
          // piece of data to extract when the D data bus is narrowed. Since we duplicated the
          // data to all locations, addr_lo still points at a valid copy.
      }
    }

    def split[T <: TLDataChannel](edgeIn: TLEdge, in: DecoupledIO[T], edgeOut: TLEdge, out: DecoupledIO[T]) = {
      val inBytes = edgeIn.manager.beatBytes
      val outBytes = edgeOut.manager.beatBytes
      val ratio = inBytes / outBytes

      val hasData = edgeIn.hasData(in.bits)
      val size = edgeIn.size(in.bits)
      val data = edgeIn.data(in.bits)
      val mask = edgeIn.mask(in.bits)

      val dataSlices = Vec.tabulate(ratio) { i => data((i+1)*outBytes*8-1, i*outBytes*8) }
      val maskSlices = Vec.tabulate(ratio) { i => mask((i+1)*outBytes  -1, i*outBytes)   }
      val filter = Reg(UInt(width = ratio), init = SInt(-1, width = ratio).asUInt)
      val maskR = maskSlices.map(_.orR)

      // decoded_size = 1111 (for smallest), 0101, 0001 (for largest)
      val sizeOH1 = UIntToOH1(size, log2Ceil(inBytes)) >> log2Ceil(outBytes)
      val decoded_size = Seq.tabulate(ratio) { i => trailingZeros(i).map(!sizeOH1(_)).getOrElse(Bool(true)) }

      val first = filter(ratio-1)
      val new_filter = Mux(first, Cat(decoded_size.reverse), filter << 1)
      val last = new_filter(ratio-1) || !hasData
      when (out.fire()) {
        filter := new_filter 
        when (!hasData) { filter := SInt(-1, width = ratio).asUInt }
      }

      val select = Cat(maskR.reverse) & new_filter
      val dataOut = if (edgeIn.staticHasData(in.bits) == Some(false)) UInt(0) else Mux1H(select, dataSlices)
      val maskOut = Mux1H(select, maskSlices)

      out <> in
      edgeOut.data(out.bits) := dataOut

      out.bits match {
        case a: TLBundleA => a.mask := maskOut
        case b: TLBundleB => b.mask := maskOut
        case c: TLBundleC => ()
        case d: TLBundleD => () // addr_lo gets truncated automagically
      }

      // Repeat the input if we're not last
      !last
    }
    
    def splice[T <: TLDataChannel](edgeIn: TLEdge, in: DecoupledIO[T], edgeOut: TLEdge, out: DecoupledIO[T]) = {
      if (edgeIn.manager.beatBytes == edgeOut.manager.beatBytes) {
        // nothing to do; pass it through
        out <> in
      } else if (edgeIn.manager.beatBytes > edgeOut.manager.beatBytes) {
        // split input to output
        val repeat = Wire(Bool())
        repeat := split(edgeIn, Repeater(in, repeat), edgeOut, out)
      } else {
        // merge input to output
        merge(edgeIn, in, edgeOut, out)
      }
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      splice(edgeIn,  in.a,  edgeOut, out.a)
      splice(edgeOut, out.d, edgeIn,  in.d)

      if (edgeOut.manager.anySupportAcquireB && edgeIn.client.anySupportProbe) {
        splice(edgeOut, out.b, edgeIn,  in.b)
        splice(edgeIn,  in.c,  edgeOut, out.c)
        in.e.ready := out.e.ready
        out.e.valid := in.e.valid
        out.e.bits := in.e.bits
      } else {
        in.b.valid := Bool(false)
        in.c.ready := Bool(true)
        in.e.ready := Bool(true)
        out.b.ready := Bool(true)
        out.c.valid := Bool(false)
        out.e.valid := Bool(false)
      }
    }
  }
}

object TLWidthWidget
{
  // applied to the TL source node; y.node := WidthWidget(x.node, 16)
  def apply(innerBeatBytes: Int)(x: TLOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): TLOutwardNode = {
    val widget = LazyModule(new TLWidthWidget(innerBeatBytes))
    widget.node := x
    widget.node
  }
}

/** Synthesizeable unit tests */
import unittest._

class TLRAMWidthWidget(first: Int, second: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(5000))
  val model = LazyModule(new TLRAMModel)
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x3ff)))

  model.node := fuzz.node
  ram.node := TLDelayer(0.1)(TLFragmenter(4, 256)(
                if (first == second ) { TLWidthWidget(first)(TLDelayer(0.1)(model.node)) }
                else {
                  TLWidthWidget(second)(
                    TLWidthWidget(first)(TLDelayer(0.1)(model.node)))}))

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMWidthWidgetTest(little: Int, big: Int)(implicit p: Parameters) extends UnitTest(timeout = 500000) {
  io.finished := Module(LazyModule(new TLRAMWidthWidget(little,big)).module).io.finished
}
