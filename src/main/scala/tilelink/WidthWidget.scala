// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
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
      val keepBits  = log2Ceil(outBytes)
      val dropBits  = log2Ceil(inBytes)
      val countBits = log2Ceil(ratio)

      val size    = edgeIn.size(in.bits)
      val hasData = edgeIn.hasData(in.bits)
      val limit   = UIntToOH1(size, keepBits) >> dropBits

      val count  = RegInit(UInt(0, width = countBits))
      val first  = count === UInt(0)
      val last   = count === limit || !hasData
      val enable = Seq.tabulate(ratio) { i => !((count ^ UInt(i)) & limit).orR }

      when (in.fire()) {
        count := count + UInt(1)
        when (last) { count := UInt(0) }
      }

      def helper(idata: UInt): UInt = {
        val odata = Seq.fill(ratio) { idata }
        val rdata = Reg(Vec(ratio-1, idata))
        val pdata = rdata :+ idata
        val mdata = (enable zip (odata zip pdata)) map { case (e, (o, p)) => Mux(e, o, p) }
        when (in.fire() && !last) {
          (rdata zip mdata) foreach { case (r, m) => r := m }
        }
        Cat(mdata.reverse)
      }

      def reduce(i: Bool): Bool = {
        val state = Reg(Bool())
        val next = i || (!first && state)
        when (in.fire()) { state := next }
        next
      }

      in.ready := out.ready || !last
      out.valid := in.valid && last
      out.bits := in.bits

      // Don't put down hardware if we never carry data
      edgeOut.data(out.bits) := (if (edgeIn.staticHasData(in.bits) == Some(false)) UInt(0) else helper(edgeIn.data(in.bits)))

      (out.bits, in.bits) match {
        case (o: TLBundleA, i: TLBundleA) => o.mask := edgeOut.mask(o.address, o.size) & Mux(hasData, helper(i.mask), ~UInt(0, width=outBytes))
        case (o: TLBundleB, i: TLBundleB) => o.mask := edgeOut.mask(o.address, o.size) & Mux(hasData, helper(i.mask), ~UInt(0, width=outBytes))
        case (o: TLBundleC, i: TLBundleC) => o.error := reduce(i.error)
        case (o: TLBundleD, i: TLBundleD) => o.error := reduce(i.error)
        case _ => require(false, "Impossible bundle combination in WidthWidget")
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
        val repeated = Repeater(in, repeat)
        val cated = Wire(repeated)
        cated <> repeated
        edgeIn.data(cated.bits) := Cat(
          edgeIn.data(repeated.bits)(edgeIn.manager.beatBytes*8-1, edgeOut.manager.beatBytes*8),
          edgeIn.data(in.bits)(edgeOut.manager.beatBytes*8-1, 0))
        repeat := split(edgeIn, cated, edgeOut, out)
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
import freechips.rocketchip.unittest._

class TLRAMWidthWidget(first: Int, second: Int, txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("WidthWidget"))
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

class TLRAMWidthWidgetTest(little: Int, big: Int, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  io.finished := Module(LazyModule(new TLRAMWidthWidget(little,big,txns)).module).io.finished
}
