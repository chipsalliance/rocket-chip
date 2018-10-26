// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
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

      val corrupt_reg = RegInit(Bool(false))
      val corrupt_in = edgeIn.corrupt(in.bits)
      val corrupt_out = corrupt_in || corrupt_reg

      when (in.fire()) {
        count := count + UInt(1)
        corrupt_reg := corrupt_out
        when (last) {
          count := UInt(0)
          corrupt_reg := Bool(false)
        }
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

      in.ready := out.ready || !last
      out.valid := in.valid && last
      out.bits := in.bits

      // Don't put down hardware if we never carry data
      edgeOut.data(out.bits) := (if (edgeIn.staticHasData(in.bits) == Some(false)) UInt(0) else helper(edgeIn.data(in.bits)))
      edgeOut.corrupt(out.bits) := corrupt_out

      (out.bits, in.bits) match {
        case (o: TLBundleA, i: TLBundleA) => o.mask := edgeOut.mask(o.address, o.size) & Mux(hasData, helper(i.mask), ~UInt(0, width=outBytes))
        case (o: TLBundleB, i: TLBundleB) => o.mask := edgeOut.mask(o.address, o.size) & Mux(hasData, helper(i.mask), ~UInt(0, width=outBytes))
        case (o: TLBundleC, i: TLBundleC) => ()
        case (o: TLBundleD, i: TLBundleD) => ()
        case _ => require(false, "Impossible bundle combination in WidthWidget")
      }
    }

    def split[T <: TLDataChannel](edgeIn: TLEdge, in: DecoupledIO[T], edgeOut: TLEdge, out: DecoupledIO[T], sourceMap: UInt => UInt) = {
      val inBytes = edgeIn.manager.beatBytes
      val outBytes = edgeOut.manager.beatBytes
      val ratio = inBytes / outBytes
      val keepBits  = log2Ceil(inBytes)
      val dropBits  = log2Ceil(outBytes)
      val countBits = log2Ceil(ratio)

      val size    = edgeIn.size(in.bits)
      val hasData = edgeIn.hasData(in.bits)
      val limit   = UIntToOH1(size, keepBits) >> dropBits

      val count = RegInit(UInt(0, width = countBits))
      val first = count === UInt(0)
      val last  = count === limit || !hasData

      when (out.fire()) {
        count := count + UInt(1)
        when (last) { count := UInt(0) }
      }

      // For sub-beat transfer, extract which part matters
      val sel = in.bits match {
        case a: TLBundleA => a.address(keepBits-1, dropBits)
        case b: TLBundleB => b.address(keepBits-1, dropBits)
        case c: TLBundleC => c.address(keepBits-1, dropBits)
        case d: TLBundleD => {
          val sel = sourceMap(d.source)
          val hold = Mux(first, sel, RegEnable(sel, first)) // a_first is not for whole xfer
          hold & ~limit // if more than one a_first/xfer, the address must be aligned anyway
        }
      }

      val index  = sel | count
      def helper(idata: UInt, width: Int): UInt = {
        val mux = Vec.tabulate(ratio) { i => idata((i+1)*outBytes*width-1, i*outBytes*width) }
        mux(index)
      }

      out <> in

      // Don't put down hardware if we never carry data
      edgeOut.data(out.bits) := (if (edgeIn.staticHasData(in.bits) == Some(false)) UInt(0) else helper(edgeIn.data(in.bits), 8))

      (out.bits, in.bits) match {
        case (o: TLBundleA, i: TLBundleA) => o.mask := helper(i.mask, 1)
        case (o: TLBundleB, i: TLBundleB) => o.mask := helper(i.mask, 1)
        case (o: TLBundleC, i: TLBundleC) => () // replicating corrupt to all beats is ok
        case (o: TLBundleD, i: TLBundleD) => ()
        case _ => require(false, "Impossbile bundle combination in WidthWidget")
      }

      // Repeat the input if we're not last
      !last
    }
    
    def splice[T <: TLDataChannel](edgeIn: TLEdge, in: DecoupledIO[T], edgeOut: TLEdge, out: DecoupledIO[T], sourceMap: UInt => UInt) = {
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
        repeat := split(edgeIn, cated, edgeOut, out, sourceMap)
      } else {
        // merge input to output
        merge(edgeIn, in, edgeOut, out)
      }
    }

    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>

      // If the master is narrower than the slave, the D channel must be narrowed.
      // This is tricky, because the D channel has no address data.
      // Thus, you don't know which part of a sub-beat transfer to extract.
      // To fix this, we record the relevant address bits for all sources.
      // The assumption is that this sort of situation happens only where
      // you connect a narrow master to the system bus, so there are few sources.

      def sourceMap(source: UInt) = {
        require (edgeOut.manager.beatBytes > edgeIn.manager.beatBytes)
        val keepBits = log2Ceil(edgeOut.manager.beatBytes)
        val dropBits = log2Ceil(edgeIn.manager.beatBytes)
        val sources  = Reg(Vec(edgeIn.client.endSourceId, UInt(width = keepBits-dropBits)))
        val a_sel = in.a.bits.address(keepBits-1, dropBits)
        when (in.a.fire()) {
          sources(in.a.bits.source) := a_sel
        }

        // depopulate unused source registers:
        edgeIn.client.unusedSources.foreach { id => sources(id) := UInt(0) }

        val bypass = in.a.valid && in.a.bits.source === source
        if (edgeIn.manager.minLatency > 0) sources(source)
        else Mux(bypass, a_sel, sources(source))
      }

      splice(edgeIn,  in.a,  edgeOut, out.a, sourceMap)
      splice(edgeOut, out.d, edgeIn,  in.d,  sourceMap)

      if (edgeOut.manager.anySupportAcquireB && edgeIn.client.anySupportProbe) {
        splice(edgeOut, out.b, edgeIn,  in.b,  sourceMap)
        splice(edgeIn,  in.c,  edgeOut, out.c, sourceMap)
        out.e <> in.e
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
  def apply(innerBeatBytes: Int)(implicit p: Parameters): TLNode =
  {
    val widget = LazyModule(new TLWidthWidget(innerBeatBytes))
    widget.node
  }
  def apply(wrapper: TLBusWrapper)(implicit p: Parameters): TLNode = apply(wrapper.beatBytes)
}

/** Synthesizeable unit tests */
import freechips.rocketchip.unittest._

class TLRAMWidthWidget(first: Int, second: Int, txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("WidthWidget"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x3ff)))

  (ram.node
    := TLDelayer(0.1)
    := TLFragmenter(4, 256)
    := TLWidthWidget(second)
    := TLWidthWidget(first)
    := TLDelayer(0.1)
    := model.node
    := fuzz.node)

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMWidthWidgetTest(little: Int, big: Int, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new TLRAMWidthWidget(little,big,txns)).module)
  io.finished := dut.io.finished
}
