// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import scala.math.{min,max}

// innBeatBytes => the new client-facing bus width
class TLWidthWidget(innerBeatBytes: Int) extends LazyModule
{
  val node = TLAdapterNode(
    clientFn  = { case Seq(c) => c },
    managerFn = { case Seq(m) => m.copy(beatBytes = innerBeatBytes) })

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
      val size = edgeIn.size(in.bits)
      val hasData = edgeIn.hasData(in.bits)
      val addr_lo = in.bits match {
        case x: TLAddrChannel => edgeIn.address(x)
        case _ => UInt(0)
      }
      val addr = addr_lo >> log2Ceil(outBytes)

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
      val maskOut = maskMux(size) & edgeOut.mask(addr_lo, size)

      in.ready := out.ready || !last
      out.valid := in.valid && last
      out.bits := in.bits
      edgeOut.data(out.bits) := dataOut

      out.bits match {
        case a: TLBundleA => a.addr_hi := addr; a.mask := maskOut
        case b: TLBundleB => b.addr_hi := addr; b.mask := maskOut
        case c: TLBundleC => c.addr_hi := addr; c.addr_lo := addr_lo
        case d: TLBundleD => ()
          // addr_lo gets padded with 0s on D channel, the only lossy transform in this core
          // this should be safe, because we only care about addr_log on D to determine which
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
      val addr = in.bits match {
        case x: TLAddrChannel => edgeIn.address(x) >> log2Ceil(outBytes)
        case _ => UInt(0)
      }

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

      in.ready := out.ready && last
      out.valid := in.valid
      out.bits := in.bits
      edgeOut.data(out.bits) := dataOut

      out.bits match {
        case a: TLBundleA => a.addr_hi := addr; a.mask := maskOut
        case b: TLBundleB => b.addr_hi := addr; b.mask := maskOut
        case c: TLBundleC => c.addr_hi := addr
        case d: TLBundleD => ()
      }

      // addr_lo gets truncated automagically
    }
    
    def splice[T <: TLDataChannel](edgeIn: TLEdge, in: DecoupledIO[T], edgeOut: TLEdge, out: DecoupledIO[T]) = {
      if (edgeIn.manager.beatBytes == edgeOut.manager.beatBytes) {
        // nothing to do; pass it through
        out <> in
      } else if (edgeIn.manager.beatBytes > edgeOut.manager.beatBytes) {
        // split input to output
        split(edgeIn, in, edgeOut, out)
      } else {
        // merge input to output
        merge(edgeIn, in, edgeOut, out)
      }
    }

    val edgeOut = node.edgesOut(0)
    val edgeIn = node.edgesIn(0)
    val in = io.in(0)
    val out = io.out(0)

    splice(edgeIn,  in.a,  edgeOut, out.a)
    splice(edgeOut, out.d, edgeIn,  in.d)

    if (edgeOut.manager.anySupportAcquire && edgeIn.client.anySupportProbe) {
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

object TLWidthWidget
{
  // applied to the TL source node; connect (WidthWidget(x.node, 16) -> y.node)
  def apply(x: TLBaseNode, innerBeatBytes: Int)(implicit lazyModule: LazyModule, sourceInfo: SourceInfo): TLBaseNode = {
    val widget = LazyModule(new TLWidthWidget(innerBeatBytes))
    lazyModule.connect(x -> widget.node)
    widget.node
  }
}
