// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import scala.math.{min,max}

// innBeatBytes => the bus width after the adapter
class TLNarrower(innerBeatBytes: Int) extends LazyModule
{
  val node = TLAdapterNode(
    clientFn  = { case Seq(c) => c },
    managerFn = { case Seq(m) => m.copy(beatBytes = innerBeatBytes) })

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    val edgeOut = node.edgesOut(0)
    val edgeIn  = node.edgesIn(0)
    val outerBeatBytes = edgeOut.manager.beatBytes
    require (outerBeatBytes < innerBeatBytes)

    val ratio = innerBeatBytes / outerBeatBytes
    val bce = edgeOut.manager.anySupportAcquire && edgeIn.client.anySupportProbe

    def trailingZeros(x: Int) = if (x > 0) Some(log2Ceil(x & -x)) else None

    def split(edge: TLEdge, in: TLDataChannel, fire: Bool): (Bool, UInt, UInt) = {
      val dataSlices = Vec.tabulate (ratio) { i => edge.data(in)((i+1)*outerBeatBytes*8-1, i*outerBeatBytes*8) }
      val maskSlices = Vec.tabulate (ratio) { i => edge.mask(in)((i+1)*outerBeatBytes  -1, i*outerBeatBytes)   }
      val filter = Reg(UInt(width = ratio), init = SInt(-1, width = ratio).asUInt)
      val mask = maskSlices.map(_.orR)
      val hasData = edge.hasData(in)

      // decoded_size = 1111 (for smallest), 0101, 0001 (for largest)
      val sizeOH1 = UIntToOH1(edge.size(in), log2Ceil(innerBeatBytes)) >> log2Ceil(outerBeatBytes)
      val decoded_size = Seq.tabulate(ratio) { i => trailingZeros(i).map(!sizeOH1(_)).getOrElse(Bool(true)) }

      val first = filter(ratio-1)
      val new_filter = Mux(first, Cat(decoded_size.reverse), filter << 1)
      val last = new_filter(ratio-1) || !hasData
      when (fire) {
        filter := new_filter 
        when (!hasData) { filter := SInt(-1, width = ratio).asUInt }
      }

      if (edge.staticHasData(in) == Some(false)) {
        (Bool(true), UInt(0), UInt(0))
      } else {
        val select = Cat(mask.reverse) & new_filter
        (last, Mux1H(select, dataSlices), Mux1H(select, maskSlices))
      }
    }

    def merge(edge: TLEdge, in: TLDataChannel, fire: Bool): (Bool, UInt) = {
      val count = RegInit(UInt(0, width = log2Ceil(ratio)))
      val rdata = Reg(UInt(width = (ratio-1)*outerBeatBytes*8))
      val data = Cat(edge.data(in), rdata)
      val first = count === UInt(0)
      val limit = UIntToOH1(edge.size(in), log2Ceil(innerBeatBytes)) >> log2Ceil(outerBeatBytes)
      val last = count === limit || !edge.hasData(in)

      when (fire) {
        rdata := data >> outerBeatBytes*8
        count := count + UInt(1)
        when (last) { count := UInt(0) }
      }

      val cases = Seq.tabulate(log2Ceil(ratio)+1) { i =>
        val high = innerBeatBytes*8
        val take = (1 << i)*outerBeatBytes*8
        Fill(1 << (log2Ceil(ratio)-i), data(high-1, high-take))
      }
      val mux = Vec.tabulate(log2Ceil(edge.maxTransfer)+1) { lgSize =>
        cases(min(max(lgSize - log2Ceil(outerBeatBytes), 0), log2Ceil(ratio)))
      }

      if (edge.staticHasData(in) == Some(false)) {
        (Bool(true), UInt(0))
      } else {
        (last, mux(edge.size(in)))
      }
    }

    val in = io.in(0)
    val out = io.out(0)

    val (alast, adata, amask) = split(edgeIn, in.a.bits, out.a.fire())
    in.a.ready := out.a.ready && alast
    out.a.valid := in.a.valid
    out.a.bits := in.a.bits
    out.a.bits.addr_hi := Cat(in.a.bits.addr_hi, edgeIn.addr_lo(in.a.bits) >> log2Ceil(outerBeatBytes))
    out.a.bits.data := adata
    out.a.bits.mask := amask

    val (dlast, ddata) = merge(edgeOut, out.d.bits, out.d.fire())
    out.d.ready := in.d.ready || !dlast
    in.d.valid := out.d.valid && dlast
    in.d.bits := out.d.bits
    in.d.bits.data := ddata

    if (bce) {
      require (false)
      // C has no wmask !!!
//      val (clast, cdata, cmask) = split(in.c.bits, out.c.fire())
//      in.c.ready := out.c.ready && clast
//      out.c.valid := in.c.valid
//      out.c.bits := in.c.bits
//      out.c.bits.data := cdata
//      out.c.bits.mask := cmask

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

object TLNarrower
{
  // applied to the TL source node; connect (Narrower(x.node, 16) -> y.node)
  def apply(x: TLBaseNode, innerBeatBytes: Int)(implicit lazyModule: LazyModule, sourceInfo: SourceInfo): TLBaseNode = {
    val narrower = LazyModule(new TLNarrower(innerBeatBytes))
    lazyModule.connect(x -> narrower.node)
    narrower.node
  }
}
