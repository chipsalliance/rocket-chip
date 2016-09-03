// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

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

    val edge = node.edgesOut(0)
    val outerBeatBytes = edge.manager.beatBytes
    require (outerBeatBytes < innerBeatBytes)

    val ratio = innerBeatBytes / outerBeatBytes
    val bce = edge.manager.anySupportAcquire && edge.client.anySupportProbe

    def UIntToOH1(x: UInt, width: Int) = (UInt((1 << width) - 1) << x)(width*2-1, width)
    def trailingZeros(x: Int) = if (x > 0) Some(log2Ceil(x & -x)) else None

    def split(in: HasTLData, fire: Bool): (Bool, UInt, UInt) = {
      val dataSlices = Vec.tabulate (ratio) { i => in.data()((i+1)*outerBeatBytes*8-1, i*outerBeatBytes*8) }
      val maskSlices = Vec.tabulate (ratio) { i => in.mask()((i+1)*outerBeatBytes  -1, i*outerBeatBytes)   }
      val filter = Reg(UInt(width = ratio), init = SInt(-1, width = ratio).asUInt)
      val mask = maskSlices.map(_.orR)
      val hasData = in.hasData()

      // decoded_size = 1111 (for smallest), 0101, 0001 (for largest)
      val sizeOH1 = UIntToOH1(in.size(), log2Ceil(innerBeatBytes)) >> log2Ceil(outerBeatBytes)
      val decoded_size = Seq.tabulate(ratio) { i => trailingZeros(i).map(!sizeOH1(_)).getOrElse(Bool(true)) }

      val first = filter(ratio-1)
      val new_filter = Mux(first, Cat(decoded_size.reverse), filter << 1)
      val last = new_filter(ratio-1) || !hasData
      when (fire) {
        filter := new_filter 
        when (!hasData) { filter := SInt(-1, width = ratio).asUInt }
      }

      val select = Cat(mask.reverse) & new_filter
      // !!! if never data
      (last, Mux1H(select, dataSlices), Mux1H(select, maskSlices))
    }

    def merge(in: HasTLData, fire: Bool): (Bool, UInt) = {
      val count = RegInit(UInt(0, width = log2Ceil(ratio)))
      val rdata = Reg(UInt(width = (ratio-1)*outerBeatBytes*8))
      val data = rdata << outerBeatBytes*8 | in.data()
      val first = count === UInt(0)
      val limit = UIntToOH1(in.size(), log2Ceil(innerBeatBytes)) >> log2Ceil(outerBeatBytes)
      val last = count === limit || !in.hasData()
      val cases = Vec.tabulate (log2Ceil(ratio)+1) { i => 
        val pow = 1 << i
        Fill(1 << (ratio-i), data((pow+1)*outerBeatBytes*8-1, pow*outerBeatBytes*8))
      }

      when (fire) {
        rdata := data
        count := count + UInt(1)
        when (last) { count := UInt(0) }
      }

      // !!! if never data
      (last, Mux1H(limit, cases))
    }

    val in = io.in(0)
    val out = io.out(0)

    val (alast, adata, amask) = split(in.a.bits, out.a.fire())
    in.a.ready := out.a.ready && alast
    out.a.valid := in.a.valid
    out.a.bits := in.a.bits
    out.a.bits.data := adata
    out.a.bits.mask := amask

    val (dlast, ddata) = merge(out.d.bits, out.d.fire())
    out.d.ready := in.d.ready
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
    }
  }
}

object TLNarrower
{
  // applied to the TL source node; connect (Narrower(x.node, 16) -> y.node)
  def apply(x: TLBaseNode, innerBeatBytes: Int)(implicit lazyModule: LazyModule): TLBaseNode = {
    val narrower = LazyModule(new TLNarrower(innerBeatBytes))
    lazyModule.connect(x -> narrower.node)
    narrower.node
  }
}
