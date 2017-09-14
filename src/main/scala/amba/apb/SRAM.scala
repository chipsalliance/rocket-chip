// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class APBRAM(address: AddressSet, executable: Boolean = true, beatBytes: Int = 4, errors: Seq[AddressSet] = Nil)(implicit p: Parameters) extends LazyModule
{
  val node = APBSlaveNode(Seq(APBSlavePortParameters(
    Seq(APBSlaveParameters(
      address       = List(address) ++ errors,
      regionType    = RegionType.UNCACHED,
      executable    = executable,
      supportsRead  = true,
      supportsWrite = true)),
    beatBytes  = beatBytes)))

  // We require the address range to include an entire beat (for the write mask)
  require ((address.mask & (beatBytes-1)) == beatBytes-1)

  lazy val module = new LazyModuleImp(this) {
    val (in, _) = node.in(0)

    def bigBits(x: BigInt, tail: List[Boolean] = List.empty[Boolean]): List[Boolean] =
      if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)
    val mask = bigBits(address.mask >> log2Ceil(beatBytes))
    val paddr = Cat((mask zip (in.paddr >> log2Ceil(beatBytes)).toBools).filter(_._1).map(_._2).reverse)
    val legal = address.contains(in.paddr)

    // Use single-ported memory with byte-write enable
    val mem = SeqMem(1 << mask.filter(b=>b).size, Vec(beatBytes, Bits(width = 8)))

    val read = in.psel && !in.penable && !in.pwrite
    when (in.psel && !in.penable && in.pwrite && legal) {
      mem.write(paddr, Vec.tabulate(beatBytes) { i => in.pwdata(8*(i+1)-1, 8*i) }, in.pstrb.toBools)
    }

    in.pready  := Bool(true)
    in.pslverr := RegNext(!legal)
    in.prdata  := mem.readAndHold(paddr, read).asUInt
  }
}
