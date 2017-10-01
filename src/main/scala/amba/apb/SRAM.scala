// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class APBRAM(
    address: AddressSet,
    executable: Boolean = true,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil)
  (implicit p: Parameters) extends DiplomaticSRAM(address, beatBytes, devName)
{
  val node = APBSlaveNode(Seq(APBSlavePortParameters(
    Seq(APBSlaveParameters(
      address       = List(address) ++ errors,
      resources     = resources,
      regionType    = RegionType.UNCACHED,
      executable    = executable,
      supportsRead  = true,
      supportsWrite = true)),
    beatBytes  = beatBytes)))

  lazy val module = new LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val mem = makeSinglePortedByteWriteSeqMem(1 << mask.filter(b=>b).size)

    val paddr = Cat((mask zip (in.paddr >> log2Ceil(beatBytes)).toBools).filter(_._1).map(_._2).reverse)
    val legal = address.contains(in.paddr)

    val read = in.psel && !in.penable && !in.pwrite
    when (in.psel && !in.penable && in.pwrite && legal) {
      mem.write(paddr, Vec.tabulate(beatBytes) { i => in.pwdata(8*(i+1)-1, 8*i) }, in.pstrb.toBools)
    }

    in.pready  := Bool(true)
    in.pslverr := RegNext(!legal)
    in.prdata  := mem.readAndHold(paddr, read).asUInt
  }
}
