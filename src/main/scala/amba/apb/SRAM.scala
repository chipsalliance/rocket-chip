// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree._
import freechips.rocketchip.diplomaticobjectmodel.model.OMTLRAM
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink.LFSRNoiseMaker

class APBRAM(
    address: AddressSet,
    executable: Boolean = true,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil,
    fuzzReady: Boolean = false,
    fuzzError: Boolean = false)
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
    val (mem, omMem) = makeSinglePortedByteWriteSeqMem(1 << mask.filter(b=>b).size)
    val width = beatBytes*8 // TODO Wes/Henry?
    val lanes = beatBytes   // TODO Wes/Henry?
    val addrBits = List[Boolean]()(32) // (mask zip edge.addr_hi(in.a.bits).asBools).filter(_._1).map(_._2) // TODO Wes/Henry?
    val size = 32 // 1 << addrBits.size // TODO Wes/Henry?
    val bits = width

    val paddr = Cat((mask zip (in.paddr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    val legal = address.contains(in.paddr)

    val read = in.psel && !in.penable && !in.pwrite
    when (in.psel && !in.penable && in.pwrite && legal) {
      mem.write(paddr, Vec.tabulate(beatBytes) { i => in.pwdata(8*(i+1)-1, 8*i) }, in.pstrb.asBools)
    }

    in.pready  := Bool(!fuzzReady) || LFSRNoiseMaker(1)(0)
    in.pslverr := RegEnable(!legal, !in.penable) || (Bool(fuzzError) && LFSRNoiseMaker(1)(0))
    in.prdata  := mem.readAndHold(paddr, read).asUInt
  }
}
