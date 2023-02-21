// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink.LFSRNoiseMaker

class APBRAM(
    address: AddressSet,
    cacheable: Boolean = true,
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
      regionType    = if (cacheable) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = executable,
      supportsRead  = true,
      supportsWrite = true)),
    beatBytes  = beatBytes)))

  private val outer = this

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with HasJustOneSeqMem {
    val (in, _) = node.in(0)
    val laneDataBits = 8
    val mem = makeSinglePortedByteWriteSeqMem(
      size = BigInt(1) << mask.filter(b=>b).size,
      lanes = beatBytes,
      bits = laneDataBits)
    val eccCode = None
    val address = outer.address

    val paddr = Cat((mask zip (in.paddr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    val legal = address.contains(in.paddr)

    val read = in.psel && !in.penable && !in.pwrite
    when (in.psel && !in.penable && in.pwrite && legal) {
      mem.write(paddr, VecInit.tabulate(beatBytes) { i => in.pwdata(8*(i+1)-1, 8*i) }, in.pstrb.asBools)
    }

    in.pready  := !fuzzReady.B || LFSRNoiseMaker(1)(0)
    in.pslverr := RegEnable(!legal, !in.penable) || (fuzzError.B && LFSRNoiseMaker(1)(0))
    in.prdata  := mem.readAndHold(paddr, read).asUInt
  }
}
