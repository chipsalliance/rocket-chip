// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class TLROM(val base: BigInt, val size: Int, contentsDelayed: => Seq[Byte], executable: Boolean = true, beatBytes: Int = 4,
  resources: Seq[Resource] = new SimpleDevice("rom", Seq("sifive,rom0")).reg("mem"))(implicit p: Parameters) extends LazyModule
{

  val node = TLManagerNode(beatBytes, TLManagerParameters (
    address     = List(AddressSet(base, size-1)),
    resources   = resources,
    regionType  = RegionType.UNCACHED,
    executable  = executable,
    supportsGet = TransferSizes(1, beatBytes),
    fifoId      = Some(0)))

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
    }

    val contents = contentsDelayed
    val wrapSize = 1 << log2Ceil(contents.size)
    require (wrapSize <= size)

    val in = io.in(0)
    val edge = node.edgesIn(0)

    val words = (contents ++ Seq.fill(wrapSize-contents.size)(0.toByte)).grouped(beatBytes).toSeq
    val bigs = words.map(_.foldRight(BigInt(0)){ case (x,y) => (x.toInt & 0xff) | y << 8})
    val rom = Vec(bigs.map(x => UInt(x, width = 8*beatBytes)))

    in.d.valid := in.a.valid
    in.a.ready := in.d.ready

    val index = in.a.bits.address(log2Ceil(wrapSize)-1,log2Ceil(beatBytes))
    val high = if (wrapSize == size) UInt(0) else in.a.bits.address(log2Ceil(size)-1, log2Ceil(wrapSize))
    in.d.bits := edge.AccessAck(in.a.bits, Mux(high.orR, UInt(0), rom(index)))

    // Tie off unused channels
    in.b.valid := Bool(false)
    in.c.ready := Bool(true)
    in.e.ready := Bool(true)
  }
}
