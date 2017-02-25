// See LICENSE.SiFive for license details.

package uncore.axi4

import Chisel._
import config._
import diplomacy._
import util._

class AXI4RAM(address: AddressSet, executable: Boolean = true, beatBytes: Int = 4)(implicit p: Parameters) extends LazyModule
{
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = List(address),
      regionType    = RegionType.UNCACHED,
      executable    = executable,
      supportsRead  = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes),
      interleavedId = Some(0))),
    beatBytes  = beatBytes,
    minLatency = 0))) // B responds on same cycle

  // We require the address range to include an entire beat (for the write mask)
  require ((address.mask & (beatBytes-1)) == beatBytes-1)

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
    }

    def bigBits(x: BigInt, tail: List[Boolean] = List.empty[Boolean]): List[Boolean] =
      if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)
    val mask = bigBits(address.mask >> log2Ceil(beatBytes))

    val in = io.in(0)
    val mem = SeqMem(1 << mask.filter(b=>b).size, Vec(beatBytes, Bits(width = 8)))

    val r_addr = Cat((mask zip (in.ar.bits.addr >> log2Ceil(beatBytes)).toBools).filter(_._1).map(_._2).reverse)
    val w_addr = Cat((mask zip (in.aw.bits.addr >> log2Ceil(beatBytes)).toBools).filter(_._1).map(_._2).reverse)

    in.aw.ready := in. w.valid && in.b.ready
    in. w.ready := in.aw.valid && in.b.ready
    in. b.valid := in.w.valid && in.aw.valid

    in.b.bits.id   := in.aw.bits.id
    in.b.bits.resp := AXI4Parameters.RESP_OKAY
    val wdata = Vec.tabulate(beatBytes) { i => in.w.bits.data(8*(i+1)-1, 8*i) }
    when (in.b.fire()) {
      mem.write(w_addr, wdata, in.w.bits.strb.toBools)
    }

    val r_full = RegInit(Bool(false))
    val r_id   = Reg(UInt())

    when (in. r.fire()) { r_full := Bool(false) }
    when (in.ar.fire()) { r_full := Bool(true) }

    in. r.valid := r_full
    in.ar.ready := in.r.ready || !r_full

    when (in.ar.fire()) {
      r_id := in.ar.bits.id
    }

    val ren = in.ar.fire()
    val rdata = mem.readAndHold(r_addr, ren)

    in.r.bits.id   := r_id
    in.r.bits.resp := AXI4Parameters.RESP_OKAY
    in.r.bits.data := Cat(rdata.reverse)
    in.r.bits.last := Bool(true)
  }
}
