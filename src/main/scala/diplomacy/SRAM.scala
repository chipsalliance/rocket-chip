// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel._
import freechips.rocketchip.config.Parameters

abstract class DiplomaticSRAM(
    address: AddressSet,
    beatBytes: Int,
    devName: Option[String])(implicit p: Parameters) extends LazyModule
{
  protected val resources = devName
    .map(new SimpleDevice(_, Seq("sifive,sram0")).reg("mem"))
    .getOrElse(new MemoryDevice().reg)

  def bigBits(x: BigInt, tail: List[Boolean] = Nil): List[Boolean] =
    if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)

  def mask: List[Boolean] = bigBits(address.mask >> log2Ceil(beatBytes))

  // Use single-ported memory with byte-write enable
  def makeSinglePortedByteWriteSeqMem(size: Int) = {
    // We require the address range to include an entire beat (for the write mask)
    require ((address.mask & (beatBytes-1)) == beatBytes-1)
    val mem = SeqMem(size, Vec(beatBytes, Bits(width = 8)))
    devName.foreach(n => mem.suggestName(n.split("-").last))
    mem
  }
}
