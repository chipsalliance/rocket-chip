// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import chisel3._
import chisel3.util.log2Ceil
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.{DescribedSRAM, Code}

abstract class DiplomaticSRAM(
    val address: AddressSet,
    beatBytes: Int,
    devName: Option[String],
    dtsCompat: Option[Seq[String]] = None,
    devOverride: Option[Device with DeviceRegName] = None)(implicit p: Parameters) extends LazyModule
{
  val device = devOverride.getOrElse(devName
    .map(new SimpleDevice(_, dtsCompat.getOrElse(Seq("sifive,sram0"))))
    .getOrElse(new MemoryDevice())
  )

  val resources = device.reg("mem")

  def bigBits(x: BigInt, tail: List[Boolean] = Nil): List[Boolean] =
    if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)

  def mask: List[Boolean] = bigBits(address.mask >> log2Ceil(beatBytes))

  // Use single-ported memory with byte-write enable
  def makeSinglePortedByteWriteSeqMem(size: BigInt, lanes: Int = beatBytes, bits: Int = 8) = {
    // We require the address range to include an entire beat (for the write mask)

    val mem  =  DescribedSRAM(
      name = devName.getOrElse("mem"),
      desc = devName.getOrElse("mem"),
      size = size,
      data = Vec(lanes, UInt(bits.W))
    )
    devName.foreach(n => mem.suggestName(n.split("-").last))

    mem
  }
}

/** Represents a single seq mem mapped to a single, contiguous address space
  */
trait HasJustOneSeqMem {

  /** A reference to the chisel memory mapped to this address
    *
    * Each element of the Vec type is a lane
    */
  def mem: SyncReadMem[Vec[UInt]]

  /** The number of bits used for data in a single lane
    *
    * laneDataBits + laneECCBits should equal the total width of a lane
    */
  def laneDataBits: Int

    /** The ecc code used by this memory
    */
  def eccCode: Option[Code]

  /** The address set this memory is mapped to
    */
  def address: AddressSet
}
