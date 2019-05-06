// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.model.{OMSRAM, OMMemoryRegion, OMRTLInterface, OMRTLModule}
import freechips.rocketchip.util.DescribedSRAM

abstract class DiplomaticSRAM(
    address: AddressSet,
    beatBytes: Int,
    devName: Option[String])(implicit p: Parameters) extends LazyModule
{
  val device = devName
    .map(new SimpleDevice(_, Seq("sifive,sram0")))
    .getOrElse(new MemoryDevice())

  def getOMMemRegions(resourceBindingsMap: ResourceBindingsMap): Seq[OMMemoryRegion] = {
    val resourceBindings = DiplomaticObjectModelAddressing.getResourceBindings(device, resourceBindingsMap)
    require(resourceBindings.isDefined)
    resourceBindings.map(DiplomaticObjectModelAddressing.getOMMemoryRegions(devName.getOrElse(""), _)).getOrElse(Nil) // TODO name source???
  }

  val resources = device.reg("mem")

  def bigBits(x: BigInt, tail: List[Boolean] = Nil): List[Boolean] =
    if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)

  def mask: List[Boolean] = bigBits(address.mask >> log2Ceil(beatBytes))

  // Use single-ported memory with byte-write enable
  def makeSinglePortedByteWriteSeqMem(size: Int, lanes: Int = beatBytes, bits: Int = 8, logicalTreeNode: Option[LogicalTreeNode]) = {
    // We require the address range to include an entire beat (for the write mask)
    val mem =  DescribedSRAM(
      name = devName.getOrElse("mem"),
      desc = devName.getOrElse("mem"),
      size = size,
      data = Vec(lanes, UInt(width = bits))
    )
    devName.foreach(n => mem.suggestName(n.split("-").last))

    val omMem: OMSRAM = DiplomaticObjectModelAddressing.makeOMMemory(
      desc = "mem", //lim._2.name.map(n => n).getOrElse(lim._1.name),
      depth = size,
      data = Vec(lanes, UInt(width = bits))
    )


    ): DescribedSRAM[T] = {
      // We require the address range to include an entire beat (for the write mask)	    // We require the address range to include an entire beat (for the write mask)
      val mem =  DescribedSRAM(	    val describedSRAM = DescribedSRAM.build(name = devName.getOrElse("mem"), desc = devName.getOrElse("mem"), size = size, data = data)
      name = devName.getOrElse("mem"),
      desc = devName.getOrElse("mem"),	    logicalTreeNode.map {
        size = size,	      case parentLTN =>
          data = Vec(lanes, UInt(width = bits))	        def sramLogicalTreeNode: MemoryLogicalTreeNode[T] = new MemoryLogicalTreeNode(describedSRAM)
        )	        LogicalModuleTree.add(parentLTN, new MemoryLogicalTreeNode(describedSRAM))
        devName.foreach(n => mem.suggestName(n.split("-").last))	    }


    (mem, Seq(omMem))
  }
}
