// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel.{Data, _}
import chisel3.SyncReadMem
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.logicaltree._
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.util.DescribedSRAM

case class SRAMInfo[T <: Data](
  mem: SyncReadMem[T],
  omMem: Seq[OMMemory],
  sramLogicalTreeNodes: Option[MemoryLogicalTreeNode[T]] = None
)

abstract class DiplomaticSRAM(
    address: AddressSet,
    beatBytes: Int,
    devName: Option[String],
    logicalTreeNode: Option[LogicalTreeNode] = None)(implicit p: Parameters) extends LazyModule
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

  def makeMemoryLogicalTreeNode[T <: Data](
    describedSRAM: DescribedSRAM[T],
    logicalTreeNode: LogicalTreeNode): MemoryLogicalTreeNode[T] = {
      def sramLogicalTreeNode: MemoryLogicalTreeNode[T] = new MemoryLogicalTreeNode(describedSRAM)
      LogicalModuleTree.add(logicalTreeNode, sramLogicalTreeNode)
      sramLogicalTreeNode
  }

  // Use single-ported memory with byte-write enable
  def makeSinglePortedByteWriteSeqMem[T <: Data](data: T, description: String, architecture: RAMArchitecture, size: Int,
    logicalTreeNode: Option[LogicalTreeNode]): SyncReadMem[T] ={
    // We require the address range to include an entire beat (for the write mask)
    val describedSRAM = DescribedSRAM.build(name = devName.getOrElse("mem"), desc = devName.getOrElse("mem"), size = size, data = data)

    logicalTreeNode.map {
      case parentLTN =>
        val childLTN = makeMemoryLogicalTreeNode(describedSRAM, parentLTN)
        LogicalModuleTree.add(parentLTN, childLTN)
    }

    describedSRAM.mem
  }
}

