// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel._
import chisel3.core.SyncReadMem
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{BusMemoryLogicalTreeNode, LogicalModuleTree, LogicalTreeNode}
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.util.DescribedSRAM

abstract class DiplomaticSRAM(
    address: AddressSet,
    beatBytes: Int,
    parentLogicalTreeNode: Option[LogicalTreeNode],
    devName: Option[String])(implicit p: Parameters) extends LazyModule
{
  val device = devName
    .map(new SimpleDevice(_, Seq("sifive,sram0")))
    .getOrElse(new MemoryDevice())

  def getOMMemRegions(): Seq[OMMemoryRegion] = {
    val resourceBindings = LogicalModuleTree.getResourceBindings(device)
    DiplomaticObjectModelAddressing.getOMMemoryRegions(devName.getOrElse(""), resourceBindings)
  }

  val resources = device.reg("mem")

  def bigBits(x: BigInt, tail: List[Boolean] = Nil): List[Boolean] =
    if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)

  def mask: List[Boolean] = bigBits(address.mask >> log2Ceil(beatBytes))

  // Use single-ported memory with byte-write enable
  def makeSinglePortedByteWriteSeqMem(
    size: Int,
    lanes: Int = beatBytes,
    bits: Int = 8,
    busProtocol: Option[OMProtocol],
    dataECC: Option[OMECC] = None,
    hasAtomics: Option[Boolean] = None)= { // chisel3.SyncReadMem[Vec[Chisel.UInt]]
    // We require the address range to include an entire beat (for the write mask)

    val mem = DescribedSRAM(
      name = devName.getOrElse("mem"),
      desc = devName.getOrElse("mem"),
      size = size,
      data = Vec(lanes, UInt(width = bits))
    )

    val omSRAM: OMSRAM = DiplomaticObjectModelAddressing.makeOMSRAM(
      desc = "mem", //lim._2.name.map(n => n).getOrElse(lim._1.name),
      depth = size,
      data = Vec(lanes, UInt(width = bits))
    )

    parentLogicalTreeNode.map {
      case parentLTN =>
        def sramLogicalTreeNode = new BusMemoryLogicalTreeNode(
          device = () => device,
          omSRAMs = Seq(omSRAM),
          busProtocol = busProtocol.getOrElse(throw new IllegalArgumentException("Protocol not specified")),
          dataECC = dataECC,
          hasAtomics = hasAtomics,
          busProtocolSpecification = None)
        LogicalModuleTree.add(parentLTN, sramLogicalTreeNode)
    }

    val omMem: OMMemory = DiplomaticObjectModelAddressing.makeOMMemory(
      desc = "mem", //lim._2.name.map(n => n).getOrElse(lim._1.name),
      depth = size,
      data = Vec(lanes, UInt(width = bits))
    )

    (mem, Seq(omMem))
  }
}
