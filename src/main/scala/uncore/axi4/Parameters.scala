// See LICENSE.SiFive for license details.

package uncore.axi4

import Chisel._
import config._
import diplomacy._
import scala.math.max

case class AXI4SlaveParameters(
  address:       Seq[AddressSet],
  resources:     Seq[Resource] = Nil,
  regionType:    RegionType.T  = RegionType.GET_EFFECTS,
  executable:    Boolean       = false, // processor can execute from this memory
  nodePath:      Seq[BaseNode] = Seq(),
  supportsWrite: TransferSizes = TransferSizes.none,
  supportsRead:  TransferSizes = TransferSizes.none,
  interleavedId: Option[Int]   = None) // The device will not interleave read responses
{
  address.foreach { a => require (a.finite) }
  address.combinations(2).foreach { case Seq(x,y) => require (!x.overlaps(y)) }

  val name = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")
  val maxTransfer = max(supportsWrite.max, supportsRead.max)
  val maxAddress = address.map(_.max).max
  val minAlignment = address.map(_.alignment).min

  // The device had better not support a transfer larger than it's alignment
  require (minAlignment >= maxTransfer)
}

case class AXI4SlavePortParameters(
  slaves:     Seq[AXI4SlaveParameters],
  beatBytes:  Int,
  minLatency: Int = 1)
{
  require (!slaves.isEmpty)
  require (isPow2(beatBytes))

  val maxTransfer = slaves.map(_.maxTransfer).max
  val maxAddress = slaves.map(_.maxAddress).max

  // Check the link is not pointlessly wide
  require (maxTransfer >= beatBytes)
  // Check that the link can be implemented in AXI4
  require (maxTransfer <= beatBytes * (1 << AXI4Parameters.lenBits))

  lazy val routingMask = AddressDecoder(slaves.map(_.address))
  def findSafe(address: UInt) = Vec(slaves.map(_.address.map(_.contains(address)).reduce(_ || _)))
  def findFast(address: UInt) = Vec(slaves.map(_.address.map(_.widen(~routingMask)).distinct.map(_.contains(address)).reduce(_ || _)))

  // Require disjoint ranges for addresses
  slaves.combinations(2).foreach { case Seq(x,y) =>
    x.address.foreach { a => y.address.foreach { b =>
      require (!a.overlaps(b))
    } }
  }
}

case class AXI4MasterParameters(
  id:       IdRange       = IdRange(0, 1),
  aligned:  Boolean       = false,
  nodePath: Seq[BaseNode] = Seq())
{
  val name = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")
}

case class AXI4MasterPortParameters(
  masters: Seq[AXI4MasterParameters])
{
  val endId = masters.map(_.id.end).max

  // Require disjoint ranges for ids
  masters.combinations(2).foreach { case Seq(x,y) => require (!x.id.overlaps(y.id)) }
}

case class AXI4BundleParameters(
  addrBits: Int,
  dataBits: Int,
  idBits:   Int)
{
  require (dataBits >= 8)
  require (addrBits >= 1)
  require (idBits >= 1)
  require (isPow2(dataBits))

  // Bring the globals into scope
  val lenBits   = AXI4Parameters.lenBits
  val sizeBits  = AXI4Parameters.sizeBits
  val burstBits = AXI4Parameters.burstBits
  val lockBits  = AXI4Parameters.lockBits
  val cacheBits = AXI4Parameters.cacheBits
  val protBits  = AXI4Parameters.protBits
  val qosBits   = AXI4Parameters.qosBits
  val respBits  = AXI4Parameters.respBits

  def union(x: AXI4BundleParameters) =
    AXI4BundleParameters(
      max(addrBits, x.addrBits),
      max(dataBits, x.dataBits),
      max(idBits,   x.idBits))
}

object AXI4BundleParameters
{
  val emptyBundleParams = AXI4BundleParameters(addrBits=1, dataBits=8, idBits=1)
  def union(x: Seq[AXI4BundleParameters]) = x.foldLeft(emptyBundleParams)((x,y) => x.union(y))

  def apply(master: AXI4MasterPortParameters, slave: AXI4SlavePortParameters) =
    new AXI4BundleParameters(
      addrBits = log2Up(slave.maxAddress+1),
      dataBits = slave.beatBytes * 8,
      idBits   = log2Up(master.endId))
}

case class AXI4EdgeParameters(
  master: AXI4MasterPortParameters,
  slave:  AXI4SlavePortParameters)
{
  val bundle = AXI4BundleParameters(master, slave)
}
