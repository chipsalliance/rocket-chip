// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.math.max

case class AHBSlaveParameters(
  address:       Seq[AddressSet],
  resources:     Seq[Resource] = Nil,
  regionType:    RegionType.T  = RegionType.GET_EFFECTS,
  executable:    Boolean       = false, // processor can execute from this memory
  nodePath:      Seq[BaseNode] = Seq(),
  supportsWrite: TransferSizes = TransferSizes.none,
  supportsRead:  TransferSizes = TransferSizes.none,
  device: Option[Device] = None)
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

case class AHBSlavePortParameters(
  slaves:    Seq[AHBSlaveParameters],
  beatBytes: Int)
{
  require (!slaves.isEmpty)
  require (isPow2(beatBytes))

  val maxTransfer = slaves.map(_.maxTransfer).max
  val maxAddress = slaves.map(_.maxAddress).max

  // Check the link is not pointlessly wide
  require (maxTransfer >= beatBytes)
  // Check that the link can be implemented in AHB
  require (maxTransfer <= beatBytes * AHBParameters.maxTransfer)

  // Require disjoint ranges for addresses
  slaves.combinations(2).foreach { case Seq(x,y) =>
    x.address.foreach { a => y.address.foreach { b =>
      require (!a.overlaps(b))
    } }
  }
}

case class AHBMasterParameters(
  name:     String,
  nodePath: Seq[BaseNode] = Seq())

case class AHBMasterPortParameters(
  masters: Seq[AHBMasterParameters])

case class AHBBundleParameters(
  addrBits: Int,
  dataBits: Int)
{
  require (dataBits >= 8)
  require (addrBits >= 1)
  require (isPow2(dataBits))

  // Bring the globals into scope
  val transBits = AHBParameters.transBits
  val burstBits = AHBParameters.burstBits
  val protBits  = AHBParameters.protBits
  val sizeBits  = AHBParameters.sizeBits

  def union(x: AHBBundleParameters) =
    AHBBundleParameters(
      max(addrBits, x.addrBits),
      max(dataBits, x.dataBits))
}

object AHBBundleParameters
{
  val emptyBundleParams = AHBBundleParameters(addrBits = 1, dataBits = 8)
  def union(x: Seq[AHBBundleParameters]) = x.foldLeft(emptyBundleParams)((x,y) => x.union(y))

  def apply(master: AHBMasterPortParameters, slave: AHBSlavePortParameters) =
    new AHBBundleParameters(
      addrBits = log2Up(slave.maxAddress+1),
      dataBits = slave.beatBytes * 8)
}

case class AHBEdgeParameters(
  master: AHBMasterPortParameters,
  slave:  AHBSlavePortParameters,
  params: Parameters,
  sourceInfo: SourceInfo)
{
  val bundle = AHBBundleParameters(master, slave)
}
