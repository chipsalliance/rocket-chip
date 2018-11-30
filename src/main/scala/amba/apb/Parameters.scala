// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.math.max

case class APBSlaveParameters(
  address:       Seq[AddressSet],
  resources:     Seq[Resource] = Nil,
  regionType:    RegionType.T  = RegionType.GET_EFFECTS,
  executable:    Boolean       = false, // processor can execute from this memory
  nodePath:      Seq[BaseNode] = Seq(),
  supportsWrite: Boolean       = true,
  supportsRead:  Boolean       = true,
  device: Option[Device] = None)
{
  address.foreach { a => require (a.finite) }
    address.combinations(2).foreach { case Seq(x,y) => require (!x.overlaps(y)) }

  val name = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")
  val maxAddress = address.map(_.max).max
  val minAlignment = address.map(_.alignment).min
}

case class APBSlavePortParameters(
  slaves:    Seq[APBSlaveParameters],
  beatBytes: Int)
{
  require (!slaves.isEmpty)
  require (isPow2(beatBytes))

  val maxAddress = slaves.map(_.maxAddress).max

  // Require disjoint ranges for addresses
  slaves.combinations(2).foreach { case Seq(x,y) =>
    x.address.foreach { a => y.address.foreach { b =>
      require (!a.overlaps(b))
    } }
  }
}

case class APBMasterParameters(
  name:     String,
  nodePath: Seq[BaseNode] = Seq())

case class APBMasterPortParameters(
  masters: Seq[APBMasterParameters])

case class APBBundleParameters(
  addrBits: Int,
  dataBits: Int)
{
  require (dataBits >= 8)
  require (addrBits >= 1)
  require (isPow2(dataBits))

  // Bring the globals into scope
  val protBits  = APBParameters.protBits

  def union(x: APBBundleParameters) =
    APBBundleParameters(
      max(addrBits, x.addrBits),
      max(dataBits, x.dataBits))
}

object APBBundleParameters
{
  val emptyBundleParams = APBBundleParameters(addrBits = 1, dataBits = 8)
  def union(x: Seq[APBBundleParameters]) = x.foldLeft(emptyBundleParams)((x,y) => x.union(y))

  def apply(master: APBMasterPortParameters, slave: APBSlavePortParameters) =
    new APBBundleParameters(
      addrBits = log2Up(slave.maxAddress+1),
      dataBits = slave.beatBytes * 8)
}

case class APBEdgeParameters(
  master: APBMasterPortParameters,
  slave:  APBSlavePortParameters,
  params: Parameters,
  sourceInfo: SourceInfo)
{
  val bundle = APBBundleParameters(master, slave)
}
