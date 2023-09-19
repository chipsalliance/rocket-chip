// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import chisel3.util._
import chisel3.experimental.SourceInfo
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
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

  def toResource: ResourceAddress = {
    ResourceAddress(address, ResourcePermissions(
      r = supportsRead,
      w = supportsWrite,
      x = executable,
      c = false,
      a = false))
  }
}

case class APBSlavePortParameters(
  slaves:    Seq[APBSlaveParameters],
  beatBytes: Int,
  responseFields: Seq[BundleFieldBase] = Nil,
  requestKeys:    Seq[BundleKeyBase]   = Nil)
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
  name:       String,
  nodePath:   Seq[BaseNode] = Seq())

case class APBMasterPortParameters(
  masters: Seq[APBMasterParameters],
  requestFields: Seq[BundleFieldBase] = Nil,
  responseKeys:  Seq[BundleKeyBase]   = Nil)

case class APBBundleParameters(
  addrBits: Int,
  dataBits: Int,
  requestFields:  Seq[BundleFieldBase] = Nil,
  responseFields: Seq[BundleFieldBase] = Nil)
{
  require (dataBits >= 8)
  require (addrBits >= 1)
  require (isPow2(dataBits))

  // Bring the globals into scope
  val protBits  = APBParameters.protBits

  def union(x: APBBundleParameters) =
    APBBundleParameters(
      max(addrBits, x.addrBits),
      max(dataBits, x.dataBits),
      BundleField.union(requestFields  ++ x.requestFields),
      BundleField.union(responseFields ++ x.responseFields))
}

object APBBundleParameters
{
  val emptyBundleParams = APBBundleParameters(addrBits = 1, dataBits = 8, requestFields = Nil, responseFields = Nil)
  def union(x: Seq[APBBundleParameters]) = x.foldLeft(emptyBundleParams)((x,y) => x.union(y))

  def apply(master: APBMasterPortParameters, slave: APBSlavePortParameters) =
    new APBBundleParameters(
      addrBits   = log2Up(slave.maxAddress+1),
      dataBits   = slave.beatBytes * 8,
      requestFields  = BundleField.accept(master.requestFields, slave.requestKeys),
      responseFields = BundleField.accept(slave.responseFields, master.responseKeys))
}

case class APBEdgeParameters(
  master: APBMasterPortParameters,
  slave:  APBSlavePortParameters,
  params: Parameters,
  sourceInfo: SourceInfo)
{
  val bundle = APBBundleParameters(master, slave)
}
