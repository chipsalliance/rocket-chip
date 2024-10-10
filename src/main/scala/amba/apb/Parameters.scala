// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import chisel3.util.{isPow2, log2Up}
import chisel3.experimental.SourceInfo

import org.chipsalliance.cde.config.Parameters

import org.chipsalliance.diplomacy.nodes.BaseNode

import freechips.rocketchip.diplomacy.{AddressSet, RegionType}
import freechips.rocketchip.resources.{Resource, Device, ResourceAddress, ResourcePermissions}
import freechips.rocketchip.util.{BundleField, BundleKeyBase, BundleFieldBase}

import scala.math.max

case class APBSubordinateParameters(
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

case class APBSubordinatePortParameters(
  subordinates:    Seq[APBSubordinateParameters],
  beatBytes: Int,
  responseFields: Seq[BundleFieldBase] = Nil,
  requestKeys:    Seq[BundleKeyBase]   = Nil)
{
  require (!subordinates.isEmpty)
  require (isPow2(beatBytes))

  val maxAddress = subordinates.map(_.maxAddress).max

  // Require disjoint ranges for addresses
  subordinates.combinations(2).foreach { case Seq(x,y) =>
    x.address.foreach { a => y.address.foreach { b =>
      require (!a.overlaps(b))
    } }
  }
}

case class APBManagerParameters(
  name:       String,
  nodePath:   Seq[BaseNode] = Seq())

case class APBManagerPortParameters(
  managers: Seq[APBManagerParameters],
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

  def apply(manager: APBManagerPortParameters, subordinate: APBSubordinatePortParameters) =
    new APBBundleParameters(
      addrBits   = log2Up(subordinate.maxAddress+1),
      dataBits   = subordinate.beatBytes * 8,
      requestFields  = BundleField.accept(manager.requestFields, subordinate.requestKeys),
      responseFields = BundleField.accept(subordinate.responseFields, manager.responseKeys))
}

case class APBEdgeParameters(
  manager: APBManagerPortParameters,
  subordinate:  APBSubordinatePortParameters,
  params: Parameters,
  sourceInfo: SourceInfo)
{
  val bundle = APBBundleParameters(manager, subordinate)
}
