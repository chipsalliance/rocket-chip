// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import chisel3.experimental.SourceInfo
import chisel3.util._

import org.chipsalliance.cde.config.Parameters

import org.chipsalliance.diplomacy.nodes.BaseNode

import freechips.rocketchip.resources.{Resource, Device, ResourceAddress, ResourcePermissions}
import freechips.rocketchip.diplomacy.{AddressSet, RegionType, TransferSizes}
import freechips.rocketchip.util.{BundleField, BundleFieldBase, BundleKeyBase}

import scala.math.{max, min}

case class AHBSubordinateParameters(
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
  val minMaxTransfer = min(supportsWrite.max, supportsRead.max)
  val maxTransfer = max(supportsWrite.max, supportsRead.max)
  val maxAddress = address.map(_.max).max
  val minAlignment = address.map(_.alignment).min

  // The device had better not support a transfer larger than it's alignment
  require (minAlignment >= maxTransfer)

  def toResource: ResourceAddress = {
    ResourceAddress(address, ResourcePermissions(
      r = supportsRead,
      w = supportsWrite,
      x = executable,
      c = false,
      a = false))
  }
}

case class AHBSubordinatePortParameters(
  subordinates:     Seq[AHBSubordinateParameters],
  beatBytes:  Int,
  lite:       Boolean,
  responseFields: Seq[BundleFieldBase] = Nil,
  requestKeys:    Seq[BundleKeyBase]   = Nil)
{
  require (!subordinates.isEmpty)
  require (isPow2(beatBytes))

  val minMaxTransfer = subordinates.map(_.minMaxTransfer).min // useful for fragmentation
  val maxTransfer = subordinates.map(_.maxTransfer).max
  val maxAddress = subordinates.map(_.maxAddress).max

  // Check the link is not pointlessly wide
  require (maxTransfer >= beatBytes)
  // Check that the link can be implemented in AHB
  require (maxTransfer <= beatBytes * AHBParameters.maxTransfer)

  // Require disjoint ranges for addresses
  subordinates.combinations(2).foreach { case Seq(x,y) =>
    x.address.foreach { a => y.address.foreach { b =>
      require (!a.overlaps(b))
    } }
  }
}

case class AHBManagerParameters(
  name:     String,
  nodePath: Seq[BaseNode] = Nil)

case class AHBManagerPortParameters(
  managers: Seq[AHBManagerParameters],
  requestFields: Seq[BundleFieldBase] = Nil,
  responseKeys:  Seq[BundleKeyBase]   = Nil)

case class AHBBundleParameters(
  addrBits:   Int,
  dataBits:   Int,
  requestFields:  Seq[BundleFieldBase],
  responseFields: Seq[BundleFieldBase],
  lite:       Boolean)
{
  require (dataBits >= 8)
  require (addrBits >= 1)
  require (isPow2(dataBits))

  // Bring the globals into scope
  val transBits = AHBParameters.transBits
  val burstBits = AHBParameters.burstBits
  val protBits  = AHBParameters.protBits
  val sizeBits  = AHBParameters.sizeBits
  val hrespBits = if (lite) 1 else AHBParameters.hrespBits

  def union(x: AHBBundleParameters) = {
    require (x.lite == lite)
    AHBBundleParameters(
      max(addrBits, x.addrBits),
      max(dataBits, x.dataBits),
      BundleField.union(requestFields ++ x.requestFields),
      BundleField.union(responseFields ++ x.responseFields),
      lite)
  }
}

object AHBBundleParameters
{
  val emptyBundleParams = AHBBundleParameters(addrBits = 1, dataBits = 8, requestFields = Nil, responseFields = Nil, lite = true)
  def union(x: Seq[AHBBundleParameters]) =
    if (x.isEmpty) emptyBundleParams else x.tail.foldLeft(x.head)((x,y) => x.union(y))

  def apply(manager: AHBManagerPortParameters, subordinate: AHBSubordinatePortParameters) =
    new AHBBundleParameters(
      addrBits = log2Up(subordinate.maxAddress+1),
      dataBits = subordinate.beatBytes * 8,
      requestFields  = BundleField.accept(manager.requestFields, subordinate.requestKeys),
      responseFields = BundleField.accept(subordinate.responseFields, manager.responseKeys),
      lite  = subordinate.lite)
}

case class AHBEdgeParameters(
  manager: AHBManagerPortParameters,
  subordinate:  AHBSubordinatePortParameters,
  params: Parameters,
  sourceInfo: SourceInfo)
{
  val bundle = AHBBundleParameters(manager, subordinate)
}
