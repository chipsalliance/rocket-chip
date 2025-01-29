// See LICENSE.SiFive for license details.
package freechips.rocketchip.amba.axis

import chisel3.experimental.SourceInfo
import chisel3.util.{isPow2, log2Ceil}

import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.nodes.BaseNode

import freechips.rocketchip.diplomacy.{TransferSizes, IdRange}
import freechips.rocketchip.resources.{Resource}
import freechips.rocketchip.util.{BundleFieldBase, BundleField}


class AXISSubordinateParameters private (
  val name:          String,
  val supportsSizes: TransferSizes,
  val destinationId: Int,
  val resources:     Seq[Resource],
  val nodePath:      Seq[BaseNode])
{
  require (!supportsSizes.none)
  require (destinationId >= 0)

  def v1copy(
      name:          String        = name,
      supportsSizes: TransferSizes = supportsSizes,
      destinationId: Int           = destinationId,
      resources:     Seq[Resource] = resources,
      nodePath:      Seq[BaseNode] = nodePath) =
  {
    new AXISSubordinateParameters(
      name         = name,
      supportsSizes = supportsSizes,
      destinationId = destinationId,
      resources     = resources,
      nodePath      = nodePath)
  }
}

object AXISSubordinateParameters {
  def v1(
      name:          String,
      supportsSizes: TransferSizes,
      destinationId: Int           = 0,
      resources:     Seq[Resource] = Nil,
      nodePath:      Seq[BaseNode] = Nil) =
  {
    new AXISSubordinateParameters(
      name         = name,
      supportsSizes = supportsSizes,
      destinationId = destinationId,
      resources     = resources,
      nodePath      = nodePath)
  }
}

class AXISSubordinatePortParameters private (
  val subordinates:        Seq[AXISSubordinateParameters],
  val reqAligned:    Boolean, /* 'Position byte's are unsupported */
  val reqContinuous: Boolean, /* 'Null byte's inside transfers unsupported */
  val beatBytes:     Option[Int])
{
  require (!subordinates.isEmpty)
  beatBytes.foreach { b => require(isPow2(b)) }

  val endDestinationId = subordinates.map(_.destinationId).max + 1
  val supportsMinCover = TransferSizes.mincover(subordinates.map(_.supportsSizes))

  def v1copy(
    subordinates:        Seq[AXISSubordinateParameters] = subordinates,
    reqAligned:    Boolean                  = reqAligned,
    reqContinuous: Boolean                  = reqContinuous,
    beatBytes:     Option[Int]              = beatBytes) =
  {
    new AXISSubordinatePortParameters(
      subordinates        = subordinates,
      reqAligned    = reqAligned,
      reqContinuous = reqContinuous,
      beatBytes     = beatBytes)
  }
}

object AXISSubordinatePortParameters {
  def v1(
    subordinates:        Seq[AXISSubordinateParameters],
    reqAligned:    Boolean     = false,
    reqContinuous: Boolean     = false,
    beatBytes:     Option[Int] = None) =
  {
    new AXISSubordinatePortParameters(
      subordinates        = subordinates,
      reqAligned    = reqAligned,
      reqContinuous = reqContinuous,
      beatBytes     = beatBytes)
  }
}

class AXISManagerParameters private (
  val name:       String,
  val emitsSizes: TransferSizes,
  val sourceId:   IdRange,
  val resources:  Seq[Resource],
  val nodePath:   Seq[BaseNode])
{
  require (!emitsSizes.none)
  require (!sourceId.isEmpty)

  def v1copy(
    name:       String        = name,
    emitsSizes: TransferSizes = emitsSizes,
    sourceId:   IdRange       = sourceId,
    resources:  Seq[Resource] = resources,
    nodePath:   Seq[BaseNode] = nodePath) =
  {
    new AXISManagerParameters(
      name       = name,
      emitsSizes = emitsSizes,
      sourceId   = sourceId,
      resources  = resources,
      nodePath   = nodePath)
  }
}

object AXISManagerParameters {
  def v1(
    name:       String,
    emitsSizes: TransferSizes,
    sourceId:   IdRange       = IdRange(0,1),
    resources:  Seq[Resource] = Nil,
    nodePath:   Seq[BaseNode] = Nil) =
  {
    new AXISManagerParameters(
      name       = name,
      emitsSizes = emitsSizes,
      sourceId   = sourceId,
      resources  = resources,
      nodePath   = nodePath)
  }
}

class AXISManagerPortParameters private (
  val managers:      Seq[AXISManagerParameters],
  val userFields:   Seq[BundleFieldBase],
  val isAligned:    Boolean, /* there are no 'Position byte's in transfers */
  val isContinuous: Boolean, /* there are no 'Null byte's except at the end of a transfer */
  val beatBytes:    Option[Int])
{
  require (!managers.isEmpty)
  beatBytes.foreach { b => require(isPow2(b)) }

  val endSourceId = managers.map(_.sourceId.end).max
  val emitsMinCover = TransferSizes.mincover(managers.map(_.emitsSizes))

  def v1copy(
    managers:      Seq[AXISManagerParameters] = managers,
    userFields:   Seq[BundleFieldBase]      = userFields,
    isAligned:    Boolean                   = isAligned,
    isContinuous: Boolean                   = isContinuous,
    beatBytes:    Option[Int]               = beatBytes) =
  {
    new AXISManagerPortParameters(
      managers      = managers,
      userFields   = userFields,
      isAligned    = isAligned,
      isContinuous = isContinuous,
      beatBytes    = beatBytes)
  }
}

object AXISManagerPortParameters {
  def v1(
    managers:      Seq[AXISManagerParameters],
    userFields:   Seq[BundleFieldBase] = Nil,
    isAligned:    Boolean              = false,
    isContinuous: Boolean              = false,
    beatBytes:    Option[Int]          = None) =
  {
    new AXISManagerPortParameters(
      managers      = managers,
      userFields   = userFields,
      isAligned    = isAligned,
      isContinuous = isContinuous,
      beatBytes    = beatBytes)
  }
}

class AXISBundleParameters private (
  val idBits:      Int,
  val destBits:    Int,
  val dataBits:    Int,
  val userFields:  Seq[BundleFieldBase],
  val oneBeat:     Boolean,
  val aligned:     Boolean)
{
  require (idBits >= 0)
  require (destBits >= 0)
  require (dataBits >= 8)
  require (isPow2(dataBits))

  val keepBits = dataBits/8
  val strbBits = dataBits/8

  def hasLast = !oneBeat
  def hasId   = idBits > 0
  def hasDest = destBits > 0
  def hasKeep = true
  def hasStrb = !aligned
  def hasData = true

  def union(x: AXISBundleParameters) = new AXISBundleParameters(
    idBits      = idBits   max x.idBits,
    destBits    = destBits max x.destBits,
    dataBits    = dataBits max x.dataBits,
    userFields  = BundleField.union(userFields ++ x.userFields),
    oneBeat     = oneBeat && x.oneBeat,
    aligned     = aligned && x.aligned)

  def v1copy(
    idBits:      Int = idBits,
    destBits:    Int = destBits,
    dataBits:    Int = dataBits,
    userFields:  Seq[BundleFieldBase] = userFields,
    oneBeat:     Boolean = oneBeat,
    aligned:     Boolean = aligned) =
  {
    new AXISBundleParameters(
      idBits     = idBits,
      destBits   = destBits,
      dataBits   = dataBits,
      userFields = userFields,
      oneBeat    = oneBeat,
      aligned    = aligned)
  }
}

object AXISBundleParameters {
  val emptyBundleParams = new AXISBundleParameters(
    idBits      = 0,
    destBits    = 0,
    dataBits    = 8,
    userFields  = Nil,
    oneBeat     = true,
    aligned     = true)
  def union(x: Seq[AXISBundleParameters]) = x.foldLeft(emptyBundleParams)((x,y) => x.union(y))

  def v1(
    idBits:      Int,
    destBits:    Int,
    dataBits:    Int,
    userFields:  Seq[BundleFieldBase],
    oneBeat:     Boolean,
    aligned:     Boolean) =
  {
    new AXISBundleParameters(
      idBits     = idBits,
      destBits   = destBits,
      dataBits   = dataBits,
      userFields = userFields,
      oneBeat    = oneBeat,
      aligned    = aligned)
  }
}

class AXISEdgeParameters private (
  val manager:     AXISManagerPortParameters,
  val subordinate:      AXISSubordinatePortParameters,
  val params:     Parameters,
  val sourceInfo: SourceInfo)
{
  require (!subordinate.beatBytes.isEmpty || !manager.beatBytes.isEmpty,
    s"Neither manager nor subordinate port specify a bus width (insert an AXISBusBinder between them?) at ${sourceInfo}")
  require (subordinate.beatBytes.isEmpty || manager.beatBytes.isEmpty || subordinate.beatBytes == manager.beatBytes,
    s"Manager and subordinate ports specify incompatible bus widths (insert an AXISWidthWidget between them?) at ${sourceInfo}")
  require (!subordinate.reqAligned || manager.isAligned, s"Subordinate port requires aligned stream data at ${sourceInfo}")
  require (!subordinate.reqContinuous || manager.isContinuous, s"Subordinate port requires continuous stream data at ${sourceInfo}")

  val beatBytes = subordinate.beatBytes.getOrElse(manager.beatBytes.get)
  val transferSizes = manager.emitsMinCover intersect subordinate.supportsMinCover

  val bundle = AXISBundleParameters.v1(
    idBits      = log2Ceil(manager.endSourceId),
    destBits    = log2Ceil(subordinate.endDestinationId),
    dataBits    = beatBytes * 8,
    userFields  = manager.userFields,
    oneBeat     = transferSizes.max <= beatBytes,
    aligned     = manager.isAligned)

  def v1copy(
    manager:     AXISManagerPortParameters = manager,
    subordinate:      AXISSubordinatePortParameters  = subordinate,
    params:     Parameters               = params,
    sourceInfo: SourceInfo               = sourceInfo) =
  {
    new AXISEdgeParameters(
      manager     = manager,
      subordinate      = subordinate,
      params     = params,
      sourceInfo = sourceInfo)
  }
}

object AXISEdgeParameters {
  def v1(
    manager:     AXISManagerPortParameters,
    subordinate:      AXISSubordinatePortParameters,
    params:     Parameters,
    sourceInfo: SourceInfo) =
  {
    new AXISEdgeParameters(
      manager     = manager,
      subordinate      = subordinate,
      params     = params,
      sourceInfo = sourceInfo)
  }
}
