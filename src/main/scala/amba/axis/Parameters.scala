// See LICENSE.SiFive for license details.
package freechips.rocketchip.amba.axis

import chisel3.util._
import chisel3.experimental.SourceInfo
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util._
import freechips.rocketchip.diplomacy._

class AXISSlaveParameters private (
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
    new AXISSlaveParameters(
      name         = name,
      supportsSizes = supportsSizes,
      destinationId = destinationId,
      resources     = resources,
      nodePath      = nodePath)
  }
}

object AXISSlaveParameters {
  def v1(
      name:          String,
      supportsSizes: TransferSizes,
      destinationId: Int           = 0,
      resources:     Seq[Resource] = Nil,
      nodePath:      Seq[BaseNode] = Nil) =
  {
    new AXISSlaveParameters(
      name         = name,
      supportsSizes = supportsSizes,
      destinationId = destinationId,
      resources     = resources,
      nodePath      = nodePath)
  }
}

class AXISSlavePortParameters private (
  val slaves:        Seq[AXISSlaveParameters],
  val reqAligned:    Boolean, /* 'Position byte's are unsupported */
  val reqContinuous: Boolean, /* 'Null byte's inside transfers unsupported */
  val beatBytes:     Option[Int])
{
  require (!slaves.isEmpty)
  beatBytes.foreach { b => require(isPow2(b)) }

  val endDestinationId = slaves.map(_.destinationId).max + 1
  val supportsMinCover = TransferSizes.mincover(slaves.map(_.supportsSizes))

  def v1copy(
    slaves:        Seq[AXISSlaveParameters] = slaves,
    reqAligned:    Boolean                  = reqAligned,
    reqContinuous: Boolean                  = reqContinuous,
    beatBytes:     Option[Int]              = beatBytes) =
  {
    new AXISSlavePortParameters(
      slaves        = slaves,
      reqAligned    = reqAligned,
      reqContinuous = reqContinuous,
      beatBytes     = beatBytes)
  }
}

object AXISSlavePortParameters {
  def v1(
    slaves:        Seq[AXISSlaveParameters],
    reqAligned:    Boolean     = false,
    reqContinuous: Boolean     = false,
    beatBytes:     Option[Int] = None) =
  {
    new AXISSlavePortParameters(
      slaves        = slaves,
      reqAligned    = reqAligned,
      reqContinuous = reqContinuous,
      beatBytes     = beatBytes)
  }
}

class AXISMasterParameters private (
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
    new AXISMasterParameters(
      name       = name,
      emitsSizes = emitsSizes,
      sourceId   = sourceId,
      resources  = resources,
      nodePath   = nodePath)
  }
}

object AXISMasterParameters {
  def v1(
    name:       String,
    emitsSizes: TransferSizes,
    sourceId:   IdRange       = IdRange(0,1),
    resources:  Seq[Resource] = Nil,
    nodePath:   Seq[BaseNode] = Nil) =
  {
    new AXISMasterParameters(
      name       = name,
      emitsSizes = emitsSizes,
      sourceId   = sourceId,
      resources  = resources,
      nodePath   = nodePath)
  }
}

class AXISMasterPortParameters private (
  val masters:      Seq[AXISMasterParameters],
  val userFields:   Seq[BundleFieldBase],
  val isAligned:    Boolean, /* there are no 'Position byte's in transfers */
  val isContinuous: Boolean, /* there are no 'Null byte's except at the end of a transfer */
  val beatBytes:    Option[Int])
{
  require (!masters.isEmpty)
  beatBytes.foreach { b => require(isPow2(b)) }

  val endSourceId = masters.map(_.sourceId.end).max
  val emitsMinCover = TransferSizes.mincover(masters.map(_.emitsSizes))

  def v1copy(
    masters:      Seq[AXISMasterParameters] = masters,
    userFields:   Seq[BundleFieldBase]      = userFields,
    isAligned:    Boolean                   = isAligned,
    isContinuous: Boolean                   = isContinuous,
    beatBytes:    Option[Int]               = beatBytes) =
  {
    new AXISMasterPortParameters(
      masters      = masters,
      userFields   = userFields,
      isAligned    = isAligned,
      isContinuous = isContinuous,
      beatBytes    = beatBytes)
  }
}

object AXISMasterPortParameters {
  def v1(
    masters:      Seq[AXISMasterParameters],
    userFields:   Seq[BundleFieldBase] = Nil,
    isAligned:    Boolean              = false,
    isContinuous: Boolean              = false,
    beatBytes:    Option[Int]          = None) =
  {
    new AXISMasterPortParameters(
      masters      = masters,
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
  val master:     AXISMasterPortParameters,
  val slave:      AXISSlavePortParameters,
  val params:     Parameters,
  val sourceInfo: SourceInfo)
{
  require (!slave.beatBytes.isEmpty || !master.beatBytes.isEmpty,
    s"Neither master nor slave port specify a bus width (insert an AXISBusBinder between them?) at ${sourceInfo}")
  require (slave.beatBytes.isEmpty || master.beatBytes.isEmpty || slave.beatBytes == master.beatBytes,
    s"Master and slave ports specify incompatible bus widths (insert an AXISWidthWidget between them?) at ${sourceInfo}")
  require (!slave.reqAligned || master.isAligned, s"Slave port requires aligned stream data at ${sourceInfo}")
  require (!slave.reqContinuous || master.isContinuous, s"Slave port requires continuous stream data at ${sourceInfo}")

  val beatBytes = slave.beatBytes.getOrElse(master.beatBytes.get)
  val transferSizes = master.emitsMinCover intersect slave.supportsMinCover

  val bundle = AXISBundleParameters.v1(
    idBits      = log2Ceil(master.endSourceId),
    destBits    = log2Ceil(slave.endDestinationId),
    dataBits    = beatBytes * 8,
    userFields  = master.userFields,
    oneBeat     = transferSizes.max <= beatBytes,
    aligned     = master.isAligned)

  def v1copy(
    master:     AXISMasterPortParameters = master,
    slave:      AXISSlavePortParameters  = slave,
    params:     Parameters               = params,
    sourceInfo: SourceInfo               = sourceInfo) =
  {
    new AXISEdgeParameters(
      master     = master,
      slave      = slave,
      params     = params,
      sourceInfo = sourceInfo)
  }
}

object AXISEdgeParameters {
  def v1(
    master:     AXISMasterPortParameters,
    slave:      AXISSlavePortParameters,
    params:     Parameters,
    sourceInfo: SourceInfo) =
  {
    new AXISEdgeParameters(
      master     = master,
      slave      = slave,
      params     = params,
      sourceInfo = sourceInfo)
  }
}
