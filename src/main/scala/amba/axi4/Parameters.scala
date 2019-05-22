// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.AsyncQueueParams
import scala.math.max

case class AXI4SlaveParameters(
  address:       Seq[AddressSet],
  resources:     Seq[Resource] = Nil,
  regionType:    RegionType.T  = RegionType.GET_EFFECTS,
  executable:    Boolean       = false, // processor can execute from this memory
  nodePath:      Seq[BaseNode] = Seq(),
  supportsWrite: TransferSizes = TransferSizes.none,
  supportsRead:  TransferSizes = TransferSizes.none,
  interleavedId: Option[Int]   = None,
  device: Option[Device] = None) // The device will not interleave responses (R+B)
{
  address.foreach { a => require (a.finite) }
  address.combinations(2).foreach { case Seq(x,y) => require (!x.overlaps(y), s"$x and $y overlap") }

  val name = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")
  val maxTransfer = max(supportsWrite.max, supportsRead.max)
  val maxAddress = address.map(_.max).max
  val minAlignment = address.map(_.alignment).min

  // The device had better not support a transfer larger than its alignment
  require (minAlignment >= maxTransfer,
    s"minAlignment ($minAlignment) must be >= maxTransfer ($maxTransfer)")

  def toResource: ResourceAddress = {
    ResourceAddress(address, ResourcePermissions(
      r = supportsRead,
      w = supportsWrite,
      x = executable,
      c = false,
      a = false))
  }
}

case class AXI4SlavePortParameters(
  slaves:     Seq[AXI4SlaveParameters],
  beatBytes:  Int,
  wcorrupt:   Boolean = false,
  minLatency: Int = 1)
{
  require (!slaves.isEmpty)
  require (isPow2(beatBytes))

  val maxTransfer = slaves.map(_.maxTransfer).max
  val maxAddress = slaves.map(_.maxAddress).max

  // Check the link is not pointlessly wide
  require (maxTransfer >= beatBytes,
    s"maxTransfer ($maxTransfer) should not be smaller than bus width ($beatBytes)")
  // Check that the link can be implemented in AXI4
  val limit = beatBytes * (1 << AXI4Parameters.lenBits)
  require (maxTransfer <= limit,
    s"maxTransfer ($maxTransfer) cannot be larger than $limit on a $beatBytes*8 width bus")

  // Require disjoint ranges for addresses
  slaves.combinations(2).foreach { case Seq(x,y) =>
    x.address.foreach { a => y.address.foreach { b =>
      require (!a.overlaps(b), s"$a and $b overlap")
    } }
  }
}

case class AXI4MasterParameters(
  name:      String,
  id:        IdRange       = IdRange(0, 1),
  aligned:   Boolean       = false,
  maxFlight: Option[Int]   = None, // None = infinite, else is a per-ID cap
  nodePath:  Seq[BaseNode] = Seq())
{
  maxFlight.foreach { m => require (m >= 0) }
}

case class AXI4MasterPortParameters(
  masters:   Seq[AXI4MasterParameters],
  userBits:  Int = 0)
{
  val endId = masters.map(_.id.end).max
  require (userBits >= 0)

  // Require disjoint ranges for ids
  IdRange.overlaps(masters.map(_.id)).foreach { case (x, y) =>
    require (!x.overlaps(y), s"AXI4MasterParameters.id $x and $y overlap")
  }
}

case class AXI4BundleParameters(
  addrBits: Int,
  dataBits: Int,
  idBits:   Int,
  userBits: Int = 0,
  wcorrupt: Boolean = false)
{
  require (dataBits >= 8, s"AXI4 data bits must be >= 8 (got $dataBits)")
  require (addrBits >= 1, s"AXI4 addr bits must be >= 1 (got $addrBits)")
  require (idBits >= 1, s"AXI4 id bits must be >= 1 (got $idBits)")
  require (isPow2(dataBits), s"AXI4 data bits must be pow2 (got $dataBits)")

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
      max(idBits,   x.idBits),
      max(userBits, x.userBits),
      wcorrupt || x.wcorrupt)
}

object AXI4BundleParameters
{
  val emptyBundleParams = AXI4BundleParameters(addrBits=1, dataBits=8, idBits=1, userBits=0, wcorrupt=false)
  def union(x: Seq[AXI4BundleParameters]) = x.foldLeft(emptyBundleParams)((x,y) => x.union(y))

  def apply(master: AXI4MasterPortParameters, slave: AXI4SlavePortParameters) =
    new AXI4BundleParameters(
      addrBits = log2Up(slave.maxAddress+1),
      dataBits = slave.beatBytes * 8,
      idBits   = log2Up(master.endId),
      userBits = master.userBits,
      wcorrupt = slave.wcorrupt)
}

case class AXI4EdgeParameters(
  master: AXI4MasterPortParameters,
  slave:  AXI4SlavePortParameters,
  params: Parameters,
  sourceInfo: SourceInfo)
{
  val bundle = AXI4BundleParameters(master, slave)
}

case class AXI4AsyncSlavePortParameters(async: AsyncQueueParams, base: AXI4SlavePortParameters)
case class AXI4AsyncMasterPortParameters(base: AXI4MasterPortParameters)

case class AXI4AsyncBundleParameters(async: AsyncQueueParams, base: AXI4BundleParameters)
case class AXI4AsyncEdgeParameters(master: AXI4AsyncMasterPortParameters, slave: AXI4AsyncSlavePortParameters, params: Parameters, sourceInfo: SourceInfo)
{
  val bundle = AXI4AsyncBundleParameters(slave.async, AXI4BundleParameters(master.base, slave.base))
}

case class AXI4BufferParams(
  aw: BufferParams = BufferParams.none,
  w:  BufferParams = BufferParams.none,
  b:  BufferParams = BufferParams.none,
  ar: BufferParams = BufferParams.none,
  r:  BufferParams = BufferParams.none
) extends DirectedBuffers[AXI4BufferParams] {
  def copyIn(x: BufferParams) = this.copy(b = x, r = x)
  def copyOut(x: BufferParams) = this.copy(aw = x, ar = x, w = x)
  def copyInOut(x: BufferParams) = this.copyIn(x).copyOut(x)
}
