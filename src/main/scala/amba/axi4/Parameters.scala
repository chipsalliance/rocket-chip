// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import chisel3.experimental.SourceInfo
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import scala.math.max
import chisel3.util.{isPow2, log2Up}

/**
  * Parameters for AXI4 slave
  *
  * @param address base address
  * @param resources device tree resource
  * @param regionType memory region type
  * @param executable whether processor can execute from this memory
  */
case class AXI4SlaveParameters(
  address:       Seq[AddressSet],
  resources:     Seq[Resource] = Nil,
  regionType:    RegionType.T  = RegionType.GET_EFFECTS,
  executable:    Boolean       = false,
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
  minLatency: Int = 1,
  responseFields: Seq[BundleFieldBase] = Nil,
  requestKeys:    Seq[BundleKeyBase]   = Nil)
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
  masters:    Seq[AXI4MasterParameters],
  echoFields:    Seq[BundleFieldBase] = Nil,
  requestFields: Seq[BundleFieldBase] = Nil,
  responseKeys:  Seq[BundleKeyBase]   = Nil)
{
  val endId = masters.map(_.id.end).max

  // Require disjoint ranges for ids
  IdRange.overlaps(masters.map(_.id)).foreach { case (x, y) =>
    require (!x.overlaps(y), s"AXI4MasterParameters.id $x and $y overlap")
  }
}

case class AXI4BundleParameters(
  addrBits: Int,
  dataBits: Int,
  idBits:   Int,
  echoFields:     Seq[BundleFieldBase] = Nil,
  requestFields:  Seq[BundleFieldBase] = Nil,
  responseFields: Seq[BundleFieldBase] = Nil)
{
  require (dataBits >= 8, s"AXI4 data bits must be >= 8 (got $dataBits)")
  require (addrBits >= 1, s"AXI4 addr bits must be >= 1 (got $addrBits)")
  require (idBits >= 1, s"AXI4 id bits must be >= 1 (got $idBits)")
  require (isPow2(dataBits), s"AXI4 data bits must be pow2 (got $dataBits)")
  echoFields.foreach { f => require (f.key.isControl, s"${f} is not a legal echo field") }

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
      max(addrBits,   x.addrBits),
      max(dataBits,   x.dataBits),
      max(idBits,     x.idBits),
      BundleField.union(echoFields ++ x.echoFields),
      BundleField.union(requestFields ++ x.requestFields),
      BundleField.union(responseFields ++ x.responseFields))
}

object AXI4BundleParameters
{
  val emptyBundleParams = AXI4BundleParameters(addrBits=1, dataBits=8, idBits=1, echoFields=Nil, requestFields=Nil, responseFields=Nil)
  def union(x: Seq[AXI4BundleParameters]) = x.foldLeft(emptyBundleParams)((x,y) => x.union(y))

  def apply(master: AXI4MasterPortParameters, slave: AXI4SlavePortParameters) =
    new AXI4BundleParameters(
      addrBits = log2Up(slave.maxAddress+1),
      dataBits = slave.beatBytes * 8,
      idBits   = log2Up(master.endId),
      echoFields     = master.echoFields,
      requestFields  = BundleField.accept(master.requestFields, slave.requestKeys),
      responseFields = BundleField.accept(slave.responseFields, master.responseKeys))
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

case class AXI4CreditedDelay(
  aw: CreditedDelay,
  w:  CreditedDelay,
  b:  CreditedDelay,
  ar: CreditedDelay,
  r:  CreditedDelay)
{
  def + (that: AXI4CreditedDelay): AXI4CreditedDelay = AXI4CreditedDelay(
    aw = aw + that.aw,
    w  = w  + that.w,
    b  = b  + that.b,
    ar = ar + that.ar,
    r  = r  + that.r)

  override def toString = s"(${aw}, ${w}, ${b}, ${ar}, ${r})"
}

object AXI4CreditedDelay {
  def apply(delay: CreditedDelay): AXI4CreditedDelay = apply(delay, delay, delay.flip, delay, delay.flip)
}

case class AXI4CreditedSlavePortParameters(delay: AXI4CreditedDelay, base: AXI4SlavePortParameters)
case class AXI4CreditedMasterPortParameters(delay: AXI4CreditedDelay, base: AXI4MasterPortParameters)
case class AXI4CreditedEdgeParameters(master: AXI4CreditedMasterPortParameters, slave: AXI4CreditedSlavePortParameters, params: Parameters, sourceInfo: SourceInfo)
{
  val delay = master.delay + slave.delay
  val bundle = AXI4BundleParameters(master.base, slave.base)
}

/** Pretty printing of AXI4 source id maps */
class AXI4IdMap(axi4: AXI4MasterPortParameters) extends IdMap[AXI4IdMapEntry] {
  private val axi4Digits = String.valueOf(axi4.endId-1).length()
  protected val fmt = s"\t[%${axi4Digits}d, %${axi4Digits}d) %s%s%s"
  private val sorted = axi4.masters.sortBy(_.id)

  val mapping: Seq[AXI4IdMapEntry] = sorted.map { case c =>
    // to conservatively state max number of transactions, assume every id has up to c.maxFlight and reuses ids between AW and AR channels
    val maxTransactionsInFlight = c.maxFlight.map(_ * c.id.size * 2)
    AXI4IdMapEntry(c.id, c.name, maxTransactionsInFlight)
  }
}

case class AXI4IdMapEntry(axi4Id: IdRange, name: String, maxTransactionsInFlight: Option[Int] = None) extends IdMapEntry {
  val from = axi4Id
  val to = axi4Id
  val isCache = false
  val requestFifo = false
}
