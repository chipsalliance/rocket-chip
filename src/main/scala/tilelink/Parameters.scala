// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import scala.math.max

//These transfer sizes describe requests issued from masters on the A channel that will be responded by slaves on the D channel
case class TLMasterToSlaveTransferSizes(
  // Supports both Acquire+Release of the following two sizes:
  acquireT:   TransferSizes = TransferSizes.none,
  acquireB:   TransferSizes = TransferSizes.none,
  arithmetic: TransferSizes = TransferSizes.none,
  logical:    TransferSizes = TransferSizes.none,
  get:        TransferSizes = TransferSizes.none,
  putFull:    TransferSizes = TransferSizes.none,
  putPartial: TransferSizes = TransferSizes.none,
  hint:       TransferSizes = TransferSizes.none)
  extends TLCommonTransferSizes {
  def intersect(rhs: TLMasterToSlaveTransferSizes) = TLMasterToSlaveTransferSizes(
    acquireT   = acquireT  .intersect(rhs.acquireT),
    acquireB   = acquireB  .intersect(rhs.acquireB),
    arithmetic = arithmetic.intersect(rhs.arithmetic),
    logical    = logical   .intersect(rhs.logical),
    get        = get       .intersect(rhs.get),
    putFull    = putFull   .intersect(rhs.putFull),
    putPartial = putPartial.intersect(rhs.putPartial),
    hint       = hint      .intersect(rhs.hint))
  def mincover(rhs: TLMasterToSlaveTransferSizes) = TLMasterToSlaveTransferSizes(
    acquireT   = acquireT  .mincover(rhs.acquireT),
    acquireB   = acquireB  .mincover(rhs.acquireB),
    arithmetic = arithmetic.mincover(rhs.arithmetic),
    logical    = logical   .mincover(rhs.logical),
    get        = get       .mincover(rhs.get),
    putFull    = putFull   .mincover(rhs.putFull),
    putPartial = putPartial.mincover(rhs.putPartial),
    hint       = hint      .mincover(rhs.hint))
  // Reduce rendering to a simple yes/no per field
  override def toString = {
    def str(x: TransferSizes, flag: String) = if (x.none) "" else flag
    def flags = Vector(
      str(acquireT,   "T"),
      str(acquireB,   "B"),
      str(arithmetic, "A"),
      str(logical,    "L"),
      str(get,        "G"),
      str(putFull,    "F"),
      str(putPartial, "P"),
      str(hint,       "H"))
    flags.mkString
  }
  // Prints out the actual information in a user readable way
  def infoString = {
    s"""acquireT = ${acquireT}
       |acquireB = ${acquireB}
       |arithmetic = ${arithmetic}
       |logical = ${logical}
       |get = ${get}
       |putFull = ${putFull}
       |putPartial = ${putPartial}
       |hint = ${hint}
       |
       |""".stripMargin
  }
}

object TLMasterToSlaveTransferSizes {
  def unknownEmits = TLMasterToSlaveTransferSizes(
    acquireT   = TransferSizes(1, 4096),
    acquireB   = TransferSizes(1, 4096),
    arithmetic = TransferSizes(1, 4096),
    logical    = TransferSizes(1, 4096),
    get        = TransferSizes(1, 4096),
    putFull    = TransferSizes(1, 4096),
    putPartial = TransferSizes(1, 4096),
    hint       = TransferSizes(1, 4096))
  def unknownSupports = TLMasterToSlaveTransferSizes()
}

//These transfer sizes describe requests issued from slaves on the B channel that will be responded by masters on the C channel
case class TLSlaveToMasterTransferSizes(
  probe:      TransferSizes = TransferSizes.none,
  arithmetic: TransferSizes = TransferSizes.none,
  logical:    TransferSizes = TransferSizes.none,
  get:        TransferSizes = TransferSizes.none,
  putFull:    TransferSizes = TransferSizes.none,
  putPartial: TransferSizes = TransferSizes.none,
  hint:       TransferSizes = TransferSizes.none
) extends TLCommonTransferSizes {
  def intersect(rhs: TLSlaveToMasterTransferSizes) = TLSlaveToMasterTransferSizes(
    probe      = probe     .intersect(rhs.probe),
    arithmetic = arithmetic.intersect(rhs.arithmetic),
    logical    = logical   .intersect(rhs.logical),
    get        = get       .intersect(rhs.get),
    putFull    = putFull   .intersect(rhs.putFull),
    putPartial = putPartial.intersect(rhs.putPartial),
    hint       = hint      .intersect(rhs.hint)
  )
  def mincover(rhs: TLSlaveToMasterTransferSizes) = TLSlaveToMasterTransferSizes(
    probe      = probe     .mincover(rhs.probe),
    arithmetic = arithmetic.mincover(rhs.arithmetic),
    logical    = logical   .mincover(rhs.logical),
    get        = get       .mincover(rhs.get),
    putFull    = putFull   .mincover(rhs.putFull),
    putPartial = putPartial.mincover(rhs.putPartial),
    hint       = hint      .mincover(rhs.hint)
  )
  // Reduce rendering to a simple yes/no per field
  override def toString = {
    def str(x: TransferSizes, flag: String) = if (x.none) "" else flag
    def flags = Vector(
      str(probe,      "P"),
      str(arithmetic, "A"),
      str(logical,    "L"),
      str(get,        "G"),
      str(putFull,    "F"),
      str(putPartial, "P"),
      str(hint,       "H"))
    flags.mkString
  }
  // Prints out the actual information in a user readable way
  def infoString = {
    s"""probe = ${probe}
       |arithmetic = ${arithmetic}
       |logical = ${logical}
       |get = ${get}
       |putFull = ${putFull}
       |putPartial = ${putPartial}
       |hint = ${hint}
       |
       |""".stripMargin
  }
}

object TLSlaveToMasterTransferSizes {
  def unknownEmits = TLSlaveToMasterTransferSizes(
    arithmetic = TransferSizes(1, 4096),
    logical    = TransferSizes(1, 4096),
    get        = TransferSizes(1, 4096),
    putFull    = TransferSizes(1, 4096),
    putPartial = TransferSizes(1, 4096),
    hint       = TransferSizes(1, 4096),
    probe      = TransferSizes(1, 4096))
  def unknownSupports = TLSlaveToMasterTransferSizes()
}

trait TLCommonTransferSizes {
  def arithmetic: TransferSizes
  def logical:    TransferSizes
  def get:        TransferSizes
  def putFull:    TransferSizes
  def putPartial: TransferSizes
  def hint:       TransferSizes
}

class TLSlaveParameters private(
  val nodePath:           Seq[BaseNode],
  val resources:          Seq[Resource],
  setName:                Option[String],
  val address:            Seq[AddressSet],
  val regionType:         RegionType.T,
  val executable:         Boolean,
  val fifoId:             Option[Int],
  val supports:           TLMasterToSlaveTransferSizes,
  val emits:              TLSlaveToMasterTransferSizes,
  // By default, slaves are forbidden from issuing 'denied' responses (it prevents Fragmentation)
  val alwaysGrantsT:      Boolean, // typically only true for CacheCork'd read-write devices; dual: neverReleaseData
  // If fifoId=Some, all accesses sent to the same fifoId are executed and ACK'd in FIFO order
  // Note: you can only rely on this FIFO behaviour if your TLMasterParameters include requestFifo
  val mayDenyGet:         Boolean, // applies to: AccessAckData, GrantData
  val mayDenyPut:         Boolean) // applies to: AccessAck,     Grant,    HintAck
                                   // ReleaseAck may NEVER be denied
  extends SimpleProduct
{
  override def canEqual(that: Any): Boolean = that.isInstanceOf[TLSlaveParameters]
  override def productPrefix = "TLSlaveParameters"
  // We intentionally omit nodePath for equality testing / formatting
  def productArity: Int = 11
  def productElement(n: Int): Any = n match {
    case 0 => name
    case 1 => address
    case 2 => resources
    case 3 => regionType
    case 4 => executable
    case 5 => fifoId
    case 6 => supports
    case 7 => emits
    case 8 => alwaysGrantsT
    case 9 => mayDenyGet
    case 10 => mayDenyPut
    case _ => throw new IndexOutOfBoundsException(n.toString)
  }

  def supportsAcquireT:   TransferSizes = supports.acquireT
  def supportsAcquireB:   TransferSizes = supports.acquireB
  def supportsArithmetic: TransferSizes = supports.arithmetic
  def supportsLogical:    TransferSizes = supports.logical
  def supportsGet:        TransferSizes = supports.get
  def supportsPutFull:    TransferSizes = supports.putFull
  def supportsPutPartial: TransferSizes = supports.putPartial
  def supportsHint:       TransferSizes = supports.hint

  require (!address.isEmpty, "Address cannot be empty")
  address.foreach { a => require (a.finite, "Address must be finite") }

  address.combinations(2).foreach { case Seq(x,y) => require (!x.overlaps(y), s"$x and $y overlap.") }
  require (supportsPutFull.contains(supportsPutPartial), s"PutFull($supportsPutFull) < PutPartial($supportsPutPartial)")
  require (supportsPutFull.contains(supportsArithmetic), s"PutFull($supportsPutFull) < Arithmetic($supportsArithmetic)")
  require (supportsPutFull.contains(supportsLogical),    s"PutFull($supportsPutFull) < Logical($supportsLogical)")
  require (supportsGet.contains(supportsArithmetic),     s"Get($supportsGet) < Arithmetic($supportsArithmetic)")
  require (supportsGet.contains(supportsLogical),        s"Get($supportsGet) < Logical($supportsLogical)")
  require (supportsAcquireB.contains(supportsAcquireT),  s"AcquireB($supportsAcquireB) < AcquireT($supportsAcquireT)")
  require (!alwaysGrantsT || supportsAcquireT, s"Must supportAcquireT if promising to always grantT")

  // Make sure that the regionType agrees with the capabilities
  require (!supportsAcquireB || regionType >= RegionType.UNCACHED) // acquire -> uncached, tracked, cached
  require (regionType <= RegionType.UNCACHED || supportsAcquireB)  // tracked, cached -> acquire
  require (regionType != RegionType.UNCACHED || supportsGet) // uncached -> supportsGet

  val name = setName.orElse(nodePath.lastOption.map(_.lazyModule.name)).getOrElse("disconnected")
  val maxTransfer = List( // Largest supported transfer of all types
    supportsAcquireT.max,
    supportsAcquireB.max,
    supportsArithmetic.max,
    supportsLogical.max,
    supportsGet.max,
    supportsPutFull.max,
    supportsPutPartial.max).max
  val maxAddress = address.map(_.max).max
  val minAlignment = address.map(_.alignment).min

  // The device had better not support a transfer larger than its alignment
  require (minAlignment >= maxTransfer, s"Bad $address: minAlignment ($minAlignment) must be >= maxTransfer ($maxTransfer)")

  def toResource: ResourceAddress = {
    ResourceAddress(address, ResourcePermissions(
      r = supportsAcquireB || supportsGet,
      w = supportsAcquireT || supportsPutFull,
      x = executable,
      c = supportsAcquireB,
      a = supportsArithmetic && supportsLogical))
  }

  def findTreeViolation() = nodePath.find {
    case _: MixedAdapterNode[_, _, _, _, _, _, _, _] => false
    case _: SinkNode[_, _, _, _, _] => false
    case node => node.inputs.size != 1
  }
  def isTree = findTreeViolation() == None

  def infoString = {
    s"""Slave Name = ${name}
       |Slave Address = ${address}
       |supports = ${supports.infoString}
       |
       |""".stripMargin
  }

  def v1copy(
    address:            Seq[AddressSet] = address,
    resources:          Seq[Resource]   = resources,
    regionType:         RegionType.T    = regionType,
    executable:         Boolean         = executable,
    nodePath:           Seq[BaseNode]   = nodePath,
    supportsAcquireT:   TransferSizes   = supports.acquireT,
    supportsAcquireB:   TransferSizes   = supports.acquireB,
    supportsArithmetic: TransferSizes   = supports.arithmetic,
    supportsLogical:    TransferSizes   = supports.logical,
    supportsGet:        TransferSizes   = supports.get,
    supportsPutFull:    TransferSizes   = supports.putFull,
    supportsPutPartial: TransferSizes   = supports.putPartial,
    supportsHint:       TransferSizes   = supports.hint,
    mayDenyGet:         Boolean         = mayDenyGet,
    mayDenyPut:         Boolean         = mayDenyPut,
    alwaysGrantsT:      Boolean         = alwaysGrantsT,
    fifoId:             Option[Int]     = fifoId) =
  {
    new TLSlaveParameters(
      setName       = setName,
      address       = address,
      resources     = resources,
      regionType    = regionType,
      executable    = executable,
      nodePath      = nodePath,
      supports      = TLMasterToSlaveTransferSizes(
        acquireT      = supportsAcquireT,
        acquireB      = supportsAcquireB,
        arithmetic    = supportsArithmetic,
        logical       = supportsLogical,
        get           = supportsGet,
        putFull       = supportsPutFull,
        putPartial    = supportsPutPartial,
        hint          = supportsHint),
      emits         = emits,
      mayDenyGet    = mayDenyGet,
      mayDenyPut    = mayDenyPut,
      alwaysGrantsT = alwaysGrantsT,
      fifoId        = fifoId)
  }

  def v2copy(
    nodePath:      Seq[BaseNode]                = nodePath,
    resources:     Seq[Resource]                = resources,
    name:          Option[String]               = setName,
    address:       Seq[AddressSet]              = address,
    regionType:    RegionType.T                 = regionType,
    executable:    Boolean                      = executable,
    fifoId:        Option[Int]                  = fifoId,
    supports:      TLMasterToSlaveTransferSizes = supports,
    emits:         TLSlaveToMasterTransferSizes = emits,
    alwaysGrantsT: Boolean                      = alwaysGrantsT,
    mayDenyGet:    Boolean                      = mayDenyGet,
    mayDenyPut:    Boolean                      = mayDenyPut) =
  {
    new TLSlaveParameters(
      nodePath      = nodePath,
      resources     = resources,
      setName       = name,
      address       = address,
      regionType    = regionType,
      executable    = executable,
      fifoId        = fifoId,
      supports      = supports,
      emits         = emits,
      alwaysGrantsT = alwaysGrantsT,
      mayDenyGet    = mayDenyGet,
      mayDenyPut    = mayDenyPut)
  }

  @deprecated("Use v1copy instead of copy","")
  def copy(
    address:            Seq[AddressSet] = address,
    resources:          Seq[Resource]   = resources,
    regionType:         RegionType.T    = regionType,
    executable:         Boolean         = executable,
    nodePath:           Seq[BaseNode]   = nodePath,
    supportsAcquireT:   TransferSizes   = supports.acquireT,
    supportsAcquireB:   TransferSizes   = supports.acquireB,
    supportsArithmetic: TransferSizes   = supports.arithmetic,
    supportsLogical:    TransferSizes   = supports.logical,
    supportsGet:        TransferSizes   = supports.get,
    supportsPutFull:    TransferSizes   = supports.putFull,
    supportsPutPartial: TransferSizes   = supports.putPartial,
    supportsHint:       TransferSizes   = supports.hint,
    mayDenyGet:         Boolean         = mayDenyGet,
    mayDenyPut:         Boolean         = mayDenyPut,
    alwaysGrantsT:      Boolean         = alwaysGrantsT,
    fifoId:             Option[Int]     = fifoId) =
  {
    v1copy(
      address            = address,
      resources          = resources,
      regionType         = regionType,
      executable         = executable,
      nodePath           = nodePath,
      supportsAcquireT   = supportsAcquireT,
      supportsAcquireB   = supportsAcquireB,
      supportsArithmetic = supportsArithmetic,
      supportsLogical    = supportsLogical,
      supportsGet        = supportsGet,
      supportsPutFull    = supportsPutFull,
      supportsPutPartial = supportsPutPartial,
      supportsHint       = supportsHint,
      mayDenyGet         = mayDenyGet,
      mayDenyPut         = mayDenyPut,
      alwaysGrantsT      = alwaysGrantsT,
      fifoId             = fifoId)
  }
}

object TLSlaveParameters {
  def v1(
    address:            Seq[AddressSet],
    resources:          Seq[Resource] = Seq(),
    regionType:         RegionType.T  = RegionType.GET_EFFECTS,
    executable:         Boolean       = false,
    nodePath:           Seq[BaseNode] = Seq(),
    supportsAcquireT:   TransferSizes = TransferSizes.none,
    supportsAcquireB:   TransferSizes = TransferSizes.none,
    supportsArithmetic: TransferSizes = TransferSizes.none,
    supportsLogical:    TransferSizes = TransferSizes.none,
    supportsGet:        TransferSizes = TransferSizes.none,
    supportsPutFull:    TransferSizes = TransferSizes.none,
    supportsPutPartial: TransferSizes = TransferSizes.none,
    supportsHint:       TransferSizes = TransferSizes.none,
    mayDenyGet:         Boolean = false,
    mayDenyPut:         Boolean = false,
    alwaysGrantsT:      Boolean = false,
    fifoId:             Option[Int] = None) =
  {
    new TLSlaveParameters(
      setName       = None,
      address       = address,
      resources     = resources,
      regionType    = regionType,
      executable    = executable,
      nodePath      = nodePath,
      supports      = TLMasterToSlaveTransferSizes(
        acquireT      = supportsAcquireT,
        acquireB      = supportsAcquireB,
        arithmetic    = supportsArithmetic,
        logical       = supportsLogical,
        get           = supportsGet,
        putFull       = supportsPutFull,
        putPartial    = supportsPutPartial,
        hint          = supportsHint),
      emits         = TLSlaveToMasterTransferSizes.unknownEmits,
      mayDenyGet    = mayDenyGet,
      mayDenyPut    = mayDenyPut,
      alwaysGrantsT = alwaysGrantsT,
      fifoId        = fifoId)
  }

  def v2(
    address:       Seq[AddressSet],
    nodePath:      Seq[BaseNode]                = Seq(),
    resources:     Seq[Resource]                = Seq(),
    name:          Option[String]               = None,
    regionType:    RegionType.T                 = RegionType.GET_EFFECTS,
    executable:    Boolean                      = false,
    fifoId:        Option[Int]                  = None,
    supports:      TLMasterToSlaveTransferSizes = TLMasterToSlaveTransferSizes.unknownSupports,
    emits:         TLSlaveToMasterTransferSizes = TLSlaveToMasterTransferSizes.unknownEmits,
    alwaysGrantsT: Boolean                      = false,
    mayDenyGet:    Boolean                      = false,
    mayDenyPut:    Boolean                      = false) =
  {
    new TLSlaveParameters(
    nodePath      = nodePath,
    resources     = resources,
    setName       = name,
    address       = address,
    regionType    = regionType,
    executable    = executable,
    fifoId        = fifoId,
    supports      = supports,
    emits         = emits,
    alwaysGrantsT = alwaysGrantsT,
    mayDenyGet    = mayDenyGet,
    mayDenyPut    = mayDenyPut)
  }
}

object TLManagerParameters {
  @deprecated("Use TLSlaveParameters.v1 instead of TLManagerParameters","")
  def apply(
    address:            Seq[AddressSet],
    resources:          Seq[Resource] = Seq(),
    regionType:         RegionType.T  = RegionType.GET_EFFECTS,
    executable:         Boolean       = false,
    nodePath:           Seq[BaseNode] = Seq(),
    supportsAcquireT:   TransferSizes = TransferSizes.none,
    supportsAcquireB:   TransferSizes = TransferSizes.none,
    supportsArithmetic: TransferSizes = TransferSizes.none,
    supportsLogical:    TransferSizes = TransferSizes.none,
    supportsGet:        TransferSizes = TransferSizes.none,
    supportsPutFull:    TransferSizes = TransferSizes.none,
    supportsPutPartial: TransferSizes = TransferSizes.none,
    supportsHint:       TransferSizes = TransferSizes.none,
    mayDenyGet:         Boolean = false,
    mayDenyPut:         Boolean = false,
    alwaysGrantsT:      Boolean = false,
    fifoId:             Option[Int] = None) =
    TLSlaveParameters.v1(
      address,
      resources,
      regionType,
      executable,
      nodePath,
      supportsAcquireT,
      supportsAcquireB,
      supportsArithmetic,
      supportsLogical,
      supportsGet,
      supportsPutFull,
      supportsPutPartial,
      supportsHint,
      mayDenyGet,
      mayDenyPut,
      alwaysGrantsT,
      fifoId,
    )
}

case class TLChannelBeatBytes(a: Option[Int], b: Option[Int], c: Option[Int], d: Option[Int])
{
  def members = Seq(a, b, c, d)
  members.collect { case Some(beatBytes) =>
    require (isPow2(beatBytes), "Data channel width must be a power of 2")
  }
}

object TLChannelBeatBytes{
  def apply(beatBytes: Int): TLChannelBeatBytes = TLChannelBeatBytes(
    Some(beatBytes),
    Some(beatBytes),
    Some(beatBytes),
    Some(beatBytes))
  def apply(): TLChannelBeatBytes = TLChannelBeatBytes(
    None,
    None,
    None,
    None)
}

class TLSlavePortParameters private(
  val slaves:         Seq[TLSlaveParameters],
  val channelBytes:   TLChannelBeatBytes,
  val endSinkId:      Int,
  val minLatency:     Int,
  val responseFields: Seq[BundleFieldBase],
  val requestKeys:    Seq[BundleKeyBase]) extends SimpleProduct
{
  override def canEqual(that: Any): Boolean = that.isInstanceOf[TLSlavePortParameters]
  override def productPrefix = "TLSlavePortParameters"
  def productArity: Int = 6
  def productElement(n: Int): Any = n match {
    case 0 => slaves
    case 1 => channelBytes
    case 2 => endSinkId
    case 3 => minLatency
    case 4 => responseFields
    case 5 => requestKeys
    case _ => throw new IndexOutOfBoundsException(n.toString)
  }

  require (!slaves.isEmpty, "Slave ports must have slaves")
  require (endSinkId >= 0, "Sink ids cannot be negative")
  require (minLatency >= 0, "Minimum required latency cannot be negative")

  // Using this API implies you cannot handle mixed-width busses
  def beatBytes = {
    channelBytes.members.foreach { width =>
      require (width.isDefined && width == channelBytes.a)
    }
    channelBytes.a.get
  }

  // TODO this should be deprecated
  def managers = slaves

  def requireFifo(policy: TLFIFOFixer.Policy = TLFIFOFixer.allFIFO) = {
    val relevant = slaves.filter(m => policy(m))
    relevant.foreach { m =>
      require(m.fifoId == relevant.head.fifoId, s"${m.name} had fifoId ${m.fifoId}, which was not homogeneous (${slaves.map(s => (s.name, s.fifoId))}) ")
    }
  }

  // Bounds on required sizes
  def maxAddress  = slaves.map(_.maxAddress).max
  def maxTransfer = slaves.map(_.maxTransfer).max
  def mayDenyGet  = slaves.exists(_.mayDenyGet)
  def mayDenyPut  = slaves.exists(_.mayDenyPut)

  // Diplomatically determined operation sizes emitted by all outward Slaves
  // as opposed to expectsVipChecker which generate circuitry to check which specific addresses
  val allEmitClaims = slaves.map(_.emits).reduce( _ intersect _)

  // Operation Emitted by at least one outward Slaves
  // as opposed to expectsVipChecker which generate circuitry to check which specific addresses
  val anyEmitClaims = slaves.map(_.emits).reduce(_ mincover _)

  // Diplomatically determined operation sizes supported by all outward Slaves
  // as opposed to expectsVipChecker which generate circuitry to check which specific addresses
  val allSupportClaims = slaves.map(_.supports).reduce( _ intersect _)
  val allSupportAcquireT   = allSupportClaims.acquireT
  val allSupportAcquireB   = allSupportClaims.acquireB
  val allSupportArithmetic = allSupportClaims.arithmetic
  val allSupportLogical    = allSupportClaims.logical
  val allSupportGet        = allSupportClaims.get
  val allSupportPutFull    = allSupportClaims.putFull
  val allSupportPutPartial = allSupportClaims.putPartial
  val allSupportHint       = allSupportClaims.hint

  // Operation supported by at least one outward Slaves
  // as opposed to expectsVipChecker which generate circuitry to check which specific addresses
  val anySupportClaims = slaves.map(_.supports).reduce(_ mincover _)
  val anySupportAcquireT   = !anySupportClaims.acquireT.none
  val anySupportAcquireB   = !anySupportClaims.acquireB.none
  val anySupportArithmetic = !anySupportClaims.arithmetic.none
  val anySupportLogical    = !anySupportClaims.logical.none
  val anySupportGet        = !anySupportClaims.get.none
  val anySupportPutFull    = !anySupportClaims.putFull.none
  val anySupportPutPartial = !anySupportClaims.putPartial.none
  val anySupportHint       = !anySupportClaims.hint.none

  // Supporting Acquire means being routable for GrantAck
  require ((endSinkId == 0) == !anySupportAcquireB)

  // These return Option[TLSlaveParameters] for your convenience
  def find(address: BigInt) = slaves.find(_.address.exists(_.contains(address)))

  // The safe version will check the entire address
  def findSafe(address: UInt) = Vec(slaves.map(_.address.map(_.contains(address)).reduce(_ || _)))
  // The fast version assumes the address is valid (you probably want fastProperty instead of this function)
  def findFast(address: UInt) = {
    val routingMask = AddressDecoder(slaves.map(_.address))
    Vec(slaves.map(_.address.map(_.widen(~routingMask)).distinct.map(_.contains(address)).reduce(_ || _)))
  }

  // Compute the simplest AddressSets that decide a key
  def fastPropertyGroup[K](p: TLSlaveParameters => K): Seq[(K, Seq[AddressSet])] = {
    val groups = groupByIntoSeq(slaves.map(m => (p(m), m.address)))( _._1).map { case (k, vs) =>
      k -> vs.flatMap(_._2)
    }
    val reductionMask = AddressDecoder(groups.map(_._2))
    groups.map { case (k, seq) => k -> AddressSet.unify(seq.map(_.widen(~reductionMask)).distinct) }
  }
  // Select a property
  def fastProperty[K, D <: Data](address: UInt, p: TLSlaveParameters => K, d: K => D): D =
    Mux1H(fastPropertyGroup(p).map { case (v, a) => (a.map(_.contains(address)).reduce(_||_), d(v)) })

  // Note: returns the actual fifoId + 1 or 0 if None
  def findFifoIdFast(address: UInt) = fastProperty(address, _.fifoId.map(_+1).getOrElse(0), (i:Int) => UInt(i))
  def hasFifoIdFast(address: UInt) = fastProperty(address, _.fifoId.isDefined, (b:Boolean) => Bool(b))

  // Does this Port manage this ID/address?
  def containsSafe(address: UInt) = findSafe(address).reduce(_ || _)

  private def addressHelper(
      // setting safe to false indicates that all addresses are expected to be legal, which might reduce circuit complexity
      safe:    Boolean,
      // member filters out the sizes being checked based on the opcode being emitted or supported
      member:  TLSlaveParameters => TransferSizes,
      address: UInt,
      lgSize:  UInt,
      // range provides a limit on the sizes that are expected to be evaluated, which might reduce circuit complexity
      range:   Option[TransferSizes]): Bool = {
    // trim reduces circuit complexity by intersecting checked sizes with the range argument
    def trim(x: TransferSizes) = range.map(_.intersect(x)).getOrElse(x)
    // groupBy returns an unordered map, convert back to Seq and sort the result for determinism
    // groupByIntoSeq is turning slaves into trimmed membership sizes
    // We are grouping all the slaves by their transfer size where
    // if they support the trimmed size then
    // member is the type of transfer that you are looking for (What you are trying to filter on)
    // When you consider membership, you are trimming the sizes to only the ones that you care about
    // you are filtering the slaves based on both whether they support a particular opcode and the size
    // Grouping the slaves based on the actual transfer size range they support
    // intersecting the range and checking their membership
    // FOR SUPPORTCASES instead of returning the list of slaves,
    // you are returning a map from transfer size to the set of
    // address sets that are supported for that transfer size

    // find all the slaves that support a certain type of operation and then group their addresses by the supported size
    // for every size there could be multiple address ranges
    // safety is a trade off between checking between all possible addresses vs only the addresses
    // that are known to have supported sizes
    // the trade off is 'checking all addresses is a more expensive circuit but will always give you
    // the right answer even if you give it an illegal address'
    // the not safe version is a cheaper circuit but if you give it an illegal address then it might produce the wrong answer
    // fast presumes address legality
    val supportCases = groupByIntoSeq(slaves)(m => trim(member(m))).map { case (k: TransferSizes, vs: Seq[TLSlaveParameters]) =>
      k -> vs.flatMap(_.address)
    }
    // safe produces a circuit that compares against all possible addresses,
    // whereas fast presumes that the address is legal but uses an efficient address decoder
    val mask = if (safe) ~BigInt(0) else AddressDecoder(supportCases.map(_._2))
    // Simplified creates the most concise possible representation of each cases' address sets based on the mask.
    val simplified = supportCases.map { case (k, seq) => k -> AddressSet.unify(seq.map(_.widen(~mask)).distinct) }
    simplified.map { case (s, a) =>
    // s is a size, you are checking for this size either the size of the operation is in s
    // We return an or-reduction of all the cases, checking whether any contains both the dynamic size and dynamic address on the wire.
      (Bool(Some(s) == range) || s.containsLg(lgSize)) &&
      a.map(_.contains(address)).reduce(_||_)
    }.foldLeft(Bool(false))(_||_)
  }

  @deprecated("Use edge.expectsVipCheckerMasterToSlaveAcquireT instead of manager.supportsAcquireTSafe","")
  def supportsAcquireTSafe   (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(true, _.supports.acquireT,   address, lgSize, range)
  @deprecated("Use edge.expectsVipCheckerMasterToSlaveAcquireB instead of manager.supportsAcquireBSafe","")
  def supportsAcquireBSafe   (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(true, _.supports.acquireB,   address, lgSize, range)
  @deprecated("Use edge.expectsVipCheckerMasterToSlaveArithmetic instead of manager.supportsArithmeticSafe","")
  def supportsArithmeticSafe (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(true, _.supports.arithmetic,   address, lgSize, range)
  @deprecated("Use edge.expectsVipCheckerMasterToSlaveLogical instead of manager.supportsLogicalSafe","")
  def supportsLogicalSafe    (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(true, _.supports.logical,   address, lgSize, range)
  @deprecated("Use edge.expectsVipCheckerMasterToSlaveGet instead of manager.supportsGetSafe","")
  def supportsGetSafe        (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(true, _.supports.get,   address, lgSize, range)
  @deprecated("Use edge.expectsVipCheckerMasterToSlavePutFull instead of manager.supportsPutFullSafe","")
  def supportsPutFullSafe    (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(true, _.supports.putFull,   address, lgSize, range)
  @deprecated("Use edge.expectsVipCheckerMasterToSlavePutPartial instead of manager.supportsPutPartialSafe","")
  def supportsPutPartialSafe (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(true, _.supports.putPartial,   address, lgSize, range)
  @deprecated("Use edge.expectsVipCheckerMasterToSlaveHint instead of manager.supportsHintSafe","")
  def supportsHintSafe       (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(true, _.supports.hint,   address, lgSize, range)

  @deprecated("Use edge.expectsVipCheckerMasterToSlaveAcquireT instead of manager.supportsAcquireTFast","")
  def supportsAcquireTFast   (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.acquireT,   address, lgSize, range)
  @deprecated("Use edge.expectsVipCheckerMasterToSlaveAcquireB instead of manager.supportsAcquireBFast","")
  def supportsAcquireBFast   (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.acquireB,   address, lgSize, range)
  @deprecated("Use edge.expectsVipCheckerMasterToSlaveArithmetic instead of manager.supportsArithmeticFast","")
  def supportsArithmeticFast (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.arithmetic,   address, lgSize, range)
  @deprecated("Use edge.expectsVipCheckerMasterToSlaveLogical instead of manager.supportsLogicalFast","")
  def supportsLogicalFast    (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.logical,   address, lgSize, range)
  @deprecated("Use edge.expectsVipCheckerMasterToSlaveGet instead of manager.supportsGetFast","")
  def supportsGetFast        (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.get,   address, lgSize, range)
  @deprecated("Use edge.expectsVipCheckerMasterToSlavePutFull instead of manager.supportsPutFullFast","")
  def supportsPutFullFast    (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.putFull,   address, lgSize, range)
  @deprecated("Use edge.expectsVipCheckerMasterToSlavePutPartial instead of manager.supportsPutPartialFast","")
  def supportsPutPartialFast (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.putPartial,   address, lgSize, range)
  @deprecated("Use edge.expectsVipCheckerMasterToSlaveHint instead of manager.supportsHintFast","")
  def supportsHintFast       (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.hint,   address, lgSize, range)

  // Check for support of a given operation at a specific address
  def expectsVipCheckerSupportsAcquireT  (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.acquireT,   address, lgSize, range)
  def expectsVipCheckerSupportsAcquireB  (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.acquireB,   address, lgSize, range)
  def expectsVipCheckerSupportsArithmetic(address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.arithmetic, address, lgSize, range)
  def expectsVipCheckerSupportsLogical   (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.logical,    address, lgSize, range)
  def expectsVipCheckerSupportsGet       (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.get,        address, lgSize, range)
  def expectsVipCheckerSupportsPutFull   (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.putFull,    address, lgSize, range)
  def expectsVipCheckerSupportsPutPartial(address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.putPartial, address, lgSize, range)
  def expectsVipCheckerSupportsHint      (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.supports.hint,       address, lgSize, range)

  def expectsVipCheckerEmitsProbe        (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.emits.probe,         address, lgSize, range)
  def expectsVipCheckerEmitsArithmetic   (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.emits.arithmetic,    address, lgSize, range)
  def expectsVipCheckerEmitsLogical      (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.emits.logical,       address, lgSize, range)
  def expectsVipCheckerEmitsGet          (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.emits.get,           address, lgSize, range)
  def expectsVipCheckerEmitsPutFull      (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.emits.putFull,       address, lgSize, range)
  def expectsVipCheckerEmitsPutPartial   (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.emits.putPartial,    address, lgSize, range)
  def expectsVipCheckerEmitsHint         (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = addressHelper(false, _.emits.hint,          address, lgSize, range)

  def findTreeViolation() = slaves.flatMap(_.findTreeViolation()).headOption
  def isTree = !slaves.exists(!_.isTree)

  def infoString = "Slave Port Beatbytes = " + beatBytes + "\n" + "Slave Port MinLatency = " + minLatency + "\n\n" + slaves.map(_.infoString).mkString

  def v1copy(
    managers:   Seq[TLSlaveParameters] = slaves,
    beatBytes:  Int = -1,
    endSinkId:  Int = endSinkId,
    minLatency: Int = minLatency,
    responseFields: Seq[BundleFieldBase] = responseFields,
    requestKeys:    Seq[BundleKeyBase]   = requestKeys) =
  {
    new TLSlavePortParameters(
      slaves       = managers,
      channelBytes = if (beatBytes != -1) TLChannelBeatBytes(beatBytes) else channelBytes,
      endSinkId    = endSinkId,
      minLatency   = minLatency,
      responseFields = responseFields,
      requestKeys    = requestKeys)
  }

    def v2copy(
    slaves:         Seq[TLSlaveParameters] = slaves,
    channelBytes:   TLChannelBeatBytes     = channelBytes,
    endSinkId:      Int                    = endSinkId,
    minLatency:     Int                    = minLatency,
    responseFields: Seq[BundleFieldBase]   = responseFields,
    requestKeys:    Seq[BundleKeyBase]     = requestKeys) =
  {
    new TLSlavePortParameters(
      slaves         = slaves,
      channelBytes   = channelBytes,
      endSinkId      = endSinkId,
      minLatency     = minLatency,
      responseFields = responseFields,
      requestKeys    = requestKeys)
  }

  @deprecated("Use v1copy instead of copy","")
  def copy(
    managers:   Seq[TLSlaveParameters] = slaves,
    beatBytes:  Int = -1,
    endSinkId:  Int = endSinkId,
    minLatency: Int = minLatency,
    responseFields: Seq[BundleFieldBase] = responseFields,
    requestKeys:    Seq[BundleKeyBase]   = requestKeys) =
  {
    v1copy(
      managers,
      beatBytes,
      endSinkId,
      minLatency,
      responseFields,
      requestKeys)
  }
}

object TLSlavePortParameters {
  def v1(
    managers:   Seq[TLSlaveParameters],
    beatBytes:  Int,
    endSinkId:  Int = 0,
    minLatency: Int = 0,
    responseFields: Seq[BundleFieldBase] = Nil,
    requestKeys:    Seq[BundleKeyBase]   = Nil) =
  {
    new TLSlavePortParameters(
      slaves       = managers,
      channelBytes = TLChannelBeatBytes(beatBytes),
      endSinkId    = endSinkId,
      minLatency   = minLatency,
      responseFields = responseFields,
      requestKeys    = requestKeys)
  }

}

object TLManagerPortParameters {
  @deprecated("Use TLSlavePortParameters.v1 instead of TLManagerPortParameters","")
  def apply(
    managers:   Seq[TLSlaveParameters],
    beatBytes:  Int,
    endSinkId:  Int = 0,
    minLatency: Int = 0,
    responseFields: Seq[BundleFieldBase] = Nil,
    requestKeys:    Seq[BundleKeyBase]   = Nil) =
  {
    TLSlavePortParameters.v1(
      managers,
      beatBytes,
      endSinkId,
      minLatency,
      responseFields,
      requestKeys)
  }
}

class TLMasterParameters private(
  val nodePath:          Seq[BaseNode],
  val resources:         Seq[Resource],
  val name:              String,
  val visibility:        Seq[AddressSet],
  val unusedRegionTypes: Set[RegionType.T],
  val executesOnly:      Boolean,
  val requestFifo:       Boolean, // only a request, not a requirement. applies to A, not C.
  val supports:          TLSlaveToMasterTransferSizes,
  val emits:             TLMasterToSlaveTransferSizes,
  val neverReleasesData: Boolean,
  val sourceId:          IdRange) extends SimpleProduct
{
  override def canEqual(that: Any): Boolean = that.isInstanceOf[TLMasterParameters]
  override def productPrefix = "TLMasterParameters"
  // We intentionally omit nodePath for equality testing / formatting
  def productArity: Int = 10
  def productElement(n: Int): Any = n match {
    case 0 => name
    case 1 => sourceId
    case 2 => resources
    case 3 => visibility
    case 4 => unusedRegionTypes
    case 5 => executesOnly
    case 6 => requestFifo
    case 7 => supports
    case 8 => emits
    case 9 => neverReleasesData
    case _ => throw new IndexOutOfBoundsException(n.toString)
  }

  @deprecated("Use supports.probe instead of supportsProbe","")
  def supportsProbe:       TransferSizes   = supports.probe
  @deprecated("Use supports.arithmetic instead of supportsArithmetic","")
  def supportsArithmetic:  TransferSizes   = supports.arithmetic
  @deprecated("Use supports.logical instead of supportsLogical","")
  def supportsLogical:     TransferSizes   = supports.logical
  @deprecated("Use supports.get instead of supportsGet","")
  def supportsGet:         TransferSizes   = supports.get
  @deprecated("Use supports.putFull instead of supportsPutFull","")
  def supportsPutFull:     TransferSizes   = supports.putFull
  @deprecated("Use supports.putPartial instead of supportsPutPartial","")
  def supportsPutPartial:  TransferSizes   = supports.putPartial
  @deprecated("Use supports.hint instead of supportsHint","")
  def supportsHint:        TransferSizes   = supports.hint

  require (!sourceId.isEmpty)
  require (!visibility.isEmpty)
  require (supports.putFull.contains(supports.putPartial))
  // We only support these operations if we support Probe (ie: we're a cache)
  require (supports.probe.contains(supports.arithmetic))
  require (supports.probe.contains(supports.logical))
  require (supports.probe.contains(supports.get))
  require (supports.probe.contains(supports.putFull))
  require (supports.probe.contains(supports.putPartial))
  require (supports.probe.contains(supports.hint))

  visibility.combinations(2).foreach { case Seq(x,y) => require (!x.overlaps(y), s"$x and $y overlap.") }

  val maxTransfer = List(
    supports.probe.max,
    supports.arithmetic.max,
    supports.logical.max,
    supports.get.max,
    supports.putFull.max,
    supports.putPartial.max).max

  def infoString = {
    s"""Master Name = ${name}
       |visibility = ${visibility}
       |emits = ${emits.infoString}
       |sourceId = ${sourceId}
       |
       |""".stripMargin
  }

  def v1copy(
    name:                String          = name,
    sourceId:            IdRange         = sourceId,
    nodePath:            Seq[BaseNode]   = nodePath,
    requestFifo:         Boolean         = requestFifo,
    visibility:          Seq[AddressSet] = visibility,
    supportsProbe:       TransferSizes   = supports.probe,
    supportsArithmetic:  TransferSizes   = supports.arithmetic,
    supportsLogical:     TransferSizes   = supports.logical,
    supportsGet:         TransferSizes   = supports.get,
    supportsPutFull:     TransferSizes   = supports.putFull,
    supportsPutPartial:  TransferSizes   = supports.putPartial,
    supportsHint:        TransferSizes   = supports.hint) =
  {
    new TLMasterParameters(
      nodePath          = nodePath,
      resources         = this.resources,
      name              = name,
      visibility        = visibility,
      unusedRegionTypes = this.unusedRegionTypes,
      executesOnly      = this.executesOnly,
      requestFifo       = requestFifo,
      supports          = TLSlaveToMasterTransferSizes(
        probe             = supportsProbe,
        arithmetic        = supportsArithmetic,
        logical           = supportsLogical,
        get               = supportsGet,
        putFull           = supportsPutFull,
        putPartial        = supportsPutPartial,
        hint              = supportsHint),
      emits             = this.emits,
      neverReleasesData = this.neverReleasesData,
      sourceId          = sourceId)
  }

  def v2copy(
    nodePath:          Seq[BaseNode]                = nodePath,
    resources:         Seq[Resource]                = resources,
    name:              String                       = name,
    visibility:        Seq[AddressSet]              = visibility,
    unusedRegionTypes: Set[RegionType.T]            = unusedRegionTypes,
    executesOnly:      Boolean                      = executesOnly,
    requestFifo:       Boolean                      = requestFifo,
    supports:          TLSlaveToMasterTransferSizes = supports,
    emits:             TLMasterToSlaveTransferSizes = emits,
    neverReleasesData: Boolean                      = neverReleasesData,
    sourceId:          IdRange                      = sourceId) =
  {
    new TLMasterParameters(
      nodePath          = nodePath,
      resources         = resources,
      name              = name,
      visibility        = visibility,
      unusedRegionTypes = unusedRegionTypes,
      executesOnly      = executesOnly,
      requestFifo       = requestFifo,
      supports          = supports,
      emits             = emits,
      neverReleasesData = neverReleasesData,
      sourceId          = sourceId)
  }

  @deprecated("Use v1copy instead of copy","")
  def copy(
    name:                String          = name,
    sourceId:            IdRange         = sourceId,
    nodePath:            Seq[BaseNode]   = nodePath,
    requestFifo:         Boolean         = requestFifo,
    visibility:          Seq[AddressSet] = visibility,
    supportsProbe:       TransferSizes   = supports.probe,
    supportsArithmetic:  TransferSizes   = supports.arithmetic,
    supportsLogical:     TransferSizes   = supports.logical,
    supportsGet:         TransferSizes   = supports.get,
    supportsPutFull:     TransferSizes   = supports.putFull,
    supportsPutPartial:  TransferSizes   = supports.putPartial,
    supportsHint:        TransferSizes   = supports.hint) =
  {
    v1copy(
      name               = name,
      sourceId           = sourceId,
      nodePath           = nodePath,
      requestFifo        = requestFifo,
      visibility         = visibility,
      supportsProbe      = supportsProbe,
      supportsArithmetic = supportsArithmetic,
      supportsLogical    = supportsLogical,
      supportsGet        = supportsGet,
      supportsPutFull    = supportsPutFull,
      supportsPutPartial = supportsPutPartial,
      supportsHint       = supportsHint)
  }
}

object TLMasterParameters {
  def v1(
    name:                String,
    sourceId:            IdRange         = IdRange(0,1),
    nodePath:            Seq[BaseNode]   = Seq(),
    requestFifo:         Boolean         = false,
    visibility:          Seq[AddressSet] = Seq(AddressSet(0, ~0)),
    supportsProbe:       TransferSizes   = TransferSizes.none,
    supportsArithmetic:  TransferSizes   = TransferSizes.none,
    supportsLogical:     TransferSizes   = TransferSizes.none,
    supportsGet:         TransferSizes   = TransferSizes.none,
    supportsPutFull:     TransferSizes   = TransferSizes.none,
    supportsPutPartial:  TransferSizes   = TransferSizes.none,
    supportsHint:        TransferSizes   = TransferSizes.none) =
  {
    new TLMasterParameters(
      nodePath          = nodePath,
      resources         = Nil,
      name              = name,
      visibility        = visibility,
      unusedRegionTypes = Set(),
      executesOnly      = false,
      requestFifo       = requestFifo,
      supports          = TLSlaveToMasterTransferSizes(
        probe             = supportsProbe,
        arithmetic        = supportsArithmetic,
        logical           = supportsLogical,
        get               = supportsGet,
        putFull           = supportsPutFull,
        putPartial        = supportsPutPartial,
        hint              = supportsHint),
      emits             = TLMasterToSlaveTransferSizes.unknownEmits,
      neverReleasesData = false,
      sourceId          = sourceId)
  }

  def v2(
    nodePath:          Seq[BaseNode]                = Seq(),
    resources:         Seq[Resource]                = Nil,
    name:              String,
    visibility:        Seq[AddressSet]              = Seq(AddressSet(0, ~0)),
    unusedRegionTypes: Set[RegionType.T]            = Set(),
    executesOnly:      Boolean                      = false,
    requestFifo:       Boolean                      = false,
    supports:          TLSlaveToMasterTransferSizes = TLSlaveToMasterTransferSizes.unknownSupports,
    emits:             TLMasterToSlaveTransferSizes = TLMasterToSlaveTransferSizes.unknownEmits,
    neverReleasesData: Boolean                      = false,
    sourceId:          IdRange                      = IdRange(0,1)) =
  {
    new TLMasterParameters(
      nodePath          = nodePath,
      resources         = resources,
      name              = name,
      visibility        = visibility,
      unusedRegionTypes = unusedRegionTypes,
      executesOnly      = executesOnly,
      requestFifo       = requestFifo,
      supports          = supports,
      emits             = emits,
      neverReleasesData = neverReleasesData,
      sourceId          = sourceId)
  }
}

object TLClientParameters {
  @deprecated("Use TLMasterParameters.v1 instead of TLClientParameters","")
  def apply(
    name:                String,
    sourceId:            IdRange         = IdRange(0,1),
    nodePath:            Seq[BaseNode]   = Seq(),
    requestFifo:         Boolean         = false,
    visibility:          Seq[AddressSet] = Seq(AddressSet.everything),
    supportsProbe:       TransferSizes   = TransferSizes.none,
    supportsArithmetic:  TransferSizes   = TransferSizes.none,
    supportsLogical:     TransferSizes   = TransferSizes.none,
    supportsGet:         TransferSizes   = TransferSizes.none,
    supportsPutFull:     TransferSizes   = TransferSizes.none,
    supportsPutPartial:  TransferSizes   = TransferSizes.none,
    supportsHint:        TransferSizes   = TransferSizes.none) =
  {
    TLMasterParameters.v1(
      name               = name,
      sourceId           = sourceId,
      nodePath           = nodePath,
      requestFifo        = requestFifo,
      visibility         = visibility,
      supportsProbe      = supportsProbe,
      supportsArithmetic = supportsArithmetic,
      supportsLogical    = supportsLogical,
      supportsGet        = supportsGet,
      supportsPutFull    = supportsPutFull,
      supportsPutPartial = supportsPutPartial,
      supportsHint       = supportsHint)
  }
}

class TLMasterPortParameters private(
  val masters:       Seq[TLMasterParameters],
  val channelBytes:  TLChannelBeatBytes,
  val minLatency:    Int,
  val echoFields:    Seq[BundleFieldBase],
  val requestFields: Seq[BundleFieldBase],
  val responseKeys:  Seq[BundleKeyBase]) extends SimpleProduct
{
  override def canEqual(that: Any): Boolean = that.isInstanceOf[TLMasterPortParameters]
  override def productPrefix = "TLMasterPortParameters"
  def productArity: Int = 6
  def productElement(n: Int): Any = n match {
    case 0 => masters
    case 1 => channelBytes
    case 2 => minLatency
    case 3 => echoFields
    case 4 => requestFields
    case 5 => responseKeys
    case _ => throw new IndexOutOfBoundsException(n.toString)
  }

  require (!masters.isEmpty)
  require (minLatency >= 0)

  def clients = masters

  // Require disjoint ranges for Ids
  IdRange.overlaps(masters.map(_.sourceId)).foreach { case (x, y) =>
    require (!x.overlaps(y), s"TLClientParameters.sourceId ${x} overlaps ${y}")
  }

  // Bounds on required sizes
  def endSourceId = masters.map(_.sourceId.end).max
  def maxTransfer = masters.map(_.maxTransfer).max

  // The unused sources < endSourceId
  def unusedSources: Seq[Int] = {
    val usedSources = masters.map(_.sourceId).sortBy(_.start)
    ((Seq(0) ++ usedSources.map(_.end)) zip usedSources.map(_.start)) flatMap { case (end, start) =>
      end until start
    }
  }

  // Diplomatically determined operation sizes emitted/supported by all inward Masters
  // as opposed to expectsVipChecker which generate circuitry to check which specific addresses
  val allEmitClaims    = masters.map(_.emits)   .reduce( _ intersect _)

  // Diplomatically determined operation sizes Emitted/Supported by at least one inward Masters
  // as opposed to expectsVipChecker which generate circuitry to check which specific addresses
  val anyEmitClaims    = masters.map(_.emits)   .reduce(_ mincover _)
  val anySupportClaims = masters.map(_.supports).reduce(_ mincover _)

  // Diplomatically determined operation sizes supported by all inward Masters
  // as opposed to expectsVipChecker which generate circuitry to check which specific addresses
  val allSupportProbe      = masters.map(_.supportsProbe)     .reduce(_ intersect _)
  val allSupportArithmetic = masters.map(_.supportsArithmetic).reduce(_ intersect _)
  val allSupportLogical    = masters.map(_.supportsLogical)   .reduce(_ intersect _)
  val allSupportGet        = masters.map(_.supportsGet)       .reduce(_ intersect _)
  val allSupportPutFull    = masters.map(_.supportsPutFull)   .reduce(_ intersect _)
  val allSupportPutPartial = masters.map(_.supportsPutPartial).reduce(_ intersect _)
  val allSupportHint       = masters.map(_.supportsHint)      .reduce(_ intersect _)

  // Diplomatically determined operation sizes supported by at least one master
  // as opposed to expectsVipChecker which generate circuitry to check which specific addresses
  val anySupportProbe      = masters.map(!_.supportsProbe.none)     .reduce(_ || _)
  val anySupportArithmetic = masters.map(!_.supportsArithmetic.none).reduce(_ || _)
  val anySupportLogical    = masters.map(!_.supportsLogical.none)   .reduce(_ || _)
  val anySupportGet        = masters.map(!_.supportsGet.none)       .reduce(_ || _)
  val anySupportPutFull    = masters.map(!_.supportsPutFull.none)   .reduce(_ || _)
  val anySupportPutPartial = masters.map(!_.supportsPutPartial.none).reduce(_ || _)
  val anySupportHint       = masters.map(!_.supportsHint.none)      .reduce(_ || _)

  // These return Option[TLMasterParameters] for your convenience
  def find(id: Int) = masters.find(_.sourceId.contains(id))

  // Synthesizable lookup methods
  def find(id: UInt) = Vec(masters.map(_.sourceId.contains(id)))
  def contains(id: UInt) = find(id).reduce(_ || _)

  def requestFifo(id: UInt) = Mux1H(find(id), masters.map(c => Bool(c.requestFifo)))

  // Available during RTL runtime, checks to see if (id, size) is supported by the master's (client's) diplomatic parameters
  private def sourceIdHelper(member: TLMasterParameters => TransferSizes)(id: UInt, lgSize: UInt) = {
    val allSame = masters.map(member(_) == member(masters(0))).reduce(_ && _)
    // this if statement is a coarse generalization of the groupBy in the sourceIdHelper2 version;
    // the case where there is only one group.
    if (allSame) member(masters(0)).containsLg(lgSize) else {
      // Find the master associated with ID and returns whether that particular master is able to receive transaction of lgSize
      Mux1H(find(id), masters.map(member(_).containsLg(lgSize)))
    }
  }

  @deprecated("Use edge.expectsVipCheckerSlaveToMasterProbe instead of clientPort.supportsProbe","")
  def supportsProbe = expectsVipCheckerSupportsProbe
  @deprecated("Use edge.expectsVipCheckerSlaveToMasterArithmetic instead of clientPort.supportsArithmetic","")
  def supportsArithmetic = expectsVipCheckerSupportsArithmetic
  @deprecated("Use edge.expectsVipCheckerSlaveToMasterLogical instead of clientPort.supportsLogical","")
  def supportsLogical = expectsVipCheckerSupportsLogical
  @deprecated("Use edge.expectsVipCheckerSlaveToMasterGet instead of clientPort.supportsGet","")
  def supportsGet = expectsVipCheckerSupportsGet
  @deprecated("Use edge.expectsVipCheckerSlaveToMasterPutFull instead of clientPort.supportsPutFull","")
  def supportsPutFull = expectsVipCheckerSupportsPutFull
  @deprecated("Use edge.expectsVipCheckerSlaveToMasterPutPartial instead of clientPort.supportsPutPartial","")
  def supportsPutPartial = expectsVipCheckerSupportsPutPartial
  @deprecated("Use edge.expectsVipCheckerSlaveToMasterHint instead of clientPort.supportsHint","")
  def supportsHint = expectsVipCheckerSupportsHint

  // Check for support of a given operation at a specific id
  val expectsVipCheckerSupportsProbe      = sourceIdHelper(_.supports.probe)      _
  val expectsVipCheckerSupportsArithmetic = sourceIdHelper(_.supports.arithmetic) _
  val expectsVipCheckerSupportsLogical    = sourceIdHelper(_.supports.logical)    _
  val expectsVipCheckerSupportsGet        = sourceIdHelper(_.supports.get)        _
  val expectsVipCheckerSupportsPutFull    = sourceIdHelper(_.supports.putFull)    _
  val expectsVipCheckerSupportsPutPartial = sourceIdHelper(_.supports.putPartial) _
  val expectsVipCheckerSupportsHint       = sourceIdHelper(_.supports.hint)       _

  // TODO: Merge sourceIdHelper2 with sourceIdHelper
  private def sourceIdHelper2(
    member: TLMasterParameters => TransferSizes,
    sourceId: UInt,
    lgSize:  UInt): Bool = {
    // Because sourceIds are uniquely owned by each master, we use them to group the
    // cases that have to be checked.
    val emitCases = groupByIntoSeq(masters)(m => member(m)).map { case (k, vs) =>
      k -> vs.map(_.sourceId)
    }
    emitCases.map { case (s, a) =>
      (s.containsLg(lgSize)) &&
      a.map(_.contains(sourceId)).reduce(_||_)
    }.foldLeft(Bool(false))(_||_)
  }

  // Check for emit of a given operation at a specific id
  def expectsVipCheckerEmitsAcquireT  (sourceId: UInt, lgSize: UInt) = sourceIdHelper2(_.emits.acquireT,   sourceId, lgSize)
  def expectsVipCheckerEmitsAcquireB  (sourceId: UInt, lgSize: UInt) = sourceIdHelper2(_.emits.acquireB,   sourceId, lgSize)
  def expectsVipCheckerEmitsArithmetic(sourceId: UInt, lgSize: UInt) = sourceIdHelper2(_.emits.arithmetic, sourceId, lgSize)
  def expectsVipCheckerEmitsLogical   (sourceId: UInt, lgSize: UInt) = sourceIdHelper2(_.emits.logical,    sourceId, lgSize)
  def expectsVipCheckerEmitsGet       (sourceId: UInt, lgSize: UInt) = sourceIdHelper2(_.emits.get,        sourceId, lgSize)
  def expectsVipCheckerEmitsPutFull   (sourceId: UInt, lgSize: UInt) = sourceIdHelper2(_.emits.putFull,    sourceId, lgSize)
  def expectsVipCheckerEmitsPutPartial(sourceId: UInt, lgSize: UInt) = sourceIdHelper2(_.emits.putPartial, sourceId, lgSize)
  def expectsVipCheckerEmitsHint      (sourceId: UInt, lgSize: UInt) = sourceIdHelper2(_.emits.hint,       sourceId, lgSize)

  def infoString = masters.map(_.infoString).mkString

  def v1copy(
    clients: Seq[TLMasterParameters] = masters,
    minLatency: Int = minLatency,
    echoFields:    Seq[BundleFieldBase] = echoFields,
    requestFields: Seq[BundleFieldBase] = requestFields,
    responseKeys:  Seq[BundleKeyBase]   = responseKeys) =
  {
    new TLMasterPortParameters(
      masters       = clients,
      channelBytes  = channelBytes,
      minLatency    = minLatency,
      echoFields    = echoFields,
      requestFields = requestFields,
      responseKeys  = responseKeys)
  }

  def v2copy(
    masters:       Seq[TLMasterParameters] = masters,
    channelBytes:  TLChannelBeatBytes      = channelBytes,
    minLatency:    Int                     = minLatency,
    echoFields:    Seq[BundleFieldBase]    = echoFields,
    requestFields: Seq[BundleFieldBase]    = requestFields,
    responseKeys:  Seq[BundleKeyBase]      = responseKeys) =
  {
    new TLMasterPortParameters(
      masters       = masters,
      channelBytes  = channelBytes,
      minLatency    = minLatency,
      echoFields    = echoFields,
      requestFields = requestFields,
      responseKeys  = responseKeys)
  }

  @deprecated("Use v1copy instead of copy","")
  def copy(
    clients: Seq[TLMasterParameters] = masters,
    minLatency: Int = minLatency,
    echoFields:    Seq[BundleFieldBase] = echoFields,
    requestFields: Seq[BundleFieldBase] = requestFields,
    responseKeys:  Seq[BundleKeyBase]   = responseKeys) =
  {
    v1copy(
      clients,
      minLatency,
      echoFields,
      requestFields,
      responseKeys)
  }
}

object TLClientPortParameters {
  @deprecated("Use TLMasterPortParameters.v1 instead of TLClientPortParameters","")
  def apply(
    clients: Seq[TLMasterParameters],
    minLatency: Int = 0,
    echoFields:    Seq[BundleFieldBase] = Nil,
    requestFields: Seq[BundleFieldBase] = Nil,
    responseKeys:  Seq[BundleKeyBase]   = Nil) =
  {
    TLMasterPortParameters.v1(
      clients,
      minLatency,
      echoFields,
      requestFields,
      responseKeys)
  }
}

object TLMasterPortParameters {
  def v1(
    clients:       Seq[TLMasterParameters],
    minLatency:    Int = 0,
    echoFields:    Seq[BundleFieldBase] = Nil,
    requestFields: Seq[BundleFieldBase] = Nil,
    responseKeys:  Seq[BundleKeyBase]   = Nil) =
  {
    new TLMasterPortParameters(
      masters       = clients,
      channelBytes  = TLChannelBeatBytes(),
      minLatency    = minLatency,
      echoFields    = echoFields,
      requestFields = requestFields,
      responseKeys  = responseKeys)
  }
  def v2(
    masters:       Seq[TLMasterParameters],
    channelBytes:  TLChannelBeatBytes   = TLChannelBeatBytes(),
    minLatency:    Int                  = 0,
    echoFields:    Seq[BundleFieldBase] = Nil,
    requestFields: Seq[BundleFieldBase] = Nil,
    responseKeys:  Seq[BundleKeyBase]   = Nil) =
  {
    new TLMasterPortParameters(
      masters       = masters,
      channelBytes  = channelBytes,
      minLatency    = minLatency,
      echoFields    = echoFields,
      requestFields = requestFields,
      responseKeys  = responseKeys)
  }
}

case class TLBundleParameters(
  addressBits: Int,
  dataBits:    Int,
  sourceBits:  Int,
  sinkBits:    Int,
  sizeBits:    Int,
  echoFields:     Seq[BundleFieldBase],
  requestFields:  Seq[BundleFieldBase],
  responseFields: Seq[BundleFieldBase],
  hasBCE: Boolean)
{
  // Chisel has issues with 0-width wires
  require (addressBits >= 1)
  require (dataBits    >= 8)
  require (sourceBits  >= 1)
  require (sinkBits    >= 1)
  require (sizeBits    >= 1)
  require (isPow2(dataBits))
  echoFields.foreach { f => require (f.key.isControl, s"${f} is not a legal echo field") }

  val addrLoBits = log2Up(dataBits/8)

  def union(x: TLBundleParameters) =
    TLBundleParameters(
      max(addressBits, x.addressBits),
      max(dataBits,    x.dataBits),
      max(sourceBits,  x.sourceBits),
      max(sinkBits,    x.sinkBits),
      max(sizeBits,    x.sizeBits),
      echoFields     = BundleField.union(echoFields     ++ x.echoFields),
      requestFields  = BundleField.union(requestFields  ++ x.requestFields),
      responseFields = BundleField.union(responseFields ++ x.responseFields),
      hasBCE || x.hasBCE)
}

object TLBundleParameters
{
  val emptyBundleParams = TLBundleParameters(
    addressBits = 1,
    dataBits    = 8,
    sourceBits  = 1,
    sinkBits    = 1,
    sizeBits    = 1,
    echoFields     = Nil,
    requestFields  = Nil,
    responseFields = Nil,
    hasBCE = false)

  def union(x: Seq[TLBundleParameters]) = x.foldLeft(emptyBundleParams)((x,y) => x.union(y))

  def apply(master: TLMasterPortParameters, slave: TLSlavePortParameters) =
    new TLBundleParameters(
      addressBits = log2Up(slave.maxAddress + 1),
      dataBits    = slave.beatBytes * 8,
      sourceBits  = log2Up(master.endSourceId),
      sinkBits    = log2Up(slave.endSinkId),
      sizeBits    = log2Up(log2Ceil(max(master.maxTransfer, slave.maxTransfer))+1),
      echoFields     = master.echoFields,
      requestFields  = BundleField.accept(master.requestFields, slave.requestKeys),
      responseFields = BundleField.accept(slave.responseFields, master.responseKeys),
      hasBCE = master.anySupportProbe && slave.anySupportAcquireB)
}

case class TLEdgeParameters(
  master: TLMasterPortParameters,
  slave:  TLSlavePortParameters,
  params:  Parameters,
  sourceInfo: SourceInfo) extends FormatEdge
{
  // legacy names:
  def manager = slave
  def client = master

  val maxTransfer = max(master.maxTransfer, slave.maxTransfer)
  val maxLgSize = log2Ceil(maxTransfer)

  // Sanity check the link...
  require (maxTransfer >= slave.beatBytes, s"Link's max transfer (${maxTransfer}) < ${slave.slaves.map(_.name)}'s beatBytes (${slave.beatBytes})")

  def diplomaticClaimsMasterToSlave = master.anyEmitClaims.intersect(slave.anySupportClaims)
  def diplomaticClaimsSlaveToMaster = master.anySupportClaims.intersect(slave.anyEmitClaims)

  // For emits, check that the source is allowed to send this transactions
  //These A channel messages from MasterToSlave are:
  //being routed to a slave based on address bits,
  //    so they need to be passed to a helper that checks whether the slave that owns certain addresses
  //    claimed to support transactions of this size/type
  //being sent from a master using sourceId bits,
  //    so they need to be passed to a helper that checks whether the master that owns certain sourceIds
  //    claimed to emit transactions of this size/type
  def expectsVipCheckerMasterToSlaveAcquireT   (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.expectsVipCheckerEmitsAcquireT      (sourceId, lgSize) && slave.expectsVipCheckerSupportsAcquireT   (address, lgSize, range)
  def expectsVipCheckerMasterToSlaveAcquireB   (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.expectsVipCheckerEmitsAcquireB      (sourceId, lgSize) && slave.expectsVipCheckerSupportsAcquireB   (address, lgSize, range)
  def expectsVipCheckerMasterToSlaveArithmetic (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.expectsVipCheckerEmitsArithmetic    (sourceId, lgSize) && slave.expectsVipCheckerSupportsArithmetic (address, lgSize, range)
  def expectsVipCheckerMasterToSlaveLogical    (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.expectsVipCheckerEmitsLogical       (sourceId, lgSize) && slave.expectsVipCheckerSupportsLogical    (address, lgSize, range)
  def expectsVipCheckerMasterToSlaveGet        (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.expectsVipCheckerEmitsGet           (sourceId, lgSize) && slave.expectsVipCheckerSupportsGet        (address, lgSize, range)
  def expectsVipCheckerMasterToSlavePutFull    (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.expectsVipCheckerEmitsPutFull       (sourceId, lgSize) && slave.expectsVipCheckerSupportsPutFull    (address, lgSize, range)
  def expectsVipCheckerMasterToSlavePutPartial (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.expectsVipCheckerEmitsPutPartial    (sourceId, lgSize) && slave.expectsVipCheckerSupportsPutPartial (address, lgSize, range)
  def expectsVipCheckerMasterToSlaveHint       (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.expectsVipCheckerEmitsHint          (sourceId, lgSize) && slave.expectsVipCheckerSupportsHint       (address, lgSize, range)

  //Duality: these B channel messages from SlaveToMaster are:
  //being routed to a master based on sourceId bits,
  //    so they need to be passed to a helper that checks whether the master that owns certain sourceIds
  //    claimed to support transactions of this size/type
  //being sent from a slave owning certain addresses,
  //    so they need to be passed to a helper that checks with the slave that owns certain addresses claimed
  //    to emit transactions of this size/type
  def expectsVipCheckerSlaveToMasterProbe      (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.expectsVipCheckerSupportsProbe      (sourceId, lgSize) && slave.expectsVipCheckerEmitsProbe         (address, lgSize, range)
  def expectsVipCheckerSlaveToMasterArithmetic (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.expectsVipCheckerSupportsArithmetic (sourceId, lgSize) && slave.expectsVipCheckerEmitsArithmetic    (address, lgSize, range)
  def expectsVipCheckerSlaveToMasterLogical    (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.expectsVipCheckerSupportsLogical    (sourceId, lgSize) && slave.expectsVipCheckerEmitsLogical       (address, lgSize, range)
  def expectsVipCheckerSlaveToMasterGet        (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.expectsVipCheckerSupportsGet        (sourceId, lgSize) && slave.expectsVipCheckerEmitsGet           (address, lgSize, range)
  def expectsVipCheckerSlaveToMasterPutFull    (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.expectsVipCheckerSupportsPutFull    (sourceId, lgSize) && slave.expectsVipCheckerEmitsPutFull       (address, lgSize, range)
  def expectsVipCheckerSlaveToMasterPutPartial (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.expectsVipCheckerSupportsPutPartial (sourceId, lgSize) && slave.expectsVipCheckerEmitsPutPartial    (address, lgSize, range)
  def expectsVipCheckerSlaveToMasterHint       (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.expectsVipCheckerSupportsHint       (sourceId, lgSize) && slave.expectsVipCheckerEmitsHint          (address, lgSize, range)

  val bundle = TLBundleParameters(master, slave)
  def formatEdge = master.infoString + "\n" + slave.infoString
}

case class TLCreditedDelay(
  a: CreditedDelay,
  b: CreditedDelay,
  c: CreditedDelay,
  d: CreditedDelay,
  e: CreditedDelay)
{
  def + (that: TLCreditedDelay): TLCreditedDelay = TLCreditedDelay(
    a = a + that.a,
    b = b + that.b,
    c = c + that.c,
    d = d + that.d,
    e = e + that.e)

  override def toString = s"(${a}, ${b}, ${c}, ${d}, ${e})"
}

object TLCreditedDelay {
  def apply(delay: CreditedDelay): TLCreditedDelay = apply(delay, delay.flip, delay, delay.flip, delay)
}

case class TLCreditedManagerPortParameters(delay: TLCreditedDelay, base: TLSlavePortParameters) {def infoString = base.infoString}
case class TLCreditedClientPortParameters(delay: TLCreditedDelay, base: TLMasterPortParameters) {def infoString = base.infoString}
case class TLCreditedEdgeParameters(client: TLCreditedClientPortParameters, manager: TLCreditedManagerPortParameters, params: Parameters, sourceInfo: SourceInfo) extends FormatEdge
{
  val delay = client.delay + manager.delay
  val bundle = TLBundleParameters(client.base, manager.base)
  def formatEdge = client.infoString + "\n" + manager.infoString
}

case class TLAsyncManagerPortParameters(async: AsyncQueueParams, base: TLSlavePortParameters) {def infoString = base.infoString}
case class TLAsyncClientPortParameters(base: TLMasterPortParameters) {def infoString = base.infoString}
case class TLAsyncBundleParameters(async: AsyncQueueParams, base: TLBundleParameters)
case class TLAsyncEdgeParameters(client: TLAsyncClientPortParameters, manager: TLAsyncManagerPortParameters, params: Parameters, sourceInfo: SourceInfo) extends FormatEdge
{
  val bundle = TLAsyncBundleParameters(manager.async, TLBundleParameters(client.base, manager.base))
  def formatEdge = client.infoString + "\n" + manager.infoString
}

case class TLRationalManagerPortParameters(direction: RationalDirection, base: TLSlavePortParameters) {def infoString = base.infoString}
case class TLRationalClientPortParameters(base: TLMasterPortParameters) {def infoString = base.infoString}

case class TLRationalEdgeParameters(client: TLRationalClientPortParameters, manager: TLRationalManagerPortParameters, params: Parameters, sourceInfo: SourceInfo) extends FormatEdge
{
  val bundle = TLBundleParameters(client.base, manager.base)
  def formatEdge = client.infoString + "\n" + manager.infoString
}

// To be unified, devices must agree on all of these terms
case class ManagerUnificationKey(
  resources:          Seq[Resource],
  regionType:         RegionType.T,
  executable:         Boolean,
  supportsAcquireT:   TransferSizes,
  supportsAcquireB:   TransferSizes,
  supportsArithmetic: TransferSizes,
  supportsLogical:    TransferSizes,
  supportsGet:        TransferSizes,
  supportsPutFull:    TransferSizes,
  supportsPutPartial: TransferSizes,
  supportsHint:       TransferSizes)

object ManagerUnificationKey
{
  def apply(x: TLSlaveParameters): ManagerUnificationKey = ManagerUnificationKey(
    resources          = x.resources,
    regionType         = x.regionType,
    executable         = x.executable,
    supportsAcquireT   = x.supportsAcquireT,
    supportsAcquireB   = x.supportsAcquireB,
    supportsArithmetic = x.supportsArithmetic,
    supportsLogical    = x.supportsLogical,
    supportsGet        = x.supportsGet,
    supportsPutFull    = x.supportsPutFull,
    supportsPutPartial = x.supportsPutPartial,
    supportsHint       = x.supportsHint)
}

object ManagerUnification
{
  def apply(slaves: Seq[TLSlaveParameters]): List[TLSlaveParameters] = {
    slaves.groupBy(ManagerUnificationKey.apply).values.map { seq =>
      val agree = seq.forall(_.fifoId == seq.head.fifoId)
      seq(0).v1copy(
        address = AddressSet.unify(seq.flatMap(_.address)),
        fifoId  = if (agree) seq(0).fifoId else None)
    }.toList
  }
}

case class TLBufferParams(
  a: BufferParams = BufferParams.none,
  b: BufferParams = BufferParams.none,
  c: BufferParams = BufferParams.none,
  d: BufferParams = BufferParams.none,
  e: BufferParams = BufferParams.none
) extends DirectedBuffers[TLBufferParams] {
  def copyIn(x: BufferParams) = this.copy(b = x, d = x)
  def copyOut(x: BufferParams) = this.copy(a = x, c = x, e = x)
  def copyInOut(x: BufferParams) = this.copyIn(x).copyOut(x)
}

/** Pretty printing of TL source id maps */
class TLSourceIdMap(tl: TLMasterPortParameters) extends IdMap[TLSourceIdMapEntry] {
  private val tlDigits = String.valueOf(tl.endSourceId-1).length()
  protected val fmt = s"\t[%${tlDigits}d, %${tlDigits}d) %s%s%s"
  private val sorted = tl.masters.sortBy(_.sourceId)

  val mapping: Seq[TLSourceIdMapEntry] = sorted.map { case c =>
    TLSourceIdMapEntry(c.sourceId, c.name, c.supports.probe, c.requestFifo)
  }
}

case class TLSourceIdMapEntry(tlId: IdRange, name: String, isCache: Boolean, requestFifo: Boolean)
  extends IdMapEntry
{
  val from = tlId
  val to = tlId
  val maxTransactionsInFlight = Some(tlId.size)
}
