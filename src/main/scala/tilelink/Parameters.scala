// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import scala.math.max
import scala.reflect.ClassTag

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
  def cover(rhs: TLMasterToSlaveTransferSizes) = TLMasterToSlaveTransferSizes(
    acquireT   = acquireT  .cover(rhs.acquireT),
    acquireB   = acquireB  .cover(rhs.acquireB),
    arithmetic = arithmetic.cover(rhs.arithmetic),
    logical    = logical   .cover(rhs.logical),
    get        = get       .cover(rhs.get),
    putFull    = putFull   .cover(rhs.putFull),
    putPartial = putPartial.cover(rhs.putPartial),
    hint       = hint      .cover(rhs.hint))
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
  def cover(rhs: TLSlaveToMasterTransferSizes) = TLSlaveToMasterTransferSizes(
    probe      = probe     .cover(rhs.probe),
    arithmetic = arithmetic.cover(rhs.arithmetic),
    logical    = logical   .cover(rhs.logical),
    get        = get       .cover(rhs.get),
    putFull    = putFull   .cover(rhs.putFull),
    putPartial = putPartial.cover(rhs.putPartial),
    hint       = hint      .cover(rhs.hint)
  )
  // Reduce rendering to a simple yes/no per field
  override def toString = {
    def str(x: TransferSizes, flag: String) = if (x.none) "" else flag
    def flags = Vector(
      str(probe,      "T"),
      str(arithmetic, "A"),
      str(logical,    "L"),
      str(get,        "G"),
      str(putFull,    "F"),
      str(putPartial, "P"),
      str(hint,       "H"))
    flags.mkString
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
  setName:                String,
  val address:            Seq[AddressSet],
  val regionType:         RegionType.T,
  val executable:         Boolean,
  val fifoId:             Option[Int],
  val supports:           TLMasterToSlaveTransferSizes,
  val emits:              TLSlaveToMasterTransferSizes,
  // By default, slaves are forbidden from issuing 'denied' responses (it prevents Fragmentation)
  val alwaysGrantsT:      Boolean, // typically only true for CacheCork'd read-write devices; dual: neverReleaseData
  // If fifoId=Some, all accesses sent to the same fifoId are executed and ACK'd in FIFO order
  // Note: you can only rely on this FIFO behaviour if your TLClientParameters include requestFifo
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

  val name = if (setName != "") setName else nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")
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
    s"""Manager Name = ${name}
       |Manager Address = ${address}
       |supportsAcquireT = ${supportsAcquireT}
       |supportsAcquireB = ${supportsAcquireB}
       |supportsArithmetic = ${supportsArithmetic}
       |supportsLogical = ${supportsLogical}
       |supportsGet = ${supportsGet}
       |supportsPutFull = ${supportsPutFull}
       |supportsPutPartial = ${supportsPutPartial}
       |supportsHint = ${supportsHint}
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
      emits           = emits,
      mayDenyGet    = mayDenyGet,
      mayDenyPut    = mayDenyPut,
      alwaysGrantsT = alwaysGrantsT,
      fifoId        = fifoId)
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
      setName       = "",
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

  require (!managers.isEmpty, "Manager ports must have managers")
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
    val relevant = managers.filter(m => policy(m))
    relevant.foreach { m =>
      require(m.fifoId == relevant.head.fifoId, s"${m.name} had fifoId ${m.fifoId}, which was not homogeneous (${managers.map(s => (s.name, s.fifoId))}) ")
    }
  }

  // Bounds on required sizes
  def maxAddress  = slaves.map(_.maxAddress).max
  def maxTransfer = slaves.map(_.maxTransfer).max
  def mayDenyGet  = slaves.exists(_.mayDenyGet)
  def mayDenyPut  = slaves.exists(_.mayDenyPut)

  // Operation sizes supported by all outward Managers
  val allSupports = slaves.map(_.supports).reduce( _ intersect _)
  val allSupportAcquireT   = allSupports.acquireT
  val allSupportAcquireB   = allSupports.acquireB
  val allSupportArithmetic = allSupports.arithmetic
  val allSupportLogical    = allSupports.logical
  val allSupportGet        = allSupports.get
  val allSupportPutFull    = allSupports.putFull
  val allSupportPutPartial = allSupports.putPartial
  val allSupportHint       = allSupports.hint

  // Operation supported by at least one outward Managers
  val anySupports = slaves.map(_.supports).reduce(_ cover _)
  val anySupportAcquireT   = !anySupports.acquireT.none
  val anySupportAcquireB   = !anySupports.acquireB.none
  val anySupportArithmetic = !anySupports.arithmetic.none
  val anySupportLogical    = !anySupports.logical.none
  val anySupportGet        = !anySupports.get.none
  val anySupportPutFull    = !anySupports.putFull.none
  val anySupportPutPartial = !anySupports.putPartial.none
  val anySupportHint       = !anySupports.hint.none

  // Supporting Acquire means being routable for GrantAck
  require ((endSinkId == 0) == !anySupportAcquireB)

  // These return Option[TLManagerParameters] for your convenience
  def find(address: BigInt) = managers.find(_.address.exists(_.contains(address)))

  // The safe version will check the entire address
  def findSafe(address: UInt) = Vec(managers.map(_.address.map(_.contains(address)).reduce(_ || _)))
  // The fast version assumes the address is valid (you probably want fastProperty instead of this function)
  def findFast(address: UInt) = {
    val routingMask = AddressDecoder(managers.map(_.address))
    Vec(managers.map(_.address.map(_.widen(~routingMask)).distinct.map(_.contains(address)).reduce(_ || _)))
  }

  // Compute the simplest AddressSets that decide a key
  def fastPropertyGroup[K](p: TLManagerParameters => K): Seq[(K, Seq[AddressSet])] = {
    val groups = groupByIntoSeq(managers.map(m => (p(m), m.address)))( _._1).map { case (k, vs) =>
      k -> vs.flatMap(_._2)
    }
    val reductionMask = AddressDecoder(groups.map(_._2))
    groups.map { case (k, seq) => k -> AddressSet.unify(seq.map(_.widen(~reductionMask)).distinct) }
  }
  // Select a property
  def fastProperty[K, D <: Data](address: UInt, p: TLManagerParameters => K, d: K => D): D =
    Mux1H(fastPropertyGroup(p).map { case (v, a) => (a.map(_.contains(address)).reduce(_||_), d(v)) })

  // Note: returns the actual fifoId + 1 or 0 if None
  def findFifoIdFast(address: UInt) = fastProperty(address, _.fifoId.map(_+1).getOrElse(0), (i:Int) => UInt(i))
  def hasFifoIdFast(address: UInt) = fastProperty(address, _.fifoId.isDefined, (b:Boolean) => Bool(b))

  // Does this Port manage this ID/address?
  def containsSafe(address: UInt) = findSafe(address).reduce(_ || _)

  private def supportHelper(
      safe:    Boolean,
      member:  TLManagerParameters => TransferSizes,
      address: UInt,
      lgSize:  UInt,
      range:   Option[TransferSizes]): Bool = {
    def trim(x: TransferSizes) = range.map(_.intersect(x)).getOrElse(x)
    // groupBy returns an unordered map, convert back to Seq and sort the result for determinism
    val supportCases = groupByIntoSeq(managers)(m => trim(member(m))).map { case (k, vs) =>
      k -> vs.flatMap(_.address)
    }
    val mask = if (safe) ~BigInt(0) else AddressDecoder(supportCases.map(_._2))
    val simplified = supportCases.map { case (k, seq) => k -> AddressSet.unify(seq.map(_.widen(~mask)).distinct) }
    simplified.map { case (s, a) =>
      (Bool(Some(s) == range) || s.containsLg(lgSize)) &&
      a.map(_.contains(address)).reduce(_||_)
    }.foldLeft(Bool(false))(_||_)
  }

  // Check for support of a given operation at a specific address
  def supportsAcquireTSafe  (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(true, _.supportsAcquireT,   address, lgSize, range)
  def supportsAcquireBSafe  (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(true, _.supportsAcquireB,   address, lgSize, range)
  def supportsArithmeticSafe(address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(true, _.supportsArithmetic, address, lgSize, range)
  def supportsLogicalSafe   (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(true, _.supportsLogical,    address, lgSize, range)
  def supportsGetSafe       (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(true, _.supportsGet,        address, lgSize, range)
  def supportsPutFullSafe   (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(true, _.supportsPutFull,    address, lgSize, range)
  def supportsPutPartialSafe(address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(true, _.supportsPutPartial, address, lgSize, range)
  def supportsHintSafe      (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(true, _.supportsHint,       address, lgSize, range)

  def supportsAcquireTFast  (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(false, _.supportsAcquireT,   address, lgSize, range)
  def supportsAcquireBFast  (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(false, _.supportsAcquireB,   address, lgSize, range)
  def supportsArithmeticFast(address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(false, _.supportsArithmetic, address, lgSize, range)
  def supportsLogicalFast   (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(false, _.supportsLogical,    address, lgSize, range)
  def supportsGetFast       (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(false, _.supportsGet,        address, lgSize, range)
  def supportsPutFullFast   (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(false, _.supportsPutFull,    address, lgSize, range)
  def supportsPutPartialFast(address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(false, _.supportsPutPartial, address, lgSize, range)
  def supportsHintFast      (address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = supportHelper(false, _.supportsHint,       address, lgSize, range)

  def findTreeViolation() = managers.flatMap(_.findTreeViolation()).headOption
  def isTree = !managers.exists(!_.isTree)

  def infoString = "Manager Port Beatbytes = " + beatBytes + "\n" + "Manager Port MinLatency = " + minLatency + "\n\n" + managers.map(_.infoString).mkString

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

  def supportsProbe:       TransferSizes   = supports.probe
  def supportsArithmetic:  TransferSizes   = supports.arithmetic
  def supportsLogical:     TransferSizes   = supports.logical
  def supportsGet:         TransferSizes   = supports.get
  def supportsPutFull:     TransferSizes   = supports.putFull
  def supportsPutPartial:  TransferSizes   = supports.putPartial
  def supportsHint:        TransferSizes   = supports.hint

  require (!sourceId.isEmpty)
  require (!visibility.isEmpty)
  require (supportsPutFull.contains(supportsPutPartial))
  // We only support these operations if we support Probe (ie: we're a cache)
  require (supportsProbe.contains(supportsArithmetic))
  require (supportsProbe.contains(supportsLogical))
  require (supportsProbe.contains(supportsGet))
  require (supportsProbe.contains(supportsPutFull))
  require (supportsProbe.contains(supportsPutPartial))
  require (supportsProbe.contains(supportsHint))

  visibility.combinations(2).foreach { case Seq(x,y) => require (!x.overlaps(y), s"$x and $y overlap.") }

  val maxTransfer = List(
    supportsProbe.max,
    supportsArithmetic.max,
    supportsLogical.max,
    supportsGet.max,
    supportsPutFull.max,
    supportsPutPartial.max).max

  def infoString = {
    s"""Client Name = ${name}
       |visibility = ${visibility}
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

  require (!clients.isEmpty)
  require (minLatency >= 0)

  def clients = masters

  // Require disjoint ranges for Ids
  IdRange.overlaps(clients.map(_.sourceId)).foreach { case (x, y) =>
    require (!x.overlaps(y), s"TLClientParameters.sourceId ${x} overlaps ${y}")
  }

  // Bounds on required sizes
  def endSourceId = clients.map(_.sourceId.end).max
  def maxTransfer = clients.map(_.maxTransfer).max

  // The unused sources < endSourceId
  def unusedSources: Seq[Int] = {
    val usedSources = clients.map(_.sourceId).sortBy(_.start)
    ((Seq(0) ++ usedSources.map(_.end)) zip usedSources.map(_.start)) flatMap { case (end, start) =>
      end until start
    }
  }

  // Operation sizes supported by all inward Clients
  val allSupportProbe      = clients.map(_.supportsProbe)     .reduce(_ intersect _)
  val allSupportArithmetic = clients.map(_.supportsArithmetic).reduce(_ intersect _)
  val allSupportLogical    = clients.map(_.supportsLogical)   .reduce(_ intersect _)
  val allSupportGet        = clients.map(_.supportsGet)       .reduce(_ intersect _)
  val allSupportPutFull    = clients.map(_.supportsPutFull)   .reduce(_ intersect _)
  val allSupportPutPartial = clients.map(_.supportsPutPartial).reduce(_ intersect _)
  val allSupportHint       = clients.map(_.supportsHint)      .reduce(_ intersect _)

  // Operation is supported by at least one client
  val anySupportProbe      = clients.map(!_.supportsProbe.none)     .reduce(_ || _)
  val anySupportArithmetic = clients.map(!_.supportsArithmetic.none).reduce(_ || _)
  val anySupportLogical    = clients.map(!_.supportsLogical.none)   .reduce(_ || _)
  val anySupportGet        = clients.map(!_.supportsGet.none)       .reduce(_ || _)
  val anySupportPutFull    = clients.map(!_.supportsPutFull.none)   .reduce(_ || _)
  val anySupportPutPartial = clients.map(!_.supportsPutPartial.none).reduce(_ || _)
  val anySupportHint       = clients.map(!_.supportsHint.none)      .reduce(_ || _)

  // These return Option[TLClientParameters] for your convenience
  def find(id: Int) = clients.find(_.sourceId.contains(id))

  // Synthesizable lookup methods
  def find(id: UInt) = Vec(clients.map(_.sourceId.contains(id)))
  def contains(id: UInt) = find(id).reduce(_ || _)

  def requestFifo(id: UInt) = Mux1H(find(id), clients.map(c => Bool(c.requestFifo)))

  private def safety_helper(member: TLClientParameters => TransferSizes)(id: UInt, lgSize: UInt) = {
    val allSame = clients.map(member(_) == member(clients(0))).reduce(_ && _)
    if (allSame) member(clients(0)).containsLg(lgSize) else {
      Mux1H(find(id), clients.map(member(_).containsLg(lgSize)))
    }
  }

  // Check for support of a given operation at a specific id
  val supportsProbe      = safety_helper(_.supportsProbe)      _
  val supportsArithmetic = safety_helper(_.supportsArithmetic) _
  val supportsLogical    = safety_helper(_.supportsLogical)    _
  val supportsGet        = safety_helper(_.supportsGet)        _
  val supportsPutFull    = safety_helper(_.supportsPutFull)    _
  val supportsPutPartial = safety_helper(_.supportsPutPartial) _
  val supportsHint       = safety_helper(_.supportsHint)       _

  def infoString = masters.map(_.infoString).mkString

  def v1copy(
    clients: Seq[TLClientParameters] = masters,
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

  @deprecated("Use v1copy instead of copy","")
  def copy(
    clients: Seq[TLClientParameters] = masters,
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
    clients: Seq[TLClientParameters],
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
    clients: Seq[TLClientParameters],
    minLatency: Int = 0,
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

  def apply(client: TLClientPortParameters, manager: TLManagerPortParameters) =
    new TLBundleParameters(
      addressBits = log2Up(manager.maxAddress + 1),
      dataBits    = manager.beatBytes * 8,
      sourceBits  = log2Up(client.endSourceId),
      sinkBits    = log2Up(manager.endSinkId),
      sizeBits    = log2Up(log2Ceil(max(client.maxTransfer, manager.maxTransfer))+1),
      echoFields     = client.echoFields,
      requestFields  = BundleField.accept(client.requestFields, manager.requestKeys),
      responseFields = BundleField.accept(manager.responseFields, client.responseKeys),
      hasBCE = client.anySupportProbe && manager.anySupportAcquireB)
}

case class TLEdgeParameters(
  client:  TLClientPortParameters,
  manager: TLManagerPortParameters,
  params:  Parameters,
  sourceInfo: SourceInfo) extends FormatEdge
{
  val maxTransfer = max(client.maxTransfer, manager.maxTransfer)
  val maxLgSize = log2Ceil(maxTransfer)

  // Sanity check the link...
  require (maxTransfer >= manager.beatBytes, s"Link's max transfer (${maxTransfer}) < ${manager.managers.map(_.name)}'s beatBytes (${manager.beatBytes})")

  val bundle = TLBundleParameters(client, manager)
  def formatEdge = client.infoString + "\n" + manager.infoString
}

case class TLAsyncManagerPortParameters(async: AsyncQueueParams, base: TLManagerPortParameters) {def infoString = base.infoString}
case class TLAsyncClientPortParameters(base: TLClientPortParameters) {def infoString = base.infoString}
case class TLAsyncBundleParameters(async: AsyncQueueParams, base: TLBundleParameters)
case class TLAsyncEdgeParameters(client: TLAsyncClientPortParameters, manager: TLAsyncManagerPortParameters, params: Parameters, sourceInfo: SourceInfo) extends FormatEdge
{
  val bundle = TLAsyncBundleParameters(manager.async, TLBundleParameters(client.base, manager.base))
  def formatEdge = client.infoString + "\n" + manager.infoString
}

case class TLRationalManagerPortParameters(direction: RationalDirection, base: TLManagerPortParameters) {def infoString = base.infoString}
case class TLRationalClientPortParameters(base: TLClientPortParameters) {def infoString = base.infoString}

case class TLRationalEdgeParameters(client: TLRationalClientPortParameters, manager: TLRationalManagerPortParameters, params: Parameters, sourceInfo: SourceInfo) extends FormatEdge
{
  val bundle = TLBundleParameters(client.base, manager.base)
  def formatEdge = client.infoString + "\n" + manager.infoString
}

object ManagerUnification
{
  def apply(managers: Seq[TLManagerParameters]): List[TLManagerParameters] = {
    // To be unified, devices must agree on all of these terms
    case class TLManagerKey(
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
    def key(x: TLManagerParameters) = TLManagerKey(
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
    val map = scala.collection.mutable.HashMap[TLManagerKey, TLManagerParameters]()
    managers.foreach { m =>
      val k = key(m)
      map.get(k) match {
        case None => map.update(k, m)
        case Some(n) => {
          map.update(k, m.v1copy(
            address = m.address ++ n.address,
            fifoId  = None)) // Merging means it's not FIFO anymore!
        }
      }
    }
    map.values.map(m => m.v1copy(address = AddressSet.unify(m.address))).toList
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
class TLSourceIdMap(tl: TLClientPortParameters) {
  private val tlDigits = String.valueOf(tl.endSourceId-1).length()
  private val fmt = s"\t[%${tlDigits}d, %${tlDigits}d) %s%s%s"
  private val sorted = tl.clients.sortWith(TLToAXI4.sortByType)

  val mapping: Seq[TLSourceIdMapEntry] = sorted.map { case c =>
    TLSourceIdMapEntry(c.sourceId, c.name, c.supportsProbe, c.requestFifo)
  }

  def pretty: String = mapping.map(_.pretty(fmt)).mkString(",\n")
}

case class TLSourceIdMapEntry(tlId: IdRange, name: String, isCache: Boolean, requestFifo: Boolean) {
  def pretty(fmt: String): String = fmt.format(
    tlId.start,
    tlId.end,
    s""""$name"""",
    if (isCache) " [CACHE]" else "",
    if (requestFifo) " [FIFO]" else "")
}
