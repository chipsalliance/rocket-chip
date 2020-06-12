// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import scala.math.max
import scala.reflect.ClassTag

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
      str(probe,      "P"),
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

  // Diplomatically determined operation sizes supported by all outward Slaves
  // as opposed to supportsSafe/supportsFast which generate circuitry to check which specific addresses
  val allSupports = slaves.map(_.supports).reduce( _ intersect _)
  val allSupportAcquireT   = allSupports.acquireT
  val allSupportAcquireB   = allSupports.acquireB
  val allSupportArithmetic = allSupports.arithmetic
  val allSupportLogical    = allSupports.logical
  val allSupportGet        = allSupports.get
  val allSupportPutFull    = allSupports.putFull
  val allSupportPutPartial = allSupports.putPartial
  val allSupportHint       = allSupports.hint

  // Operation supported by at least one outward Slaves
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

  // all we need to change about this is adding 2 members, one for the tlmaster parametrs and one for the tlslave parameters
  // All wee need to do is change the membership function to instead of only slave pamaters it looks at the intersection of both master and slave
  // do we need to pass in two different arguments
  private def addressHelper(
      safe:    Boolean,
      member:  TLSlaveParameters => TransferSizes,
      address: UInt,
      lgSize:  UInt,
      range:   Option[TransferSizes]): Bool = { // What does the range argument do?
    // trim is just processing the range argument
    def trim(x: TransferSizes) = range.map(_.intersect(x)).getOrElse(x)
    // groupBy returns an unordered map, convert back to Seq and sort the result for determinism
    // groupByIntoSeq is turning slaves into trimmed membership sizes
    // We are grouping all the slaves by their transfer size where
    // if they support the trimmed size then
    //    
    // member is the type of transfer that you are looking for (What you are trying to filter on)
    // When you consider membership, you are trimming the sizes to only the ones that you care about
    // you are filtering the slaves based on both whether they support a particular opcode and the size
    // Grouping the slaves based on the actual transfer size range they support
    // intersecting the range and checking their membership
    // FOR SUPPORTCASES instead of returning the list of slaves, you are returning a map from transfer size to the set of address sets that are supported for that transfer size

    // find all the slaves that support a certain type of operation and then group their addresses by the supported size
    // for every size there could be multiple address ranges
    // safety is a trade off between checking between all possible addresses vs only the addresses that are known to have supported sizes
    // the trade off is 'checking all addresses is a more expensive circuit but will always give you the right answer even if you give it an illegal address'
    // the not safe version is a cheaper circuit but if you give it an illegal address then it might produce the wrong answer
    // fast presumes address legality
    val supportCases = groupByIntoSeq(slaves)(m => trim(member(m))).map { case (k: TransferSizes, vs: Seq[TLSlaveParameters]) =>
      k -> vs.flatMap(_.address)
    }
    // the rest of this function is just processing that more
    // safe produces a circuit that compares against all possible addresses
    // whereas fast presumes that the address is legal
    // It creates an address decoder that only works if the address is supported
    val mask = if (safe) ~BigInt(0) else AddressDecoder(supportCases.map(_._2))
    // simplified...?
    // simplified is doing some unification with the addresses to combine them with the mask
    val simplified = supportCases.map { case (k, seq) => k -> AddressSet.unify(seq.map(_.widen(~mask)).distinct) }
    simplified.map { case (s, a) => // once you've done that, you are returning
    // s is a size, you are checking for this size either the size of the operation is in s
    // you are checking if the wire address named `address` is contained within the calculated address set
    // you are also checking if the size is contained within the size in the address set pair
    // you are looking at that for every pair and then reducing them
    // as long as you have an address that supports that size
    // thats the circuit you are returning
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
    s"""Master Name = ${name}
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

  // Operation sizes supported by all inward Masters
  val allSupportProbe      = masters.map(_.supportsProbe)     .reduce(_ intersect _)
  val allSupportArithmetic = masters.map(_.supportsArithmetic).reduce(_ intersect _)
  val allSupportLogical    = masters.map(_.supportsLogical)   .reduce(_ intersect _)
  val allSupportGet        = masters.map(_.supportsGet)       .reduce(_ intersect _)
  val allSupportPutFull    = masters.map(_.supportsPutFull)   .reduce(_ intersect _)
  val allSupportPutPartial = masters.map(_.supportsPutPartial).reduce(_ intersect _)
  val allSupportHint       = masters.map(_.supportsHint)      .reduce(_ intersect _)

  // Operation is supported by at least one master
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

  // Available during RTL runtime, checks to see if (id, size) is supported by the master's (master's)  diplomatic parameters
  private def sourceIdHelper(member: TLMasterParameters => TransferSizes)(id: UInt, lgSize: UInt) = {
    val allSame = masters.map(member(_) == member(masters(0))).reduce(_ && _)
    // this if statement is a coarse generalization of the groupBy in the sourceIdHelper2 version;
    // the case where there is only one group.
    if (allSame) member(masters(0)).containsLg(lgSize) else {
      // Find the master associated with ID and returns whether that particular master is able to receive transaction of lgSize
      Mux1H(find(id), masters.map(member(_).containsLg(lgSize)))
    }
  }

  // Check for support of a given operation at a specific id
  val supportsProbeChecker      = sourceIdHelper(_.supportsProbe)      _
  val supportsArithmeticChecker = sourceIdHelper(_.supportsArithmetic) _
  val supportsLogicalChecker    = sourceIdHelper(_.supportsLogical)    _
  val supportsGetChecker        = sourceIdHelper(_.supportsGet)        _
  val supportsPutFullChecker    = sourceIdHelper(_.supportsPutFull)    _
  val supportsPutPartialChecker = sourceIdHelper(_.supportsPutPartial) _
  val supportsHintChecker       = sourceIdHelper(_.supportsHint)       _

  // TODO: Merge sourceIdHelper2 with sourceIdHelper
  private def sourceIdHelper2(
    member: TLMasterParameters => TransferSizes,
    sourceId: UInt,
    lgSize:  UInt,
    range:   Option[TransferSizes]): Bool = {
    def trim(x: TransferSizes) = range.map(_.intersect(x)).getOrElse(x)
    // Because sourceIds are uniquely owned by each master, we use them to group the
    // cases that have to be checked.
    val emitCases = groupByIntoSeq(masters)(m => trim(member(m))).map { case (k, vs) =>
      k -> vs.map(_.sourceId)
    }
    emitCases.map { case (s, a) =>
      (Bool(Some(s) == range) || s.containsLg(lgSize)) &&
      a.map(_.contains(sourceId)).reduce(_||_)
    }.foldLeft(Bool(false))(_||_)
  }

  // Check for emit of a given operation at a specific id
  def emitsAcquireTChecker  (sourceId: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = sourceIdHelper2(_.emits.acquireT,   sourceId, lgSize, range)
  def emitsAcquireBChecker  (sourceId: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = sourceIdHelper2(_.emits.acquireB,   sourceId, lgSize, range)
  def emitsArithmeticChecker(sourceId: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = sourceIdHelper2(_.emits.arithmetic, sourceId, lgSize, range)
  def emitsLogicalChecker   (sourceId: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = sourceIdHelper2(_.emits.logical,    sourceId, lgSize, range)
  def emitsGetChecker       (sourceId: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = sourceIdHelper2(_.emits.get,        sourceId, lgSize, range)
  def emitsPutFullChecker   (sourceId: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = sourceIdHelper2(_.emits.putFull,    sourceId, lgSize, range)
  def emitsPutPartialChecker(sourceId: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = sourceIdHelper2(_.emits.putPartial, sourceId, lgSize, range)
  def emitsHintChecker      (sourceId: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = sourceIdHelper2(_.emits.hint,       sourceId, lgSize, range)

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
    clients: Seq[TLMasterParameters],
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

  // For emits, check that the source is allowed to send this transactions
  // TODO emitAcquire, source => sourceId (or don't need sourceId at all)
  //      Probe, source => address
  //These A channel messages from MasterToSlave are:
  //being routed to a slave based on address bits,
  //    so they need to be passed to a helper that checks whether the slave that owns certain addresses
  //    claimed to support transactions of this size/type
  //being sent from a master using sourceId bits,
  //    so they need to be passed to a helper that checks whether the master that owns certain sourceIds
  //    claimed to emit transactions of this size/type
  def expectsAcquireTMasterToSlaveSafe  (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsAcquireT(sourceId, lgSize, range)    && slave.supportsAcquireTSafe(address, lgSize, range)
  def expectsAcquireBMasterToSlaveSafe  (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsAcquireB(sourceId, lgSize, range)    && slave.supportsAcquireBSafe(address, lgSize, range)
  def expectsArithmeticMasterToSlaveSafe(sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsArithmetic(sourceId, lgSize, range)  && slave.supportsArithmeticSafe(address, lgSize, range)
  def expectsLogicalMasterToSlaveSafe   (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsLogical(sourceId, lgSize, range)     && slave.supportsLogicalSafe(address, lgSize, range)
  def expectsGetMasterToSlaveSafe       (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsGet(sourceId, lgSize, range)         && slave.supportsGetSafe(address, lgSize, range)
  def expectsPutFullMasterToSlaveSafe   (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsPutPartial(sourceId, lgSize, range)  && slave.supportsPutPartialSafe(address, lgSize, range)
  def expectsPutPartialMasterToSlaveSafe(sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsPutPartial(sourceId, lgSize, range)  && slave.supportsPutPartialSafe(address, lgSize, range)
  def expectsHintMasterToSlaveSafe      (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsHint(sourceId, lgSize, range)        && slave.supportsHintSafe(address, lgSize, range)

  def expectsAcquireTMasterToSlaveFast  (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsAcquireT(sourceId, lgSize, range)    && slave.supportsAcquireTFast(address, lgSize, range)
  def expectsAcquireBMasterToSlaveFast  (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsAcquireB(sourceId, lgSize, range)    && slave.supportsAcquireBFast(address, lgSize, range)
  def expectsArithmeticMasterToSlaveFast(sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsArithmetic(sourceId, lgSize, range)  && slave.supportsArithmeticFast(address, lgSize, range)
  def expectsLogicalMasterToSlaveFast   (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsLogical(sourceId, lgSize, range)     && slave.supportsLogicalFast(address, lgSize, range)
  def expectsGetMasterToSlaveFast       (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsGet(sourceId, lgSize, range)         && slave.supportsGetFast(address, lgSize, range)
  def expectsPutFullMasterToSlaveFast   (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsPutFull(sourceId, lgSize, range)     && slave.supportsPutFullFast(address, lgSize, range)
  def expectsPutPartialMasterToSlaveFast(sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsPutPartial(sourceId, lgSize, range)  && slave.supportsPutPartialFast(address, lgSize, range)
  def expectsHintMasterToSlaveFast      (sourceId: UInt, address: UInt, lgSize: UInt, range: Option[TransferSizes] = None) = master.emitsHint(sourceId, lgSize, range)        && slave.supportsHintFast(address, lgSize, range)

  //We don't use Safe vs Fast because we don't have the equivalent of address decoder for sourceIds
  //TODO (can use sourceId here to see if the specific master can support this transaction)
  //Duality: these B channel messages from SlaveToMaster are:
  //being routed to a master based on sourceId bits,
  //    so they need to be passed to a helper that checks whether the master that owns certain sourceIds
  //    claimed to support transactions of this size/type
  //being sent from a slave owning certain addresses,
  //    so they need to be passed to a helper that checks with the slave that owns certain addresses claimed
  //    to emit transactions of this size/type
  def expectsProbeSlaveToMaster      = master.supportsProbe
  def expectsArithmeticSlaveToMaster = master.supportsArithmetic
  def expectsLogicalSlaveToMaster    = master.supportsLogical
  def expectsGetSlaveToMaster        = master.supportsGet
  def expectsPutFullSlaveToMaster    = master.supportsPutFull
  def expectsPutPartialSlaveToMaster = master.supportsPutPartial
  def expectsHintSlaveToMaster       = master.supportsHint

  val bundle = TLBundleParameters(master, slave)
  def formatEdge = master.infoString + "\n" + slave.infoString
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
class TLSourceIdMap(tl: TLMasterPortParameters) {
  private val tlDigits = String.valueOf(tl.endSourceId-1).length()
  private val fmt = s"\t[%${tlDigits}d, %${tlDigits}d) %s%s%s"
  private val sorted = tl.masters.sortWith(TLToAXI4.sortByType)

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
