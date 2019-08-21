// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.{RationalDirection,AsyncQueueParams, groupByIntoSeq}
import scala.math.max
import scala.reflect.ClassTag

case class TLManagerParameters(
  address:            Seq[AddressSet],
  resources:          Seq[Resource] = Seq(),
  regionType:         RegionType.T  = RegionType.GET_EFFECTS,
  executable:         Boolean       = false, // processor can execute from this memory
  nodePath:           Seq[BaseNode] = Seq(),
  // Supports both Acquire+Release+Finish of these sizes
  supportsAcquireT:   TransferSizes = TransferSizes.none,
  supportsAcquireB:   TransferSizes = TransferSizes.none,
  supportsArithmetic: TransferSizes = TransferSizes.none,
  supportsLogical:    TransferSizes = TransferSizes.none,
  supportsGet:        TransferSizes = TransferSizes.none,
  supportsPutFull:    TransferSizes = TransferSizes.none,
  supportsPutPartial: TransferSizes = TransferSizes.none,
  supportsHint:       TransferSizes = TransferSizes.none,
  userBits:           Seq[UserBits] = Nil,
  // By default, slaves are forbidden from issuing 'denied' responses (it prevents Fragmentation)
  mayDenyGet:         Boolean = false, // applies to: AccessAckData, GrantData
  mayDenyPut:         Boolean = false, // applies to: AccessAck,     Grant,    HintAck
                                       // ReleaseAck may NEVER be denied
  alwaysGrantsT:      Boolean = false, // typically only true for CacheCork'd read-write devices
  // If fifoId=Some, all accesses sent to the same fifoId are executed and ACK'd in FIFO order
  // Note: you can only rely on this FIFO behaviour if your TLClientParameters include requestFifo
  fifoId:             Option[Int] = None,
  device: Option[Device] = None)
{
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

  val name = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")
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

  def getUser[T <: UserBits : ClassTag](x: UInt): Seq[UserBitField[T]] = UserBits.extract[T](userBits, x)
  def putUser[T <: UserBits : ClassTag](x: UInt, seq: Seq[UInt]): UInt = UserBits.inject[T](userBits, x, seq)
  val userBitWidth = userBits.map(_.width).sum
}

case class TLManagerPortParameters(
  managers:   Seq[TLManagerParameters],
  beatBytes:  Int,
  endSinkId:  Int = 0,
  minLatency: Int = 0)
{
  require (!managers.isEmpty, "Manager ports must have managers")
  require (isPow2(beatBytes), "Data channel width must be a power of 2")
  require (endSinkId >= 0, "Sink ids cannot be negative")
  require (minLatency >= 0, "Minimum required latency cannot be negative")

  def requireFifo() = managers.foreach { m =>
    require(m.fifoId.isDefined && m.fifoId == managers.head.fifoId, s"${m.name} had fifoId ${m.fifoId}, which was not homogeneous (${managers.map(s => (s.name, s.fifoId))}) ")
  }

  // Bounds on required sizes
  def maxAddress  = managers.map(_.maxAddress).max
  def maxTransfer = managers.map(_.maxTransfer).max
  def mayDenyGet = managers.exists(_.mayDenyGet)
  def mayDenyPut = managers.exists(_.mayDenyPut)

  // Operation sizes supported by all outward Managers
  val allSupportAcquireT   = managers.map(_.supportsAcquireT)  .reduce(_ intersect _)
  val allSupportAcquireB   = managers.map(_.supportsAcquireB)  .reduce(_ intersect _)
  val allSupportArithmetic = managers.map(_.supportsArithmetic).reduce(_ intersect _)
  val allSupportLogical    = managers.map(_.supportsLogical)   .reduce(_ intersect _)
  val allSupportGet        = managers.map(_.supportsGet)       .reduce(_ intersect _)
  val allSupportPutFull    = managers.map(_.supportsPutFull)   .reduce(_ intersect _)
  val allSupportPutPartial = managers.map(_.supportsPutPartial).reduce(_ intersect _)
  val allSupportHint       = managers.map(_.supportsHint)      .reduce(_ intersect _)

  // Operation supported by at least one outward Managers
  val anySupportAcquireT   = managers.map(!_.supportsAcquireT.none)  .reduce(_ || _)
  val anySupportAcquireB   = managers.map(!_.supportsAcquireB.none)  .reduce(_ || _)
  val anySupportArithmetic = managers.map(!_.supportsArithmetic.none).reduce(_ || _)
  val anySupportLogical    = managers.map(!_.supportsLogical.none)   .reduce(_ || _)
  val anySupportGet        = managers.map(!_.supportsGet.none)       .reduce(_ || _)
  val anySupportPutFull    = managers.map(!_.supportsPutFull.none)   .reduce(_ || _)
  val anySupportPutPartial = managers.map(!_.supportsPutPartial.none).reduce(_ || _)
  val anySupportHint       = managers.map(!_.supportsHint.none)      .reduce(_ || _)

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

  // add some user bits to the same highest offset for every manager
  val userBitWidth = managers.map(_.userBitWidth).max
  def addUser[T <: UserBits](userBits: T): TLManagerPortParameters = {
    this.copy(managers = managers.map { m =>
      val extra = if (m.userBitWidth == userBitWidth) {
        Seq(userBits)
      } else {
        Seq(PadUserBits(userBitWidth - m.userBitWidth), userBits)
      }
      m.copy(userBits = m.userBits ++ extra)
    })
  }
}

case class TLClientParameters(
  name:                String,
  sourceId:            IdRange         = IdRange(0,1),
  nodePath:            Seq[BaseNode]   = Seq(),
  requestFifo:         Boolean         = false, // only a request, not a requirement. applies to A, not C.
  visibility:          Seq[AddressSet] = Seq(AddressSet(0, ~0)), // everything
  // Supports both Probe+Grant of these sizes
  supportsProbe:       TransferSizes   = TransferSizes.none,
  supportsArithmetic:  TransferSizes   = TransferSizes.none,
  supportsLogical:     TransferSizes   = TransferSizes.none,
  supportsGet:         TransferSizes   = TransferSizes.none,
  supportsPutFull:     TransferSizes   = TransferSizes.none,
  supportsPutPartial:  TransferSizes   = TransferSizes.none,
  supportsHint:        TransferSizes   = TransferSizes.none,
  userBits:            Seq[UserBits]   = Nil)
{
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

  def getUser[T <: UserBits : ClassTag](x: UInt): Seq[UserBitField[T]] = UserBits.extract[T](userBits, x)
  def putUser[T <: UserBits : ClassTag](x: UInt, seq: Seq[UInt]): UInt = UserBits.inject[T](userBits, x, seq)
  val userBitWidth = userBits.map(_.width).sum
}

case class TLClientPortParameters(
  clients:    Seq[TLClientParameters],
  minLatency: Int = 0) // Only applies to B=>C
{
  require (!clients.isEmpty)
  require (minLatency >= 0)

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

  // add some user bits to the same highest offset for every client
  val userBitWidth = clients.map(_.userBitWidth).max
  def addUser[T <: UserBits](userBits: T): TLClientPortParameters = {
    this.copy(clients = clients.map { c =>
      val extra = if (c.userBitWidth == userBitWidth) {
        Seq(userBits)
      } else {
        Seq(PadUserBits(userBitWidth - c.userBitWidth), userBits)
      }
      c.copy(userBits = c.userBits ++ extra)
    })
  }
}

case class TLBundleParameters(
  addressBits: Int,
  dataBits:    Int,
  sourceBits:  Int,
  sinkBits:    Int,
  sizeBits:    Int,
  aUserBits:   Int,
  dUserBits:   Int,
  hasBCE:      Boolean)
{
  // Chisel has issues with 0-width wires
  require (addressBits >= 1)
  require (dataBits    >= 8)
  require (sourceBits  >= 1)
  require (sinkBits    >= 1)
  require (sizeBits    >= 1)
  require (aUserBits   >= 0)
  require (dUserBits   >= 0)
  require (isPow2(dataBits))

  val addrLoBits = log2Up(dataBits/8)

  def union(x: TLBundleParameters) =
    TLBundleParameters(
      max(addressBits, x.addressBits),
      max(dataBits,    x.dataBits),
      max(sourceBits,  x.sourceBits),
      max(sinkBits,    x.sinkBits),
      max(sizeBits,    x.sizeBits),
      max(aUserBits,   x.aUserBits),
      max(dUserBits,   x.dUserBits),
      hasBCE ||        x.hasBCE)
}

object TLBundleParameters
{
  val emptyBundleParams = TLBundleParameters(
    addressBits = 1,
    dataBits    = 8,
    sourceBits  = 1,
    sinkBits    = 1,
    sizeBits    = 1,
    aUserBits   = 0,
    dUserBits   = 0,
    hasBCE      = false)

  def union(x: Seq[TLBundleParameters]) = x.foldLeft(emptyBundleParams)((x,y) => x.union(y))

  def apply(client: TLClientPortParameters, manager: TLManagerPortParameters) =
    new TLBundleParameters(
      addressBits = log2Up(manager.maxAddress + 1),
      dataBits    = manager.beatBytes * 8,
      sourceBits  = log2Up(client.endSourceId),
      sinkBits    = log2Up(manager.endSinkId),
      sizeBits    = log2Up(log2Ceil(max(client.maxTransfer, manager.maxTransfer))+1),
      aUserBits   = client .userBitWidth,
      dUserBits   = manager.userBitWidth,
      hasBCE      = client.anySupportProbe && manager.anySupportAcquireB)
}

case class TLEdgeParameters(
  client:  TLClientPortParameters,
  manager: TLManagerPortParameters,
  params:  Parameters,
  sourceInfo: SourceInfo)
{
  val maxTransfer = max(client.maxTransfer, manager.maxTransfer)
  val maxLgSize = log2Ceil(maxTransfer)

  // Sanity check the link...
  require (maxTransfer >= manager.beatBytes, s"Link's max transfer (${maxTransfer}) < ${manager.managers.map(_.name)}'s beatBytes (${manager.beatBytes})")

  val bundle = TLBundleParameters(client, manager)
}

case class TLAsyncManagerPortParameters(async: AsyncQueueParams, base: TLManagerPortParameters)
case class TLAsyncClientPortParameters(base: TLClientPortParameters)
case class TLAsyncBundleParameters(async: AsyncQueueParams, base: TLBundleParameters)
case class TLAsyncEdgeParameters(client: TLAsyncClientPortParameters, manager: TLAsyncManagerPortParameters, params: Parameters, sourceInfo: SourceInfo)
{
  val bundle = TLAsyncBundleParameters(manager.async, TLBundleParameters(client.base, manager.base))
}

case class TLRationalManagerPortParameters(direction: RationalDirection, base: TLManagerPortParameters)
case class TLRationalClientPortParameters(base: TLClientPortParameters)

case class TLRationalEdgeParameters(client: TLRationalClientPortParameters, manager: TLRationalManagerPortParameters, params: Parameters, sourceInfo: SourceInfo)
{
  val bundle = TLBundleParameters(client.base, manager.base)
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
          map.update(k, m.copy(
            address = m.address ++ n.address,
            fifoId  = None)) // Merging means it's not FIFO anymore!
        }
      }
    }
    map.values.map(m => m.copy(address = AddressSet.unify(m.address))).toList
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
