// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import diplomacy._
import scala.math.max
import util.RationalDirection

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
  // If fifoId=Some, all accesses sent to the same fifoId are executed and ACK'd in FIFO order
  fifoId:             Option[Int]   = None)
{
  require (!address.isEmpty)
  address.foreach { a => require (a.finite) }

  address.combinations(2).foreach { case Seq(x,y) => require (!x.overlaps(y), s"$x and $y overlap.") }
  require (supportsPutFull.contains(supportsPutPartial))
  require (supportsPutFull.contains(supportsArithmetic))
  require (supportsPutFull.contains(supportsLogical))
  require (supportsGet.contains(supportsArithmetic))
  require (supportsGet.contains(supportsLogical))
  require (supportsAcquireB.contains(supportsAcquireT))

  // Make sure that the regionType agrees with the capabilities
  require ((regionType == RegionType.CACHED || regionType == RegionType.TRACKED) != supportsAcquireB.none)
  require (regionType != RegionType.UNCACHED || supportsGet)

  // Largest support transfer of all types
  val maxTransfer = List(
    supportsAcquireT.max,
    supportsAcquireB.max,
    supportsArithmetic.max,
    supportsLogical.max,
    supportsGet.max,
    supportsPutFull.max,
    supportsPutPartial.max).max
  val maxAddress = address.map(_.max).max

  val name = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")

  // The device had better not support a transfer larger than it's alignment
  val minAlignment = address.map(_.alignment).min
  require (minAlignment >= maxTransfer)

  def toResource: ResourceAddress = {
    ResourceAddress(address,
      r = supportsAcquireB || supportsGet,
      w = supportsAcquireT || supportsPutFull,
      x = executable)
  }
}

case class TLManagerPortParameters(
  managers:   Seq[TLManagerParameters],
  beatBytes:  Int,
  endSinkId:  Int = 1,
  minLatency: Int = 0)
{
  require (!managers.isEmpty)
  require (isPow2(beatBytes))
  require (endSinkId > 0)
  require (minLatency >= 0)

  // Bounds on required sizes
  def maxAddress  = managers.map(_.maxAddress).max
  def maxTransfer = managers.map(_.maxTransfer).max
  
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

  // Which bits suffice to distinguish between all managers
  lazy val routingMask = AddressDecoder(managers.map(_.address))

  // These return Option[TLManagerParameters] for your convenience
  def find(address: BigInt) = managers.find(_.address.exists(_.contains(address)))

  // The safe version will check the entire address
  def findSafe(address: UInt) = Vec(managers.map(_.address.map(_.contains(address)).reduce(_ || _)))
  // The fast version assumes the address is valid
  def findFast(address: UInt) = Vec(managers.map(_.address.map(_.widen(~routingMask)).distinct.map(_.contains(address)).reduce(_ || _)))

  // Note: returns the actual fifoId + 1 or 0 if None
  def findFifoIdFast(address: UInt) = Mux1H(findFast(address), managers.map(m => UInt(m.fifoId.map(_+1).getOrElse(0))))
  def hasFifoIdFast(address: UInt) = Mux1H(findFast(address), managers.map(m => Bool(m.fifoId.isDefined)))

  // Does this Port manage this ID/address?
  def containsSafe(address: UInt) = findSafe(address).reduce(_ || _)

  private def safe_helper(member: TLManagerParameters => TransferSizes)(address: UInt, lgSize: UInt) = {
    val allSame = managers.map(member(_) == member(managers(0))).reduce(_ && _)
    if (allSame) containsSafe(address) && member(managers(0)).containsLg(lgSize) else {
      Mux1H(findSafe(address), managers.map(member(_).containsLg(lgSize)))
    }
  }
  private def fast_helper(member: TLManagerParameters => TransferSizes)(address: UInt, lgSize: UInt) = {
    val allSame = managers.map(member(_) == member(managers(0))).reduce(_ && _)
    if (allSame) member(managers(0)).containsLg(lgSize) else {
      Mux1H(findFast(address), managers.map(member(_).containsLg(lgSize)))
    }
  }

  // Check for support of a given operation at a specific address
  val supportsAcquireTSafe   = safe_helper(_.supportsAcquireT) _
  val supportsAcquireBSafe   = safe_helper(_.supportsAcquireB) _
  val supportsArithmeticSafe = safe_helper(_.supportsArithmetic) _
  val supportsLogicalSafe    = safe_helper(_.supportsLogical) _
  val supportsGetSafe        = safe_helper(_.supportsGet) _
  val supportsPutFullSafe    = safe_helper(_.supportsPutFull) _
  val supportsPutPartialSafe = safe_helper(_.supportsPutPartial) _
  val supportsHintSafe       = safe_helper(_.supportsHint) _

  val supportsAcquireTFast   = fast_helper(_.supportsAcquireT) _
  val supportsAcquireBFast   = fast_helper(_.supportsAcquireB) _
  val supportsArithmeticFast = fast_helper(_.supportsArithmetic) _
  val supportsLogicalFast    = fast_helper(_.supportsLogical) _
  val supportsGetFast        = fast_helper(_.supportsGet) _
  val supportsPutFullFast    = fast_helper(_.supportsPutFull) _
  val supportsPutPartialFast = fast_helper(_.supportsPutPartial) _
  val supportsHintFast       = fast_helper(_.supportsHint) _
}

case class TLClientParameters(
  sourceId:            IdRange       = IdRange(0,1),
  nodePath:            Seq[BaseNode] = Seq(),
  // Supports both Probe+Grant of these sizes
  supportsProbe:       TransferSizes = TransferSizes.none,
  supportsArithmetic:  TransferSizes = TransferSizes.none,
  supportsLogical:     TransferSizes = TransferSizes.none,
  supportsGet:         TransferSizes = TransferSizes.none,
  supportsPutFull:     TransferSizes = TransferSizes.none,
  supportsPutPartial:  TransferSizes = TransferSizes.none,
  supportsHint:        TransferSizes = TransferSizes.none)
{
  require (supportsPutFull.contains(supportsPutPartial))
  // We only support these operations if we support Probe (ie: we're a cache)
  require (supportsProbe.contains(supportsArithmetic))
  require (supportsProbe.contains(supportsLogical))
  require (supportsProbe.contains(supportsGet))
  require (supportsProbe.contains(supportsPutFull))
  require (supportsProbe.contains(supportsPutPartial))
  require (supportsProbe.contains(supportsHint))

  val maxTransfer = List(
    supportsProbe.max,
    supportsArithmetic.max,
    supportsLogical.max,
    supportsGet.max,
    supportsPutFull.max,
    supportsPutPartial.max).max

  val name = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")
}

case class TLClientPortParameters(
  clients:       Seq[TLClientParameters],
  unsafeAtomics: Boolean = false,
  minLatency:    Int = 0) // Atomics are executed as get+put
{
  require (!clients.isEmpty)
  require (minLatency >= 0)

  // Require disjoint ranges for Ids
  clients.combinations(2).foreach({ case Seq(x,y) =>
    require (!x.sourceId.overlaps(y.sourceId))
  })

  // Bounds on required sizes
  def endSourceId = clients.map(_.sourceId.end).max
  def maxTransfer = clients.map(_.maxTransfer).max

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
}

case class TLBundleParameters(
  addressBits: Int,
  dataBits:    Int,
  sourceBits:  Int,
  sinkBits:    Int,
  sizeBits:    Int)
{
  // Chisel has issues with 0-width wires
  require (addressBits >= 1)
  require (dataBits    >= 8)
  require (sourceBits  >= 1)
  require (sinkBits    >= 1)
  require (sizeBits    >= 1)
  require (isPow2(dataBits))

  val addrLoBits = log2Up(dataBits/8)

  def union(x: TLBundleParameters) =
    TLBundleParameters(
      max(addressBits, x.addressBits),
      max(dataBits,    x.dataBits),
      max(sourceBits,  x.sourceBits),
      max(sinkBits,    x.sinkBits),
      max(sizeBits,    x.sizeBits))
}

object TLBundleParameters
{
  val emptyBundleParams = TLBundleParameters(
    addressBits = 1,
    dataBits    = 8,
    sourceBits  = 1,
    sinkBits    = 1,
    sizeBits    = 1)

  def union(x: Seq[TLBundleParameters]) = x.foldLeft(emptyBundleParams)((x,y) => x.union(y))

  def apply(client: TLClientPortParameters, manager: TLManagerPortParameters) =
    new TLBundleParameters(
      addressBits = log2Up(manager.maxAddress + 1),
      dataBits    = manager.beatBytes * 8,
      sourceBits  = log2Up(client.endSourceId),
      sinkBits    = log2Up(manager.endSinkId),
      sizeBits    = log2Up(log2Ceil(max(client.maxTransfer, manager.maxTransfer))+1))
}

case class TLEdgeParameters(
  client:  TLClientPortParameters,
  manager: TLManagerPortParameters)
{
  val maxTransfer = max(client.maxTransfer, manager.maxTransfer)
  val maxLgSize = log2Ceil(maxTransfer)

  // Sanity check the link...
  require (maxTransfer >= manager.beatBytes)

  val bundle = TLBundleParameters(client, manager)
}

case class TLAsyncManagerPortParameters(depth: Int, base: TLManagerPortParameters) { require (isPow2(depth)) }
case class TLAsyncClientPortParameters(base: TLClientPortParameters)

case class TLAsyncBundleParameters(depth: Int, base: TLBundleParameters)
{
  require (isPow2(depth))
  def union(x: TLAsyncBundleParameters) = TLAsyncBundleParameters(
    depth = max(depth, x.depth),
    base  = base.union(x.base))
}

object TLAsyncBundleParameters
{
  val emptyBundleParams = TLAsyncBundleParameters(depth = 1, base = TLBundleParameters.emptyBundleParams)
  def union(x: Seq[TLAsyncBundleParameters]) = x.foldLeft(emptyBundleParams)((x,y) => x.union(y))
}

case class TLAsyncEdgeParameters(client: TLAsyncClientPortParameters, manager: TLAsyncManagerPortParameters)
{
  val bundle = TLAsyncBundleParameters(manager.depth, TLBundleParameters(client.base, manager.base))
}

case class TLRationalManagerPortParameters(direction: RationalDirection, base: TLManagerPortParameters)
case class TLRationalClientPortParameters(base: TLClientPortParameters)

case class TLRationalEdgeParameters(client: TLRationalClientPortParameters, manager: TLRationalManagerPortParameters)
{
  val bundle = TLBundleParameters(client.base, manager.base)
}

object ManagerUnification
{
  def apply(managers: Seq[TLManagerParameters]) = {
    // To be unified, devices must agree on all of these terms
    case class TLManagerKey(
      regionType:         RegionType.T,
      executable:         Boolean,
      lastNode:           BaseNode,
      supportsAcquireT:   TransferSizes,
      supportsAcquireB:   TransferSizes,
      supportsArithmetic: TransferSizes,
      supportsLogical:    TransferSizes,
      supportsGet:        TransferSizes,
      supportsPutFull:    TransferSizes,
      supportsPutPartial: TransferSizes,
      supportsHint:       TransferSizes)
    def key(x: TLManagerParameters) = TLManagerKey(
      regionType         = x.regionType,
      executable         = x.executable,
      lastNode           = x.nodePath.last,
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
