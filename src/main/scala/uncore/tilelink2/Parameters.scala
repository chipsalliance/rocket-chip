// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import scala.math.max

/** Options for memory regions */
object RegionType {
  sealed trait T
  case object CACHED      extends T
  case object TRACKED     extends T
  case object UNCACHED    extends T
  case object PUT_EFFECTS extends T
  case object GET_EFFECTS extends T // GET_EFFECTS => PUT_EFFECTS
  val cases = Seq(CACHED, TRACKED, UNCACHED, PUT_EFFECTS, GET_EFFECTS)
}

// A non-empty half-open range; [start, end)
case class IdRange(start: Int, end: Int)
{
  require (start >= 0)
  require (start < end) // not empty

  // This is a strict partial ordering
  def <(x: IdRange) = end <= x.start
  def >(x: IdRange) = x < this

  def overlaps(x: IdRange) = start < x.end && x.start < end
  def contains(x: IdRange) = start <= x.start && x.end <= end
  // contains => overlaps (because empty is forbidden)

  def contains(x: Int)  = start <= x && x < end
  def contains(x: UInt) =
    if (start+1 == end) { UInt(start) === x }
    else if (isPow2(end-start) && ((end | start) & (end-start-1)) == 0)
    { ~(~(UInt(start) ^ x) | UInt(end-start-1)) === UInt(0) }
    else { UInt(start) <= x && x < UInt(end) }

  def shift(x: Int) = IdRange(start+x, end+x)
  def size = end - start
}

// An potentially empty inclusive range of 2-powers [min, max] (in bytes)
case class TransferSizes(min: Int, max: Int)
{
  def this(x: Int) = this(x, x)

  require (min <= max)
  require (min >= 0 && max >= 0)
  require (max == 0 || isPow2(max))
  require (min == 0 || isPow2(min))
  require (max == 0 || min != 0) // 0 is forbidden unless (0,0)

  def none = min == 0
  def contains(x: Int) = isPow2(x) && min <= x && x <= max
  def containsLg(x: Int) = contains(1 << x)
  def containsLg(x: UInt) =
    if (none) Bool(false)
    else if (min == max) { UInt(log2Ceil(min)) === x }
    else { UInt(log2Ceil(min)) <= x && x <= UInt(log2Ceil(max)) }

  def contains(x: TransferSizes) = x.none || (min <= x.min && x.max <= max)
  
  def intersect(x: TransferSizes) =
    if (x.max < min || min < x.max) TransferSizes.none
    else TransferSizes(scala.math.max(min, x.min), scala.math.min(max, x.max))
}

object TransferSizes {
  def apply(x: Int) = new TransferSizes(x)
  val none = new TransferSizes(0)

  implicit def asBool(x: TransferSizes) = !x.none
}

// AddressSets specify the address space managed by the manager
// Base is the base address, and mask are the bits consumed by the manager
// e.g: base=0x200, mask=0xff describes a device managing 0x200-0x2ff
// e.g: base=0x1000, mask=0xf0f decribes a device managing 0x1000-0x100f, 0x1100-0x110f, ...
case class AddressSet(base: BigInt, mask: BigInt)
{
  // Forbid misaligned base address (and empty sets)
  require ((base & mask) == 0)

  def contains(x: BigInt) = ~(~(x ^ base) | mask) == 0
  def contains(x: UInt) = ~(~(x ^ UInt(base)) | UInt(mask)) === UInt(0)

  // overlap iff bitwise: both care (~mask0 & ~mask1) => both equal (base0=base1)
  def overlaps(x: AddressSet) = (~(mask | x.mask) & (base ^ x.base)) == 0

  // contains iff bitwise: x.mask => mask && contains(x.base)
  def contains(x: AddressSet) = ((x.mask | (base ^ x.base)) & ~mask) == 0
  // 1 less than the number of bytes to which the manager should be aligned
  def alignment1 = ((mask + 1) & ~mask) - 1
  def max = base | mask

  // A strided slave serves discontiguous ranges
  def strided = alignment1 != mask
}

case class TLManagerParameters(
  address:            Seq[AddressSet],
  sinkId:             IdRange       = IdRange(0, 1),
  regionType:         RegionType.T  = RegionType.GET_EFFECTS,
  // Supports both Acquire+Release+Finish of these sizes
  supportsAcquire:    TransferSizes = TransferSizes.none,
  supportsArithmetic: TransferSizes = TransferSizes.none,
  supportsLogical:    TransferSizes = TransferSizes.none,
  supportsGet:        TransferSizes = TransferSizes.none,
  supportsPutFull:    TransferSizes = TransferSizes.none,
  supportsPutPartial: TransferSizes = TransferSizes.none,
  supportsHint:       Boolean       = false,
  // If fifoId=Some, all accesses sent to the same fifoId are executed and ACK'd in FIFO order
  fifoId:             Option[Int]   = None)
{
  address.combinations(2).foreach({ case Seq(x,y) =>
    require (!x.overlaps(y))
  })
  require (supportsPutFull.contains(supportsPutPartial))

  // Largest support transfer of all types
  val maxTransfer = List(
    supportsAcquire.max,
    supportsArithmetic.max,
    supportsLogical.max,
    supportsGet.max,
    supportsPutFull.max,
    supportsPutPartial.max).max

  // The device had better not support a transfer larger than it's alignment
  address.foreach({ case a =>
    require (a.alignment1 >= maxTransfer-1)
  })
}

case class TLManagerPortParameters(managers: Seq[TLManagerParameters], beatBytes: Int)
{
  require (!managers.isEmpty)
  require (isPow2(beatBytes))

  // Require disjoint ranges for Ids and addresses
  managers.combinations(2).foreach({ case Seq(x,y) =>
    require (!x.sinkId.overlaps(y.sinkId))
    x.address.foreach({ a => y.address.foreach({ b =>
      require (!a.overlaps(b))
    })})
  })

  // Bounds on required sizes
  def endSinkId   = managers.map(_.sinkId.end).max
  def maxAddress  = managers.map(_.address.map(_.max).max).max
  def maxTransfer = managers.map(_.maxTransfer).max
  
  // Operation sizes supported by all outward Managers
  val allSupportAcquire    = managers.map(_.supportsAcquire)   .reduce(_ intersect _)
  val allSupportArithmetic = managers.map(_.supportsArithmetic).reduce(_ intersect _)
  val allSupportLogical    = managers.map(_.supportsLogical)   .reduce(_ intersect _)
  val allSupportGet        = managers.map(_.supportsGet)       .reduce(_ intersect _)
  val allSupportPutFull    = managers.map(_.supportsPutFull)   .reduce(_ intersect _)
  val allSupportPutPartial = managers.map(_.supportsPutPartial).reduce(_ intersect _)
  val allSupportHint       = managers.map(_.supportsHint)      .reduce(_    &&     _)

  // Operation supported by at least one outward Managers
  val anySupportAcquire    = managers.map(!_.supportsAcquire.none)   .reduce(_ || _)
  val anySupportArithmetic = managers.map(!_.supportsArithmetic.none).reduce(_ || _)
  val anySupportLogical    = managers.map(!_.supportsLogical.none)   .reduce(_ || _)
  val anySupportGet        = managers.map(!_.supportsGet.none)       .reduce(_ || _)
  val anySupportPutFull    = managers.map(!_.supportsPutFull.none)   .reduce(_ || _)
  val anySupportPutPartial = managers.map(!_.supportsPutPartial.none).reduce(_ || _)
  val anySupportHint       = managers.map( _.supportsHint)           .reduce(_ || _)

  // These return Option[TLManagerParameters] for your convenience
  def find(address: BigInt) = managers.find(_.address.exists(_.contains(address)))
  def findById(id: Int) = managers.find(_.sinkId.contains(id))

  // Synthesizable lookup methods
  def find(address: UInt) = Vec(managers.map(_.address.map(_.contains(address)).reduce(_ || _)))
  def findById(id: UInt) = Vec(managers.map(_.sinkId.contains(id)))

  // !!! need a cheaper version of find, where we assume a valid address match exists
  
  // Does this Port manage this ID/address?
  def contains(address: UInt) = find(address).reduce(_ || _)
  def containsById(id: UInt) = findById(id).reduce(_ || _)
  
  private def safety_helper(member: TLManagerParameters => TransferSizes)(address: UInt, lgSize: UInt) = {
    val allSame = managers.map(member(_) == member(managers(0))).reduce(_ && _)
    if (allSame) member(managers(0)).containsLg(lgSize) else {
      Mux1H(find(address), managers.map(member(_).containsLg(lgSize)))
    }
  }

  // Check for support of a given operation at a specific address
  val supportsAcquire    = safety_helper(_.supportsAcquire)    _
  val supportsArithmetic = safety_helper(_.supportsArithmetic) _
  val supportsLogical    = safety_helper(_.supportsLogical)    _
  val supportsGet        = safety_helper(_.supportsGet)        _
  val supportsPutFull    = safety_helper(_.supportsPutFull)    _
  val supportsPutPartial = safety_helper(_.supportsPutPartial) _
  def supportsHint(address: UInt) = {
    if (allSupportHint) Bool(true) else {
      Mux1H(find(address), managers.map(m => Bool(m.supportsHint)))
    }
  }
}

case class TLClientParameters(
  sourceId:            IdRange       = IdRange(0,1),
  // Supports both Probe+Grant of these sizes
  supportsProbe:       TransferSizes = TransferSizes.none,
  supportsArithmetic:  TransferSizes = TransferSizes.none,
  supportsLogical:     TransferSizes = TransferSizes.none,
  supportsGet:         TransferSizes = TransferSizes.none,
  supportsPutFull:     TransferSizes = TransferSizes.none,
  supportsPutPartial:  TransferSizes = TransferSizes.none,
  supportsHint:        Boolean       = false)
{
  require (supportsPutFull.contains(supportsPutPartial))

  val maxTransfer = List(
    supportsProbe.max,
    supportsArithmetic.max,
    supportsLogical.max,
    supportsGet.max,
    supportsPutFull.max,
    supportsPutPartial.max).max
}

case class TLClientPortParameters(clients: Seq[TLClientParameters]) {
  require (!clients.isEmpty)

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
  val allSupportHint       = clients.map(_.supportsHint)      .reduce(_    &&     _)

  // Operation is supported by at least one client
  val anySupportProbe      = clients.map(!_.supportsProbe.none)     .reduce(_ || _)
  val anySupportArithmetic = clients.map(!_.supportsArithmetic.none).reduce(_ || _)
  val anySupportLogical    = clients.map(!_.supportsLogical.none)   .reduce(_ || _)
  val anySupportGet        = clients.map(!_.supportsGet.none)       .reduce(_ || _)
  val anySupportPutFull    = clients.map(!_.supportsPutFull.none)   .reduce(_ || _)
  val anySupportPutPartial = clients.map(!_.supportsPutPartial.none).reduce(_ || _)
  val anySupportHint       = clients.map( _.supportsHint)           .reduce(_ || _)

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
  def supportsHint(id: UInt) = {
    if (allSupportHint) Bool(true) else {
      Mux1H(find(id), clients.map(c => Bool(c.supportsHint)))
    }
  }
}

case class TLBundleParameters(
  addrHiBits: Int,
  dataBits:   Int,
  sourceBits: Int,
  sinkBits:   Int,
  sizeBits:   Int)
{
  // Chisel has issues with 0-width wires
  require (addrHiBits  >= 1)
  require (dataBits    >= 8)
  require (sourceBits  >= 1)
  require (sinkBits    >= 1)
  require (sizeBits    >= 1)
  require (isPow2(dataBits))

  val addrLoBits = log2Up(dataBits/8)

  def union(x: TLBundleParameters) =
    TLBundleParameters(
      max(addrHiBits,  x.addrHiBits),
      max(dataBits,    x.dataBits),
      max(sourceBits,  x.sourceBits),
      max(sinkBits,    x.sinkBits),
      max(sizeBits,    x.sizeBits))
}

case class TLEdgeParameters(
  client:  TLClientPortParameters,
  manager: TLManagerPortParameters)
{
  val maxTransfer = max(client.maxTransfer, manager.maxTransfer)
  val maxLgSize = log2Up(maxTransfer)

  // Sanity check the link...
  require (maxTransfer >= manager.beatBytes)

  val bundle = TLBundleParameters(
    addrHiBits  = log2Up(manager.maxAddress + 1) - log2Up(manager.beatBytes),
    dataBits    = manager.beatBytes * 8,
    sourceBits  = log2Up(client.endSourceId),
    sinkBits    = log2Up(manager.endSinkId),
    sizeBits    = log2Up(maxLgSize+1))
}
