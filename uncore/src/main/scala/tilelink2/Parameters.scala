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
  case object UNCACHEABLE extends T
  val cases = Seq(CACHED, TRACKED, UNCACHED, UNCACHEABLE)
}

// A non-empty half-open range; [start, end)
case class IdRange(start: Int, end: Int)
{
  require (start >= 0)
  require (end >= 0)
  require (start < end) // not empty

  // This is a strict partial ordering
  def <(x: IdRange) = end <= x.start
  def >(x: IdRange) = x < this

  def overlaps(x: IdRange) = start < x.end && x.start < end
  def contains(x: IdRange) = start <= x.start && x.end <= end
  // contains => overlaps (because empty is forbidden)

  def contains(x: Int)  = start <= x && x < end
  def contains(x: UInt) = UInt(start) <= x && x < UInt(end) // !!! special-case =

  def shift(x: Int) = IdRange(start+x, end+x)
}

// An potentially empty inclusive range of 2-powers [min, max]
case class TransferSizes(min: Int, max: Int)
{
  def this(x: Int) = this(x, x)

  require (min <= max)
  require (min != 0 || max == 0)
  require (max == 0 || isPow2(max))
  require (min == 0 || isPow2(min))

  def none = min == 0
  def contains(x: Int) = isPow2(x) && min <= x && x <= max
  def containsLg(x: Int) = contains(1 << x)
  def containsLg(x: UInt) = if (none) Bool(false) else { UInt(log2Ceil(min)) <= x && x <= UInt(log2Ceil(max)) } // !!! special-case =

  def contains(x: TransferSizes) = x.none || (min <= x.min && x.max <= max)
  
  def intersect(x: TransferSizes) =
    if (x.max < min || min < x.max) TransferSizes.none
    else TransferSizes(scala.math.max(min, x.min), scala.math.min(max, x.max))
}

object TransferSizes {
  def apply(x: Int) = new TransferSizes(x)
  val none = new TransferSizes(0)
}

// AddressSets specify the mask of bits consumed by the manager
// The base address used by the crossbar for routing
case class AddressSet(mask: BigInt, base: Option[BigInt] = None)
{
  // Forbid empty sets
  require (base == None || (base.get & mask) == 0)

  def contains(x: BigInt) = ((x ^ base.get) & ~mask) == 0
  def contains(x: UInt) = ((x ^ UInt(base.get)) & UInt(~mask)) === UInt(0)

  // overlap iff bitwise: both care (~mask0 & ~mask1) => both equal (base0=base1)
  // if base = None, it will be auto-assigned and thus not overlap anything
  def overlaps(x: AddressSet) = (base, x.base) match {
    case (Some(tbase), Some(xbase)) => (~(mask | x.mask) & (tbase ^ xbase)) == 0
    case _ => false
  }

  // contains iff bitwise: x.mask => mask && contains(x.base)
  def contains(x: AddressSet) = ((x.mask | (base.get ^ x.base.get)) & ~mask) == 0
  // 1 less than the number of bytes to which the manager should be aligned
  def alignment1 = ((mask + 1) & ~mask) - 1
  def max = base.get | mask
}

case class TLManagerParameters(
  address:            Seq[AddressSet],
  sinkId:             IdRange       = IdRange(0, 1),
  regionType:         RegionType.T  = RegionType.UNCACHEABLE,
  // Supports both Acquire+Release of these sizes
  supportsAcquire:    TransferSizes = TransferSizes.none,
  supportsAtomic:     TransferSizes = TransferSizes.none,
  supportsGet:        TransferSizes = TransferSizes.none,
  supportsPutFull:    TransferSizes = TransferSizes.none,
  supportsPutPartial: TransferSizes = TransferSizes.none,
  supportsHints:      Boolean       = false,
  // If fifoId=Some, all messages sent to the same fifoId are delivered in FIFO order
  fifoId:             Option[Int]   = None)
{
  address.combinations(2).foreach({ case Seq(x,y) =>
    require (!x.overlaps(y))
  })
  address.foreach({ case a =>
    require (supportsAcquire.none || a.alignment1 >= supportsAcquire.max-1)
  })

  val maxTransfer = List(
    supportsAcquire.max,
    supportsAtomic.max,
    supportsGet.max,
    supportsPutFull.max,
    supportsPutPartial.max).max
}

case class TLManagerPortParameters(managers: Seq[TLManagerParameters], beatBytes: Int)
{
  require (isPow2(beatBytes))

  // Require disjoint ranges for Ids and addresses
  managers.combinations(2).foreach({ case Seq(x,y) =>
    require (!x.sinkId.overlaps(y.sinkId))
    x.address.foreach({ a => y.address.foreach({ b =>
      require (!a.overlaps(b))
    })})
  })

  def endSinkId   = managers.map(_.sinkId.end).max
  def maxAddress  = managers.map(_.address.map(_.max).max).max
  def maxGet      = managers.map(_.supportsGet.max).max
  def maxTransfer = managers.map(_.maxTransfer).max

  // These return Option[TLSinkParameters] for your convenience
  def findById(x: Int) = managers.find(_.sinkId.contains(x))
  def findByAddress(x: BigInt) = managers.find(_.address.exists(_.contains(x)))

  //def buildCacheInfo(): UInt => Chilse(RegionType) // UInt = address, not sink_id
  //def buildAtomicInfo(): UInt => Bool
}

case class TLClientParameters(
  sourceId:            IdRange       = IdRange(0,1),
  // Supports both Probe+Grant of these sizes
  supportsProbe:       TransferSizes = TransferSizes.none,
  supportsAtomics:     TransferSizes = TransferSizes.none,
  supportsGet:         TransferSizes = TransferSizes.none,
  supportsPutFull:     TransferSizes = TransferSizes.none,
  supportsPutPartial:  TransferSizes = TransferSizes.none,
  supportsHints:       Boolean       = false)
{
  val maxTransfer = List(
    supportsProbe.max,
    supportsAtomics.max,
    supportsGet.max,
    supportsPutFull.max,
    supportsPutPartial.max).max
}

case class TLClientPortParameters(clients: Seq[TLClientParameters]) {
  def endSourceId = clients.map(_.sourceId.end).max
  def maxTransfer = clients.map(_.maxTransfer).max
//  def nSources: Int = sourceView.map(_.sourceIds.count).sum
//  def nCaches: Int = sourceView.map(s => if(s.supportsProbe) 1 else 0).sum
  //def makeSourceToCache() = ... 
  //def makeCacheToStartSource() = ...
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
  require (dataBits    >= 1)
  require (sourceBits  >= 1)
  require (sinkBits    >= 1)
  require (sizeBits    >= 1)
  require (isPow2(dataBits))
  
  def union(x: TLBundleParameters) =
    TLBundleParameters(
      max(addressBits, x.addressBits),
      max(dataBits,    x.dataBits),
      max(sourceBits,  x.sourceBits),
      max(sinkBits,    x.sinkBits),
      max(sizeBits,    x.sizeBits))
}

case class TLEdgeParameters(
  client:  TLClientPortParameters,
  manager: TLManagerPortParameters)
{
  val bundle = TLBundleParameters(
    addressBits = log2Up(manager.maxAddress + 1) - log2Up(manager.beatBytes),
    dataBits    = manager.beatBytes * 8,
    sourceBits  = log2Up(client.endSourceId),
    sinkBits    = log2Up(manager.endSinkId),
    sizeBits    = log2Up(log2Up(max(client.maxTransfer, manager.maxTransfer))+1))
}
