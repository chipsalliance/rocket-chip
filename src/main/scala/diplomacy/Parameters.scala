// See LICENSE for license details.

package diplomacy

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
    if (x.max < min || max < x.min) TransferSizes.none
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
case class AddressSet(base: BigInt, mask: BigInt) extends Ordered[AddressSet]
{
  // Forbid misaligned base address (and empty sets)
  require ((base & mask) == 0)
  require (base >= 0) // TL2 address widths are not fixed => negative is ambiguous
  // We do allow negative mask (=> ignore all high bits)

  def contains(x: BigInt) = ((x ^ base) & ~mask) == 0
  def contains(x: UInt) = ((x ^ UInt(base)).zext() & SInt(~mask)) === SInt(0)

  // overlap iff bitwise: both care (~mask0 & ~mask1) => both equal (base0=base1)
  def overlaps(x: AddressSet) = (~(mask | x.mask) & (base ^ x.base)) == 0
  // contains iff bitwise: x.mask => mask && contains(x.base)
  def contains(x: AddressSet) = ((x.mask | (base ^ x.base)) & ~mask) == 0

  // The number of bytes to which the manager must be aligned
  def alignment = ((mask + 1) & ~mask)
  // Is this a contiguous memory range
  def contiguous = alignment == mask+1

  def finite = mask >= 0
  def max = { require (finite); base | mask }

  // Widen the match function to ignore all bits in imask
  def widen(imask: BigInt) = AddressSet(base & ~imask, mask | imask)

  // AddressSets have one natural Ordering (the containment order, if contiguous)
  def compare(x: AddressSet) = {
    val primary   = (this.base - x.base).signum // smallest address first
    val secondary = (x.mask - this.mask).signum // largest mask first
    if (primary != 0) primary else secondary
  }

  // We always want to see things in hex
  override def toString() = {
    if (mask >= 0) {
      "AddressSet(0x%x, 0x%x)".format(base, mask)
    } else {
      "AddressSet(0x%x, ~0x%x)".format(base, ~mask)
    }
  }
}

object AddressSet
{
  def misaligned(base: BigInt, size: BigInt): Seq[AddressSet] = {
    val largestPow2 = BigInt(1) << log2Floor(size)
    val mostZeros = (base + size - 1) & ~(largestPow2 - 1)
    def splitLo(low: BigInt, high: BigInt, tail: Seq[AddressSet]): Seq[AddressSet] = {
      if (low == high) tail else {
        val toggleBits = low ^ high
        val misalignment = toggleBits & (-toggleBits)
        splitLo(low+misalignment, high, AddressSet(low, misalignment-1) +: tail)
      }
    }
    def splitHi(low: BigInt, high: BigInt, tail: Seq[AddressSet]): Seq[AddressSet] = {
      if (low == high) tail else {
        val toggleBits = low ^ high
        val misalignment = toggleBits & (-toggleBits)
        splitHi(low, high-misalignment, AddressSet(high-misalignment, misalignment-1) +: tail)
      }
    }
    splitLo(base, mostZeros, splitHi(mostZeros, base+size, Seq())).sorted
  }
}

