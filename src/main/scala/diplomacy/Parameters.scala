// See LICENSE.SiFive for license details.

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
  
  def range = start until end
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

// Use AddressSet instead -- this is just for pretty printing
case class AddressRange(base: BigInt, size: BigInt) extends Ordered[AddressRange]
{
  val end = base + size

  require (base >= 0)
  require (size > 0)

  def compare(x: AddressRange) = {
    val primary   = (this.base - x.base).signum
    val secondary = (x.size - this.size).signum
    if (primary != 0) primary else secondary
  }

  def contains(x: AddressRange) = base <= x.base && x.end <= end
  def union(x: AddressRange): Option[AddressRange] = {
    if (base > x.end || x.base > end) {
      None
    } else {
      val obase = if (base < x.base) base else x.base
      val oend  = if (end  > x.end)  end  else x.end
      Some(AddressRange(obase, oend-obase))
    }
  }
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

  // Return an AddressSet that only contains the addresses both sets contain
  def intersect(x: AddressSet): Option[AddressSet] = {
    if (!overlaps(x)) {
      None
    } else {
      val r_mask = mask & x.mask
      val r_base = base | x.base
      Some(AddressSet(r_base, r_mask))
    }
  }

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

  def toRanges = {
    require (finite)
    val size = alignment
    val fragments = mask & ~(size-1)
    val bits = bitIndexes(fragments)
    (BigInt(0) until (BigInt(1) << bits.size)).map { i =>
      val off = bitIndexes(i).foldLeft(base) { case (a, b) => a.setBit(bits(b)) }
      AddressRange(off, size)
    }
  }
}

object AddressRange
{
  def fromSets(seq: Seq[AddressSet]): Seq[AddressRange] = unify(seq.flatMap(_.toRanges))
  def unify(seq: Seq[AddressRange]): Seq[AddressRange] = {
    if (seq.isEmpty) return Nil
    val ranges = seq.sorted
    ranges.tail.foldLeft(Seq(ranges.head)) { case (head :: tail, x) =>
      head.union(x) match {
        case Some(z) => z :: tail
        case None => x :: head :: tail
      }
    }.reverse
  }
}

object AddressSet
{
  def misaligned(base: BigInt, size: BigInt, tail: Seq[AddressSet] = Seq()): Seq[AddressSet] = {
    if (size == 0) tail.reverse else {
      val maxBaseAlignment = base & (-base) // 0 for infinite (LSB)
      val maxSizeAlignment = BigInt(1) << log2Floor(size) // MSB of size
      val step =
        if (maxBaseAlignment == 0 || maxBaseAlignment > maxSizeAlignment)
        maxSizeAlignment else maxBaseAlignment
      misaligned(base+step, size-step, AddressSet(base, step-1) +: tail)
    }
  }

  def unify(seq: Seq[AddressSet]): Seq[AddressSet] = {
    val n = seq.size
    val array = Array(seq:_*)
    var filter = Array.fill(n) { false }
    for (i <- 0 until n-1) { if (!filter(i)) {
      for (j <- i+1 until n) { if (!filter(j)) {
        val a = array(i)
        val b = array(j)
        if (a.mask == b.mask && isPow2(a.base ^ b.base)) {
          val c_base = a.base & ~(a.base ^ b.base)
          val c_mask = a.mask | (a.base ^ b.base)
          filter.update(j, true)
          array.update(i, AddressSet(c_base, c_mask))
        }
      }}
    }}
    val out = (array zip filter) flatMap { case (a, f) => if (f) None else Some(a) }
    if (out.size != n) unify(out) else out.toList
  }
}
