// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel._
import freechips.rocketchip.util.{ShiftQueue, RationalDirection, FastToSlow, AsyncQueueParams}
import scala.reflect.ClassTag

/** Options for describing the attributes of memory regions */
object RegionType {
  // Define the 'more relaxed than' ordering
  val cases = Seq(CACHED, TRACKED, UNCACHED, IDEMPOTENT, VOLATILE, PUT_EFFECTS, GET_EFFECTS)
  sealed trait T extends Ordered[T] {
    def compare(that: T): Int = cases.indexOf(that) compare cases.indexOf(this)
  }

  case object CACHED      extends T // an intermediate agent may have cached a copy of the region for you
  case object TRACKED     extends T // the region may have been cached by another master, but coherence is being provided
  case object UNCACHED    extends T // the region has not been cached yet, but should be cached when possible
  case object IDEMPOTENT  extends T // gets return most recently put content, but content should not be cached
  case object VOLATILE    extends T // content may change without a put, but puts and gets have no side effects
  case object PUT_EFFECTS extends T // puts produce side effects and so must not be combined/delayed
  case object GET_EFFECTS extends T // gets produce side effects and so must not be issued speculatively
}

// A non-empty half-open range; [start, end)
case class IdRange(start: Int, end: Int) extends Ordered[IdRange]
{
  require (start >= 0, s"Ids cannot be negative, but got: $start.")
  require (start <= end, "Id ranges cannot be negative.")

  def compare(x: IdRange) = {
    val primary   = (this.start - x.start).signum
    val secondary = (x.end - this.end).signum
    if (primary != 0) primary else secondary
  }

  def overlaps(x: IdRange) = start < x.end && x.start < end
  def contains(x: IdRange) = start <= x.start && x.end <= end

  def contains(x: Int)  = start <= x && x < end
  def contains(x: UInt) =
    if (size == 0) {
      Bool(false)
    } else if (size == 1) { // simple comparison
      x === UInt(start)
    } else {
      // find index of largest different bit
      val largestDeltaBit = log2Floor(start ^ (end-1))
      val smallestCommonBit = largestDeltaBit + 1 // may not exist in x
      val uncommonMask = (1 << smallestCommonBit) - 1
      val uncommonBits = (x | UInt(0, width=smallestCommonBit))(largestDeltaBit, 0)
      // the prefix must match exactly (note: may shift ALL bits away)
      (x >> smallestCommonBit) === UInt(start >> smallestCommonBit) &&
      // firrtl constant prop range analysis can eliminate these two:
      UInt(start & uncommonMask) <= uncommonBits &&
      uncommonBits <= UInt((end-1) & uncommonMask)
    }

  def shift(x: Int) = IdRange(start+x, end+x)
  def size = end - start
  def isEmpty = end == start

  def range = start until end
}

object IdRange
{
  def overlaps(s: Seq[IdRange]) = if (s.isEmpty) None else {
    val ranges = s.sorted
    (ranges.tail zip ranges.init) find { case (a, b) => a overlaps b }
  }
}

// An potentially empty inclusive range of 2-powers [min, max] (in bytes)
case class TransferSizes(min: Int, max: Int)
{
  def this(x: Int) = this(x, x)

  require (min <= max, s"Min transfer $min > max transfer $max")
  require (min >= 0 && max >= 0, s"TransferSizes must be positive, got: ($min, $max)")
  require (max == 0 || isPow2(max), s"TransferSizes must be a power of 2, got: $max")
  require (min == 0 || isPow2(min), s"TransferSizes must be a power of 2, got: $min")
  require (max == 0 || min != 0, s"TransferSize 0 is forbidden unless (0,0), got: ($min, $max)")

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

  override def toString() = "TransferSizes[%d, %d]".format(min, max)

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
  require ((base & mask) == 0, s"Mis-aligned AddressSets are forbidden, got: ($base, $mask)")
  require (base >= 0, s"AddressSet negative base is ambiguous: $base") // TL2 address widths are not fixed => negative is ambiguous
  // We do allow negative mask (=> ignore all high bits)

  def contains(x: BigInt) = ((x ^ base) & ~mask) == 0
  def contains(x: UInt) = ((x ^ UInt(base)).zext() & SInt(~mask)) === SInt(0)

  // turn x into an address contained in this set
  def legalize(x: UInt): UInt = base.U | (mask.U & x)

  // overlap iff bitwise: both care (~mask0 & ~mask1) => both equal (base0=base1)
  def overlaps(x: AddressSet) = (~(mask | x.mask) & (base ^ x.base)) == 0
  // contains iff bitwise: x.mask => mask && contains(x.base)
  def contains(x: AddressSet) = ((x.mask | (base ^ x.base)) & ~mask) == 0

  // The number of bytes to which the manager must be aligned
  def alignment = ((mask + 1) & ~mask)
  // Is this a contiguous memory range
  def contiguous = alignment == mask+1

  def finite = mask >= 0
  def max = { require (finite, "Max cannot be calculated on infinite mask"); base | mask }

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

  def subtract(x: AddressSet): Seq[AddressSet] = {
    if (!overlaps(x)) {
      Seq(this)
    } else {
      val new_inflex = ~x.mask & mask
      // !!! this fractures too much; find a better algorithm
      val fracture = AddressSet.enumerateMask(new_inflex).flatMap(m => intersect(AddressSet(m, ~new_inflex)))
      fracture.filter(!_.overlaps(x))
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
    require (finite, "Ranges cannot be calculated on infinite mask")
    val size = alignment
    val fragments = mask & ~(size-1)
    val bits = bitIndexes(fragments)
    (BigInt(0) until (BigInt(1) << bits.size)).map { i =>
      val off = bitIndexes(i).foldLeft(base) { case (a, b) => a.setBit(bits(b)) }
      AddressRange(off, size)
    }
  }
}

object AddressSet
{
  val everything = AddressSet(0, -1)
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

  def enumerateMask(mask: BigInt): Seq[BigInt] = {
    def helper(id: BigInt, tail: Seq[BigInt]): Seq[BigInt] =
      if (id == mask) (id +: tail).reverse else helper(((~mask | id) + 1) & mask, id +: tail)
    helper(0, Nil)
  }

  def enumerateBits(mask: BigInt): Seq[BigInt] = {
    def helper(x: BigInt): Seq[BigInt] = {
      if (x == 0) {
        Nil
      } else {
        val bit = x & (-x)
        bit +: helper(x & ~bit)
      }
    }
    helper(mask)
  }
}

case class BufferParams(depth: Int, flow: Boolean, pipe: Boolean)
{
  require (depth >= 0, "Buffer depth must be >= 0")
  def isDefined = depth > 0
  def latency = if (isDefined && !flow) 1 else 0

  def apply[T <: Data](x: DecoupledIO[T]) =
    if (isDefined) Queue(x, depth, flow=flow, pipe=pipe)
    else x

  def sq[T <: Data](x: DecoupledIO[T]) =
    if (!isDefined) x else {
      val sq = Module(new ShiftQueue(x.bits, depth, flow=flow, pipe=pipe))
      sq.io.enq <> x
      sq.io.deq
    }

  override def toString() = "BufferParams:%d%s%s".format(depth, if (flow) "F" else "", if (pipe) "P" else "")

}

object BufferParams
{
  implicit def apply(depth: Int): BufferParams = BufferParams(depth, false, false)

  val default = BufferParams(2)
  val none    = BufferParams(0)
  val flow    = BufferParams(1, true, false)
  val pipe    = BufferParams(1, false, true)
}

case class TriStateValue(value: Boolean, set: Boolean)
{
  def update(orig: Boolean) = if (set) value else orig
}

object TriStateValue
{
  implicit def apply(value: Boolean): TriStateValue = TriStateValue(value, true)
  def unset = TriStateValue(false, false)
}

/** Enumerates the types of clock crossings generally supported by Diplomatic bus protocols  */
sealed trait ClockCrossingType
{
  def sameClock = this match {
    case _: SynchronousCrossing => true
    case _ => false
  }
}

case object NoCrossing // converts to SynchronousCrossing(BufferParams.none) via implicit def in package
case class SynchronousCrossing(params: BufferParams = BufferParams.default) extends ClockCrossingType
case class RationalCrossing(direction: RationalDirection = FastToSlow) extends ClockCrossingType
case class AsynchronousCrossing(depth: Int = 8, sourceSync: Int = 3, sinkSync: Int = 3, safe: Boolean = true, narrow: Boolean = false) extends ClockCrossingType
{
  def asSinkParams = AsyncQueueParams(depth, sinkSync, safe, narrow)
}

trait DirectedBuffers[T] {
  def copyIn(x: BufferParams): T
  def copyOut(x: BufferParams): T
  def copyInOut(x: BufferParams): T
}

trait UserBits {
  def width: Int
  require (width >= 0)
}

case class PadUserBits(width: Int) extends UserBits

case class UserBitField[T <: UserBits](tag: T, value: UInt)

object UserBits {
  // Highest index fields are returned first in the output
  def extract[T <: UserBits : ClassTag](meta: Seq[UserBits], userBits: UInt): Seq[UserBitField[T]] = meta match {
    case Nil => Nil
    case head :: tail =>
      tail.scanLeft((head, 0)) {
        case ((x, base), y) => (y, base+x.width)
      }.collect {
        case (x: T, y) => UserBitField(x, userBits(y+x.width-1, y))
      }.reverse
  }
  // Fills highest indexed fields from the front of 'seq' input
  def inject[T <: UserBits : ClassTag](meta: Seq[UserBits], userBits: UInt, seq: Seq[UInt]): UInt = meta match {
    case Nil => userBits
    case head :: tail => {
      val elts = tail.scanLeft((head, 0)) {
        case ((x, base), y) => (y, base+x.width)
      }
      val mask = ~UInt(elts.collect {
        case (x: T, y) => ((BigInt(1) << x.width) - 1) << y
      }.foldLeft(BigInt(0)) {
        case (b, a) => b | a
      }, width = meta.map(_.width).sum)
      val concat = elts.reverse.zip(seq).collect {
        case ((x: T, y), v) => (v|UInt(0, width=x.width))(x.width-1, 0) << y
      }.foldLeft(UInt(0)) {
        case (b, a) => b | a
      }
      (userBits & mask) | concat
    }
  }
}
