// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel._
import chisel3.util.ReadyValidIO
import freechips.rocketchip.util.{AsyncQueueParams, CreditedDelay, FastToSlow, RationalDirection, ShiftQueue}

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

/** A non-empty half-open range; [start, end)
  *
  * Order of [[IdRange]] is defined below:
  * - idRange with greater `start` are greater.
  * - if two `start` are same, idRange with less size are greater.
  */
case class IdRange(start: Int, end: Int) extends Ordered[IdRange]
{
  /* sanctity tests. */
  require (start >= 0, s"Ids cannot be negative, but got: $start.")
  require (start <= end, "Id ranges cannot be negative.")

  /* Define the order of IdRange. */
  def compare(x: IdRange): Int = {
    /* idRange with greater `start` are greater. */
    val primary   = (start - x.start).signum
    /* idRange with less size are greater. */
    val secondary = (x.end - end).signum
    if (primary != 0) primary else secondary
  }

  /** @return true if another [[IdRange]] `x` is overlap to this. */
  def overlaps(x: IdRange): Boolean = start < x.end && x.start < end

  /** @return true if this contains another [[IdRange]] `x`. */
  def contains(x: IdRange): Boolean = start <= x.start && x.end <= end

  /** @return true if [[Int]] x is contained by this [[IdRange]]. */
  def contains(x: Int): Boolean = start <= x && x < end

  /** It will generate hardware: if signal `x` is contained in this the range.
    *
    * original logic is ` start.B <= x && x < end.B`
    *
    * For optimization, there are 3 cases:
    * - this [[IdRange]] is 0:
    *   it will tie down to `false.B`
    * - this [[IdRange]] only has one element:
    *   just compare `x` and `start`
    * - this [[IdRange]] has multiple elements:
    *   for example, we compare 1028.U <= x < 1036.U.
    *   1028 -> 0b10000000100
    *   1035 -> 0b10000001011 (half-open range)
    *   originally, two 11 bits adder are needed to construct this logic:
    *   x - 1028.U >= 0.U
    *   x - 1035.U <= 0.U
    *
    *   In the optimized way:
    *   - find the index of smallest common bit is 4:
    *     0b10000001011 xor 0b10000000100 -> 0b1111
    *   - higher bits of `x` should equal:
    *     (x >> 4) === 0b1000000
    *   - lower bits of `x` should compare:
    *     0b0100 <= x <= 0b1011
    *
    *   In this case, two 11 bits adder will be reduced to two 4 bits adder(saves a lot timing and area.)
    *
    *   If lucky, lower bond is all 0, then we will save an additional adder.
    *   and if high bond is all 1, then we will save an additional adder with and logic
    */
  def contains(x: UInt): Bool =
    if (isEmpty) {
      /* if this range contains nothing. */
      false.B
    } else if (size == 1) {
      /* if this range only contains one instance. */
      x === start.U
    } else {
      /* optimized logic for `start.U <= x && x < end.U`. */
      /* find index of largest different bit. */
      val largestDeltaBit = log2Floor(start ^ (end - 1))
      /** the width of common bits, count from right to left. */
      val smallestCommonBit = largestDeltaBit + 1
      /** mask of different bits between start and (end - 1)*/
      val uncommonMask = (1 << smallestCommonBit) - 1
      /** the lower bits of x. */
      val uncommonBits = (x | 0.U(smallestCommonBit.W)) (largestDeltaBit, 0)
      /* the higher bits must match exactly (note: may shift ALL bits away) */
      (x >> smallestCommonBit).asUInt() === (start >> smallestCommonBit).U &&
        /* firrtl constant prop range analysis can eliminate these two: */
        UInt(start & uncommonMask) <= uncommonBits &&
        uncommonBits <= UInt((end - 1) & uncommonMask)
    }

  /** @return A new instance, adds `x` as offset. */
  def shift(x: Int): IdRange = IdRange(start + x, end + x)

  /** @return Size of this range. */
  def size: Int = end - start

  /** @return true if this [[IdRange]] is empty.*/
  def isEmpty: Boolean = end == start

  /** @return a [[Range]] of from `start` until `end`. */
  def range: Range = start until end
}

object IdRange
{
  /** Find if a sequence of [[IdRange]] are overlapped.
    *
    * @param s is a sequence of [[IdRange]]
    *
    * @return a first pair of [[IdRange]], if `s` has any overlaps to each other. */
  def overlaps(s: Seq[IdRange]): Option[(IdRange, IdRange)] = if (s.isEmpty) None else {
    val ranges = s.sorted
    (ranges.tail zip ranges.init) find { case (a, b) => a overlaps b }
  }
}

/** An potentially empty inclusive range of 2-powers [min, max] in bytes */
case class TransferSizes(min: Int, max: Int)
{
  /** @return a [[TransferSizes]] with `min` and `max` both with `x`. */
  def this(x: Int) = this(x, x)

  /* sanity check. */
  require (min <= max, s"Min transfer $min > max transfer $max")
  require (min >= 0 && max >= 0, s"TransferSizes must be positive, got: ($min, $max)")
  require (max == 0 || isPow2(max), s"TransferSizes must be a power of 2, got: $max")
  require (min == 0 || isPow2(min), s"TransferSizes must be a power of 2, got: $min")
  require (max == 0 || min != 0, s"TransferSize 0 is forbidden unless (0,0), got: ($min, $max)")

  /** @return true, if this [[TransferSizes]] is empty. */
  def none: Boolean = min == 0

  /** @return true, if `x` is a power of 2 number between `min`, `max`. */
  def contains(x: Int): Boolean = isPow2(x) && min <= x && x <= max

  /** Give a constant `x`, check if 2 to the power of `x` is in this [[TransferSizes]]. */
  def containsLg(x: Int): Boolean = contains(1 << x)

  /** Generate a circuit with [[UInt]] signal `x` as input
    * to check if 2 to the power of `x` is in this.
    */
  def containsLg(x: UInt): Bool =
    if (none) false.B
    else if (min == max) { log2Ceil(min).U === x }
    else { log2Ceil(min).U <= x && x <= log2Ceil(max).U }

  /** @return true if `x` is a subset to this. */
  def contains(x: TransferSizes): Boolean = x.none || (min <= x.min && x.max <= max)

  /** @return the intersecting [[TransferSizes]] between this and `x`. */
  def intersect(x: TransferSizes): TransferSizes =
    if (x.max < min || max < x.min) TransferSizes.none
    else TransferSizes(scala.math.max(min, x.min), scala.math.min(max, x.max))

  /** A minimal continuous [[TransferSizes]] to cover this and `x`,
    *
    * @note This is not a union, because if this and `x` has no intersection,
    *       result will contain sizes contained by neither term.
    *       This should ont to be confused with coverpoints
    */
  def mincover(x: TransferSizes): TransferSizes = {
    if (none) {
      x
    } else if (x.none) {
      this
    } else {
      TransferSizes(scala.math.min(min, x.min), scala.math.max(max, x.max))
    }
  }

  override def toString() = "TransferSizes[%d, %d]".format(min, max)
}

object TransferSizes {
  /** @return a [[TransferSizes]] with `min` and `max` both `x`. */
  def apply(x: Int): TransferSizes = new TransferSizes(x)

  /** @return a [[TransferSizes]] with `min` and `max` both 0. */
  val none: TransferSizes = new TransferSizes(0)

  /** @return A minimal continuous [[TransferSizes]] to cover all `seq`. */
  def mincover(seq: Seq[TransferSizes]): TransferSizes = seq.foldLeft(none)(_ mincover _)

  /** @return A intersection among all `seq`. */
  def intersect(seq: Seq[TransferSizes]): TransferSizes = seq.reduce(_ intersect _)

  implicit def asBool(x: TransferSizes) = !x.none
}

// AddressSets specify the address space managed by the manager
// Base is the base address, and mask are the bits consumed by the manager
// e.g: base=0x200, mask=0xff describes a device managing 0x200-0x2ff
// e.g: base=0x1000, mask=0xf0f decribes a device managing 0x1000-0x100f, 0x1100-0x110f, ...
case class AddressSet(base: BigInt, mask: BigInt) extends Ordered[AddressSet]
{
  // Forbid misaligned base address (and empty sets)
  require ((base & mask) == 0, s"Mis-aligned AddressSets are forbidden, got: ${this.toString}")
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
    intersect(x) match {
      case None => Seq(this)
      case Some(remove) => AddressSet.enumerateBits(mask & ~remove.mask).map { bit =>
        val nmask = (mask & (bit-1)) | remove.mask
        val nbase = (remove.base ^ bit) & ~nmask
        AddressSet(nbase, nmask)
      }
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

  def unify(seq: Seq[AddressSet], bit: BigInt): Seq[AddressSet] = {
    // Pair terms up by ignoring 'bit'
    seq.distinct.groupBy(x => x.copy(base = x.base & ~bit)).map { case (key, seq) =>
      if (seq.size == 1) {
        seq.head // singleton -> unaffected
      } else {
        key.copy(mask = key.mask | bit) // pair - widen mask by bit
      }
    }.toList
  }

  def unify(seq: Seq[AddressSet]): Seq[AddressSet] = {
    val bits = seq.map(_.base).foldLeft(BigInt(0))(_ | _)
    AddressSet.enumerateBits(bits).foldLeft(seq) { case (acc, bit) => unify(acc, bit) }.sorted
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

  def irrevocable[T <: Data](x: ReadyValidIO[T]) =
    if (isDefined) Queue.irrevocable(x, depth, flow=flow, pipe=pipe)
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
case class CreditedCrossing(sourceDelay: CreditedDelay, sinkDelay: CreditedDelay) extends ClockCrossingType

object CreditedCrossing {
  def apply(delay: CreditedDelay): CreditedCrossing = CreditedCrossing(delay, delay.flip)
  def apply(): CreditedCrossing = CreditedCrossing(CreditedDelay(1, 1))
}

trait DirectedBuffers[T] {
  def copyIn(x: BufferParams): T
  def copyOut(x: BufferParams): T
  def copyInOut(x: BufferParams): T
}

trait IdMapEntry {
  def name: String
  def from: IdRange
  def to: IdRange
  def isCache: Boolean
  def requestFifo: Boolean
  def maxTransactionsInFlight: Option[Int]
  def pretty(fmt: String) =
    if (from ne to) { // if the subclass uses the same reference for both from and to, assume its format string has an arity of 5
      fmt.format(to.start, to.end, from.start, from.end, s""""$name"""", if (isCache) " [CACHE]" else "", if (requestFifo) " [FIFO]" else "")
    } else {
      fmt.format(from.start, from.end, s""""$name"""", if (isCache) " [CACHE]" else "", if (requestFifo) " [FIFO]" else "")
    }
}

abstract class IdMap[T <: IdMapEntry] {
  protected val fmt: String
  val mapping: Seq[T]
  def pretty: String = mapping.map(_.pretty(fmt)).mkString(",\n")
}
