// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel._
import chisel3.util.ReadyValidIO
import freechips.rocketchip.util.{ShiftQueue}

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

  // Not a union, because the result may contain sizes contained by neither term
  // NOT TO BE CONFUSED WITH COVERPOINTS
  def mincover(x: TransferSizes) = {
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
  def apply(x: Int) = new TransferSizes(x)
  val none = new TransferSizes(0)

  def mincover(seq: Seq[TransferSizes]) = seq.foldLeft(none)(_ mincover _)
  def intersect(seq: Seq[TransferSizes]) = seq.reduce(_ intersect _)

  implicit def asBool(x: TransferSizes) = !x.none
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
