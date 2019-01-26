// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel._

// Use AddressSet instead -- this is just for pretty printing
case class AddressRange(base: BigInt, size: BigInt) extends Ordered[AddressRange]
{
  val end = base + size

  require (base >= 0, s"AddressRange base must be positive, got: $base")
  require (size > 0, s"AddressRange size must be > 0, got: $size")

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

  private def helper(base: BigInt, end: BigInt) =
    if (base < end) Seq(AddressRange(base, end-base)) else Nil
  def subtract(x: AddressRange) =
    helper(base, end min x.base) ++ helper(base max x.end, end)

  // We always want to see things in hex
  override def toString() = "AddressRange(0x%x, 0x%x)".format(base, size)

  // Other possible output formats
  def toUVM: String = f"    set_addr_range(1, 32'h${base}%08x, 32'h${end}%08x);"
  def toJSON: String = s"""{"base": ${base}, "max": ${end}}"""
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
  // Set subtraction... O(n*n) b/c I am lazy
  def subtract(from: Seq[AddressRange], take: Seq[AddressRange]): Seq[AddressRange] =
    take.foldLeft(from) { case (left, r) => left.flatMap { _.subtract(r) } }
}

case class AddressMapEntry(range: AddressRange, permissions: ResourcePermissions, names: Seq[String]) {
  val ResourcePermissions(r, w, x, c, a) = permissions

  def toString(aw: Int) = s"\t%${aw}x - %${aw}x %c%c%c%c%c %s".format(
    range.base,
    range.base+range.size,
    if (a) 'A' else ' ',
    if (r) 'R' else ' ',
    if (w) 'W' else ' ',
    if (x) 'X' else ' ',
    if (c) 'C' else ' ',
    names.mkString(", "))

  def toJSON = s"""{"base":[${range.base}],"size":[${range.size}],""" +
    s""""r":[$r],"w":[$w],"x":[$x],"c":[$c],"a":[$a],""" +
    s""""names":[${names.map('"'+_+'"').mkString(",")}]}"""
}
