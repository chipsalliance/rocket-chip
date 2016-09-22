package uncore

import Chisel._
import chisel3.internal.sourceinfo.{SourceInfo, SourceLine, UnlocatableSourceInfo}

package object tilelink2
{
  type TLBaseNode = BaseNode[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle]
  type IntBaseNode = BaseNode[IntSourcePortParameters, IntSinkPortParameters, IntEdge, IntEdge, Vec[Bool]]
  def OH1ToUInt(x: UInt) = OHToUInt((x << 1 | UInt(1)) ^ x)
  def UIntToOH1(x: UInt, width: Int) = ~(SInt(-1, width=width).asUInt << x)(width-1, 0)
  def trailingZeros(x: Int) = if (x > 0) Some(log2Ceil(x & -x)) else None
  def highOR(x: UInt) = {
    val w = x.getWidth
    def helper(s: Int, x: UInt): UInt =
      if (s >= w) x else helper(s+s, x | x << s)
    helper(1, x)
  }

  def sourceLine(sourceInfo: SourceInfo, prefix: String = " (", suffix: String = ")") = sourceInfo match {
    case SourceLine(filename, line, col) => s"$prefix$filename:$line:$col$suffix"
    case _ => ""
  }
}
