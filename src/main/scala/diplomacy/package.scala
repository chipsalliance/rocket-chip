// See LICENSE.SiFive for license details.

package freechips.rocketchip

import chisel3.internal.sourceinfo.{SourceInfo, SourceLine, UnlocatableSourceInfo}
import freechips.rocketchip.config.Parameters

package object diplomacy
{
  type SimpleNodeHandle[D, U, E, B <: Chisel.Data] = NodeHandle[D, U, E, B, D, U, E, B]

  def sourceLine(sourceInfo: SourceInfo, prefix: String = " (", suffix: String = ")") = sourceInfo match {
    case SourceLine(filename, line, col) => s"$prefix$filename:$line:$col$suffix"
    case _ => ""
  }

  def bitIndexes(x: BigInt, tail: Seq[Int] = Nil): Seq[Int] = {
    require (x >= 0)
    if (x == 0) {
      tail.reverse
    } else {
      val lowest = x.lowestSetBit
      bitIndexes(x.clearBit(lowest), lowest +: tail)
    }
  }

  implicit class BigIntHexContext(val sc: StringContext) extends AnyVal {
    def x(args: Any*): BigInt = {
      val orig = sc.s(args: _*)
      BigInt(orig.replace("_", ""), 16)
    }
  }

  def EnableMonitors[T](body: Parameters => T)(implicit p: Parameters) = body(p.alterPartial {
    case MonitorsEnabled => true
  })
  def DisableMonitors[T](body: Parameters => T)(implicit p: Parameters) = body(p.alterPartial {
    case MonitorsEnabled => false
  })
  def FlipRendering[T](body: Parameters => T)(implicit p: Parameters) = body(p.alterPartial {
    case RenderFlipped => !p(RenderFlipped)
  })
}
