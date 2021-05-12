// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import chisel3.util.experimental.decode._

object DecodeLogic
{
  def apply(addr: UInt, default: BitPat, mapping: Iterable[(BitPat, BitPat)]): UInt =
    chisel3.util.experimental.decode.decoder(QMCMinimizer, addr, TruthTable(mapping, default))
  def apply(addr: UInt, default: Seq[BitPat], mappingIn: Iterable[(BitPat, Seq[BitPat])]): Seq[UInt] = {
    val elementIndexes = default.map(_.getWidth).scan(default.map(_.getWidth).sum - 1) { case (l, r) => l - r }
    val decoded = apply(addr, default.reduce(_ ## _), mappingIn.map { case (in, out) => (in, out.reduce(_ ## _)) })
    elementIndexes.zip(elementIndexes.drop(1)).map { case (msb, lsb) => decoded(msb, lsb + 1)}
  }
  def apply(addr: UInt, default: Seq[BitPat], mappingIn: List[(UInt, Seq[BitPat])]): Seq[UInt] =
    apply(addr, default, mappingIn.map(m => (BitPat(m._1), m._2)).asInstanceOf[Iterable[(BitPat, Seq[BitPat])]])
  def apply(addr: UInt, trues: Iterable[UInt], falses: Iterable[UInt]): Bool =
    apply(addr, BitPat.dontCare(1), trues.map(BitPat(_) -> BitPat("b1")) ++ falses.map(BitPat(_) -> BitPat("b0"))).asBool
}
