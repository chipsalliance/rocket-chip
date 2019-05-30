// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3._
import chisel3.util._

abstract class LookupByHartIdImpl {
  def apply[T <: Data](f: TileParams => Option[T], hartId: UInt): T
}

case class HartsWontDeduplicate(t: TileParams) extends LookupByHartIdImpl {
  def apply[T <: Data](f: TileParams => Option[T], hartId: UInt): T = f(t).get
}

case class PriorityMuxHartIdFromSeq(seq: Seq[TileParams]) extends LookupByHartIdImpl {
  def apply[T <: Data](f: TileParams => Option[T], hartId: UInt): T =
    PriorityMux(seq.collect { case t if f(t).isDefined => (t.hartId.U === hartId) -> f(t).get })
}
