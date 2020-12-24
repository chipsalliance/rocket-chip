// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util.DecoupledIO


object Blockable extends {
  implicit def BlockableDataCanBeValid[T <: DataCanBeValid]: Blockable[T] = new Blockable[T] {
    def blockWhile(enable_blocking: Bool, data: T): T = {
      val blocked: T = Wire(chiselTypeOf(data))
      blocked := data
      when (enable_blocking) { blocked.valid := false.B }
      blocked
    }
  }

  implicit object BlockableTraceCoreInterface extends Blockable[TraceCoreInterface] {
    def blockWhile(enable_blocking: Bool, data: TraceCoreInterface): TraceCoreInterface = {
      val blocked: TraceCoreInterface = Wire(chiselTypeOf(data))
      blocked := data
      when (enable_blocking) {
        blocked.group.foreach { g =>
          g.iretire := 0.U
          g.itype := TraceItype.ITNothing
        }
      }
      blocked
    }
  }
}
