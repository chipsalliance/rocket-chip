// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util.DecoupledIO

import org.chipsalliance.rocketutils

import freechips.rocketchip.tile.TraceBundle
import freechips.rocketchip.rocket.TracedInstruction

@deprecated("moved to standalone rocketutils library", "rocketchip 2.0.0")
trait Blockable[T <: Data] {
  def blockWhile(enable_blocking: Bool, data: T): T
}

@deprecated("moved to standalone rocketutils library", "rocketchip 2.0.0")
object Blockable {
  @deprecated("moved to standalone rocketutils library", "rocketchip 2.0.0")
  implicit object BlockableBool extends Blockable[Bool] {
    def blockWhile(enable_blocking: Bool, x: Bool): Bool = x && !enable_blocking
  }

  @deprecated("moved to standalone rocketutils library", "rocketchip 2.0.0")
  implicit def BlockableDataCanBeValid[T <: DataCanBeValid]: Blockable[T] = new Blockable[T] {
    def blockWhile(enable_blocking: Bool, data: T): T = {
      val blocked: T = Wire(chiselTypeOf(data))
      blocked := data
      when (enable_blocking) { blocked.valid := false.B }
      blocked
    }
  }

  @deprecated("moved to standalone rocketutils library", "rocketchip 2.0.0")
  implicit def BlockableDecoupled[T <: Data]: Blockable[DecoupledIO[T]] = new Blockable[DecoupledIO[T]] {
    def blockWhile(enable_blocking: Bool, data: DecoupledIO[T]): DecoupledIO[T] = {
      val res = Wire(chiselTypeOf(data))
      res.valid  := data.valid
      data.ready := res.ready
      res.bits   := data.bits
      when (enable_blocking) {
        res.valid  := false.B
        data.ready := false.B
      }
      res
    }
  }

  @deprecated("moved to standalone rocketutils library", "rocketchip 2.0.0")
  implicit def BlockableCredited[T <: Data]: Blockable[CreditedIO[T]] = new Blockable[CreditedIO[T]] {
    def blockWhile(enable_blocking: Bool, data: CreditedIO[T]): CreditedIO[T] = {
      val res = Wire(chiselTypeOf(data))
      res.debit   := data.debit
      data.credit := res.credit
      res.bits    := data.bits
      when (enable_blocking) {
        res.debit := false.B
        data.credit := false.B
      }
      res
    }
  }

  @deprecated("moved to standalone rocketutils library", "rocketchip 2.0.0")
  implicit def BlockableVec[T <: Data : Blockable]: Blockable[Vec[T]] = new Blockable[Vec[T]] {
    def blockWhile(enable_blocking: Bool, data: Vec[T]): Vec[T] = {
      VecInit(data.map(x => implicitly[Blockable[T]].blockWhile(enable_blocking, x)))
    }
  }

  @deprecated("moved to standalone rocketutils library", "rocketchip 2.0.0")
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

// TODO: update to remove package scope when Blockable is fully deprecated
object BlockableTrace {
  implicit object BlockableTraceBundle extends rocketutils.Blockable[TraceBundle] {
    def blockWhile(enable_blocking: Bool, data: TraceBundle) = {
      val blocked = WireInit(data)
      blocked.insns := implicitly[rocketutils.Blockable[Vec[TracedInstruction]]].blockWhile(enable_blocking, data.insns)
      blocked
    }
  }
}
