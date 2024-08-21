// See LICENSE.SiFive for license details.
package freechips.rocketchip.prci

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy._
import org.chipsalliance.diplomacy.lazymodule._

/** This adapter takes an input reset and stretches it.
  *
  * If the reset was asynchronous, it becomes synchronous.
  */
class ResetStretcher(cycles: Int)(implicit p: Parameters) extends LazyModule {
  val node = ClockAdapterNode()(ValName("reset_stretcher"))
  require(cycles > 1, s"ResetStretcher only supports cycles > 1 but got ${cycles}")
  override lazy val desiredName = s"ResetStretcher$cycles"
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out).foreach { case ((in, _), (out, _)) =>
      out.clock := in.clock
      out.reset := withClockAndReset(in.clock, in.reset) {
        val count = RegInit(0.U(log2Ceil(cycles).W))
        val resetout = RegInit(true.B)
        when (resetout) {
          count := count + 1.U
          resetout := (count < (cycles-1).U)
        }
        resetout
      }
    }
  }
}
