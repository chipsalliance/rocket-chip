// See LICENSE.SiFive for license details.
package freechips.rocketchip.prci

import chisel3._
import chisel3.util.log2Ceil
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.Attachable

sealed trait ResetCrossingType {
  def injectClockNode(context: Attachable)(implicit p: Parameters): ClockNode
}

case class NoResetCrossing() extends ResetCrossingType {
  def injectClockNode(context: Attachable)(implicit p: Parameters): ClockNode = ClockTempNode()
}

case class StretchedResetCrossing(cycles: Int) extends ResetCrossingType {
  def injectClockNode(context: Attachable)(implicit p: Parameters): ClockNode = {
    val rs = LazyModule(new ResetStretcher(cycles))
    rs.node
  }
}

/* Stretch async reset
*/
class ResetStretcher(cycles: Int)(implicit p: Parameters) extends LazyModule {
  val node = ClockAdapterNode()(ValName("reset_stretcher"))
  require(cycles > 1, s"ResetStretcher only supports cycles > 1 but got ${cycles}")

  lazy val module = new LazyModuleImp(this) {
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
