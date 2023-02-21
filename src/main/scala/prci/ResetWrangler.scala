// See LICENSE for license details.
package freechips.rocketchip.prci

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class ResetWrangler(debounceNs: Double = 100000)(implicit p: Parameters) extends LazyModule
{
  val node = ClockAdapterNode()

  lazy val module = new Impl
  class Impl extends LazyRawModuleImp(this) {
    val (in, _) = node.in.unzip
    val (out, _) = node.out.unzip

    val status = IO(Output(UInt(in.size.W)))
    status := Cat(in.map(_.reset.asBool).reverse)
    val causes = in.map(_.reset).foldLeft(false.B)(_.asBool || _.asBool)

    require(node.in.forall(_._2.clock.isDefined), "Cannot wrangle reset for an unspecified clock frequency")
    val (slowIn, slowEdge) = node.in.minBy(_._2.clock.get.freqMHz)
    val slowPeriodNs = 1000 / slowEdge.clock.get.freqMHz
    val slowTicks = math.ceil(debounceNs/slowPeriodNs).toInt max 7
    val slowBits = log2Ceil(slowTicks+1)

    // debounce
    val increment = Wire(Bool())
    val incremented = Wire(UInt(slowBits.W))
    val debounced = withClockAndReset(slowIn.clock, causes) {
      AsyncResetReg(incremented, 0, increment, Some("debounce"))
    }
    increment := debounced =/= slowTicks.U
    incremented := debounced + 1.U
    val deglitched = AsyncResetReg(increment, slowIn.clock, causes, true, Some("deglitch"))

    // catch and sync increment to each domain
    (in zip out) foreach { case (i, o) =>
      o.clock := i.clock
      o.reset := ResetCatchAndSync(o.clock, deglitched)
    }
  }
}
