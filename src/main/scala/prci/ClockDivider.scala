// See LICENSE.SiFive for license details.
package freechips.rocketchip.prci

import chisel3._
import chisel3.util.isPow2
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.{ClockDivider3, Pow2ClockDivider}

/* An example clock adapter that divides all clocks passed through this node by an integer factor
*/
class ClockDivider(div: Int)(implicit p: Parameters) extends LazyModule {
  val node = ClockAdapterNode(
    sourceFn = { case src => src.copy(give = src.give.map(x => x.copy(freqMHz = x.freqMHz / 2))) },
    sinkFn   = { case snk => snk.copy(take = snk.take.map(x => x.copy(freqMHz = x.freqMHz * 2))) })

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out).foreach { case ((in, _), (out, _)) =>
      val div_clock: Clock = div match {
        case x if isPow2(x) => Pow2ClockDivider(in.clock, x)
        case 3 => {
          val div3 = Module(new ClockDivider3)
          div3.io.clk_in := in.clock
          div3.io.clk_out
        }
        case x => throw new IllegalArgumentException(s"rocketchip.util only supports clock division by powers of 2, or exactly 3, but got $x")
      }
      out.clock := div_clock
      out.reset := withClock(out.clock) { RegNext(in.reset) }
    }
  }
}

// TODO make a version of this that output a clock group with two members,
//      one of which is the original clock and the other of which is some number of divided clocks
