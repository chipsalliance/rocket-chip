// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.SynchronizerShiftReg
import freechips.rocketchip.diplomacy._

@deprecated("IntXing does not ensure interrupt source is glitch free. Use IntSyncSource and IntSyncSink", "rocket-chip 1.2")
class IntXing(sync: Int = 3)(implicit p: Parameters) extends LazyModule
{
  val intnode = IntAdapterNode()

  lazy val module = new LazyModuleImp(this) {
    (intnode.in zip intnode.out) foreach { case ((in, _), (out, _)) =>
      out := SynchronizerShiftReg(in, sync)
    }
  }
}

class IntSyncCrossingSource(alreadyRegistered: Boolean = false)(implicit p: Parameters) extends LazyModule
{
  val node = IntSyncSourceNode()

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      if (alreadyRegistered) {
        out.sync := in
      } else {
        out.sync := RegNext(in)
      }
    }
  }
}

class IntSyncCrossingSink(sync: Int = 3)(implicit p: Parameters) extends LazyModule
{
  val node = IntSyncSinkNode()

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out := SynchronizerShiftReg(in.sync, sync)
    }
  }
}
