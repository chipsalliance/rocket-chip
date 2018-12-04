// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

class IntXbar()(implicit p: Parameters) extends LazyModule
{
  val intnode = IntNexusNode(
    sinkFn         = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    sourceFn       = { seq =>
      IntSourcePortParameters((seq zip seq.map(_.num).scanLeft(0)(_+_).init).map {
        case (s, o) => s.sources.map(z => z.copy(range = z.range.offset(o)))
      }.flatten)
    })

  lazy val module = new LazyModuleImp(this) {
    val cat = intnode.in.map { case (i, e) => i.take(e.source.num) }.flatten
    intnode.out.foreach { case (o, _) => o := cat }
  }
}

object IntXbar {
  def apply(implicit p: Parameters): IntNode = {
    val xbar = LazyModule(new IntXbar)
    xbar.intnode
  }
}
