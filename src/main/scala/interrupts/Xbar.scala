// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

class IntXbar()(implicit p: Parameters) extends LazyModule
{
  val intnode = new IntNexusNode(
    sinkFn         = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    sourceFn       = { seq =>
      IntSourcePortParameters((seq zip seq.map(_.num).scanLeft(0)(_+_).init).map {
        case (s, o) => s.sources.map(z => z.copy(range = z.range.offset(o)))
      }.flatten)
    })
  {
    override def circuitIdentity = outputs == 1 && inputs == 1
  }

  lazy val module = new Impl
  class Impl extends LazyRawModuleImp(this) {
    val cat = intnode.in.map { case (i, e) => i.take(e.source.num) }.flatten
    intnode.out.foreach { case (o, _) => o := cat }
  }
}

class IntSyncXbar()(implicit p: Parameters) extends LazyModule
{
  val intnode = new IntSyncNexusNode(
    sinkFn         = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    sourceFn       = { seq =>
      IntSourcePortParameters((seq zip seq.map(_.num).scanLeft(0)(_+_).init).map {
        case (s, o) => s.sources.map(z => z.copy(range = z.range.offset(o)))
      }.flatten)
    })
  {
    override def circuitIdentity = outputs == 1 && inputs == 1
  }

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val cat = intnode.in.map { case (i, e) => i.sync.take(e.source.num) }.flatten
    intnode.out.foreach { case (o, _) => o.sync := cat }
  }
}

object IntXbar {
  def apply()(implicit p: Parameters): IntNode = {
    val xbar = LazyModule(new IntXbar)
    xbar.intnode
  }
}

object IntSyncXbar {
  def apply()(implicit p: Parameters): IntSyncNode = {
    val xbar = LazyModule(new IntSyncXbar)
    xbar.intnode
  }
}
