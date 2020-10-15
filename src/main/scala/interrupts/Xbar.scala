// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, ValName}

class IntXbarNode(
    inputRequiresOutput: Boolean = true,
    outputRequiresInput: Boolean = true)(
    implicit valName: ValName) extends IntNexusNode(
  sinkFn = { seq =>
    require(!seq.flatMap(_.sinks).exists(_.rangeOpt.isDefined),
      "IntXbar aggregates ranges from sources, but some sinks had defined ranges which would be lost.")
    IntSinkPortParameters(Seq(IntSinkParameters(None)))
  },
  sourceFn = { seq => IntSourcePortParameters(
    seq.zip(seq.map(_.num).scanLeft(0)(_+_).init).map {
      case (s, o) => s.sources.map(z => z.copy(rangeOpt = z.rangeOpt.map(_.offset(o))))
    }.flatten
  )},
  inputRequiresOutput = inputRequiresOutput,
  outputRequiresInput = outputRequiresInput)
{
  override def circuitIdentity = {
    outputs == 1 && inputs == 1 ||
    outputs == 0 && inputs == 0 ||
    !inputRequiresOutput && outputs == 0 ||
    !outputRequiresInput &&  inputs == 0
  }
}

class IntXbar()(implicit p: Parameters) extends LazyModule
{
  val intnode = new IntXbarNode()

  lazy val module = new LazyModuleImp(this) {
    val cat = intnode.in.map { case (i, e) => i.take(e.num) }.flatten
    intnode.out.foreach { case (o, _) => o := cat }
  }
}

object IntXbar {
  def apply(implicit p: Parameters): IntNode = {
    val xbar = LazyModule(new IntXbar)
    xbar.intnode
  }
}
