// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage

import firrtl.AnnotationSeq
import firrtl.options.{Phase, PreservesAll, Shell, Stage, StageMain}
import firrtl.stage.FirrtlCli

import chisel3.stage.{ChiselCli, ChiselStage}

class RocketChipStage extends Stage with PreservesAll[Phase] {

  override val shell = new Shell("rocket-chip") with RocketChipCli with ChiselCli with FirrtlCli

  override def run(annotations: AnnotationSeq): AnnotationSeq =
    Seq(
      new freechips.rocketchip.stage.phases.Checks,
      new freechips.rocketchip.stage.phases.PreElaboration,
      new chisel3.stage.ChiselStage
    ).foldLeft(annotations){ case (a, p) => p.transform(a) }

}

object RocketChipFoo extends App {

  (new RocketChipStage).execute(args, Seq.empty)

}

object RocketChipMain extends StageMain(new RocketChipStage)
