// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage

import firrtl.options.{Phase, PreservesAll, Shell, StageMain}
import firrtl.stage.FirrtlCli
import chisel3.stage.{ChiselCli, ChiselStage}
import firrtl.options.PhaseManager.PhaseDependency

class RocketChipStage extends ChiselStage with PreservesAll[Phase] {

  override val shell = new Shell("rocket-chip") with RocketChipCli with ChiselCli with FirrtlCli

  override val targets: Seq[PhaseDependency] = Seq(
    classOf[freechips.rocketchip.stage.phases.Checks],
    classOf[freechips.rocketchip.stage.phases.PreElaboration],
    classOf[chisel3.stage.phases.Checks],
    classOf[chisel3.stage.phases.Elaborate],
    classOf[freechips.rocketchip.stage.phases.GenerateFirrtl],
    classOf[freechips.rocketchip.stage.phases.GenerateFirrtlAnnos],
    classOf[freechips.rocketchip.stage.phases.GenerateTestSuiteMakefrags],
    classOf[freechips.rocketchip.stage.phases.GenerateROMs],
    classOf[freechips.rocketchip.stage.phases.GenerateArtefacts],
  )

}

object RocketChipMain extends StageMain(new RocketChipStage)
