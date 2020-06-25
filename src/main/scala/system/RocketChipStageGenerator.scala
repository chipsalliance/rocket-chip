// See LICENSE.SiFive for license details.

package freechips.rocketchip.system

import chisel3.stage.{ChiselCli, ChiselStage}
import firrtl.AnnotationSeq
import firrtl.options.PhaseManager.PhaseDependency
import firrtl.options.{Dependency, Phase, PhaseManager, PreservesAll, Shell, Stage, StageMain}
import firrtl.stage.FirrtlCli
import freechips.rocketchip.stage.RocketChipCli

/** Modified ChiselStage that includes the GenerateROMs phase */
private[freechips] final class RocketChiselStage extends ChiselStage {

  override val targets = Seq(
    Dependency[chisel3.stage.phases.Checks],
    Dependency[chisel3.stage.phases.Elaborate],
    Dependency[freechips.rocketchip.stage.phases.GenerateROMs],
    Dependency[chisel3.stage.phases.AddImplicitOutputFile],
    Dependency[chisel3.stage.phases.AddImplicitOutputAnnotationFile],
    Dependency[chisel3.stage.phases.MaybeAspectPhase],
    Dependency[chisel3.stage.phases.Emitter],
    Dependency[chisel3.stage.phases.Convert]
  )

}

class RocketChipStage extends Stage with PreservesAll[Phase] {

  override val shell = new Shell("rocket-chip") with RocketChipCli with ChiselCli with FirrtlCli
  val targets: Seq[PhaseDependency] = Seq(
    Dependency[freechips.rocketchip.stage.phases.Checks],
    Dependency[freechips.rocketchip.stage.phases.TransformAnnotations],
    Dependency[freechips.rocketchip.stage.phases.PreElaboration],
    Dependency[RocketChiselStage],
    Dependency[freechips.rocketchip.stage.phases.GenerateFirrtlAnnos],
    Dependency[freechips.rocketchip.stage.phases.AddDefaultTests],
    Dependency[freechips.rocketchip.stage.phases.GenerateTestSuiteMakefrags],
    Dependency[freechips.rocketchip.stage.phases.GenerateArtefacts]
  )

  private val pm = new PhaseManager(targets)

  override def run(annotations: AnnotationSeq): AnnotationSeq = pm.transform(annotations)

}

object Generator extends StageMain(new RocketChipStage)
