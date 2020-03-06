// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import chisel3.stage.ChiselCircuitAnnotation
import chisel3.stage.phases.{Elaborate, MaybeAspectPhase}
import firrtl.AnnotationSeq
import firrtl.options.{Phase, PreservesAll, StageOptions}
import firrtl.options.Viewer.view
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.util.HasRocketChipStageUtils

class GenerateInjectModule extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(classOf[Checks], classOf[Elaborate])
//  override val prerequisites = Seq(classOf[Checks], classOf[Elaborate], classOf[MaybeAspectPhase])
//  override val dependents = Seq(classOf[MaybeAspectPhase])


  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val targetDir = view[StageOptions](annotations).targetDir

    annotations.flatMap {
      case a: ChiselCircuitAnnotation => 
        injectModule(targetDir, a.circuit)
        Some(a)
      case a => Some(a)
    }
  }

  println("DEBUG_AOP: SULTAN GenerateInjectModule")
}

