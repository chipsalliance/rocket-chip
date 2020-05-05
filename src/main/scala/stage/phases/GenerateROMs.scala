// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import chisel3.stage.ChiselCircuitAnnotation
import chisel3.stage.phases.{Convert, Elaborate}
import firrtl.AnnotationSeq
import firrtl.options.{Dependency, Phase, PreservesAll, StageOptions}
import firrtl.options.Viewer.view
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.util.HasRocketChipStageUtils

/** Dumps ROM information into a file */
class GenerateROMs extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(Dependency[Checks], Dependency[Elaborate])
  override val dependents = Seq(Dependency[Convert])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val targetDir = view[StageOptions](annotations).targetDir
    val fileName = s"${view[RocketChipOptions](annotations).longName.get}.rom.conf"

    annotations.flatMap {
      case a: ChiselCircuitAnnotation =>
        writeOutputFile(targetDir, fileName, enumerateROMs(a.circuit))
        Some(a)
      case a => Some(a)
    }
  }

}
