// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import chisel3.stage.phases.{Convert, Elaborate}
import firrtl.AnnotationSeq
import firrtl.options.Viewer.view
import firrtl.options.{Phase, PreservesAll, StageOptions}
import firrtl.stage.FirrtlCircuitAnnotation
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.util.HasRocketChipStageUtils

/** Dumps circuit as FIRRTL string into a file */
class GenerateFirrtl extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(classOf[Checks], classOf[Elaborate], classOf[Convert])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val targetDir = view[StageOptions](annotations).targetDir
    val fileName = s"${view[RocketChipOptions](annotations).longName.get}.fir"

    annotations.flatMap {
      case a: FirrtlCircuitAnnotation =>
        writeOutputFile(targetDir, fileName, a.circuit.serialize)
        Some(a)
      case a => Some(a)
    }
  }

}
