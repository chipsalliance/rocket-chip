// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import java.io.File

import chisel3.Driver
import chisel3.stage.ChiselCircuitAnnotation
import chisel3.stage.phases.Elaborate
import firrtl.AnnotationSeq
import firrtl.options.{Phase, PreservesAll, StageOptions}
import firrtl.options.Viewer.view
import freechips.rocketchip.stage.RocketChipOptions

class GenerateFirrtl extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(classOf[Checks], classOf[Elaborate])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val targetDir = view[StageOptions](annotations).targetDir
    val file = new File(targetDir, s"${view[RocketChipOptions](annotations).longName}.fir")

    annotations.flatMap {
      case a: ChiselCircuitAnnotation =>
        Driver.dumpFirrtl(a.circuit, Some(file))
        Some(a)
      case a => Some(a)
    }
  }

}
