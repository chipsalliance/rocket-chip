// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import chisel3.stage.ChiselCircuitAnnotation
import chisel3.stage.phases.Elaborate
import firrtl.AnnotationSeq
import firrtl.options.{Phase, PreservesAll}
import firrtl.options.Viewer.view
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.util.HasGeneratorUtilities

class GenerateROMs extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils with HasGeneratorUtilities {

  override val prerequisites = Seq(classOf[Elaborate])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val rOpts = view[RocketChipOptions](annotations)
    val targetDir = rOpts.targetDir.get
    val fileName = s"${getLongName(annotations)}.rom.conf"

    annotations.flatMap {
      case a: ChiselCircuitAnnotation =>
        writeOutputFile(targetDir, fileName, enumerateROMs(a.circuit))
        Some(a)
      case a => Some(a)
    }
  }

}
