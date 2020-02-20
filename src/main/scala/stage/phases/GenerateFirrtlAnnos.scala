// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import java.io.{File, FileWriter}

import chisel3.stage.ChiselCircuitAnnotation
import chisel3.stage.phases.Elaborate
import firrtl.AnnotationSeq
import firrtl.annotations.JsonProtocol
import firrtl.options.Viewer.view
import firrtl.options.{Phase, PreservesAll, StageOptions}
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.util.HasRocketChipStageUtils

/** Writes FIRRTL annotations into a file */
class GenerateFirrtlAnnos extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(classOf[Checks], classOf[Elaborate])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val targetDir = view[StageOptions](annotations).targetDir
    val file = new File(targetDir, s"${view[RocketChipOptions](annotations).longName}.anno.json")

    annotations.flatMap {
      case a: ChiselCircuitAnnotation =>
        val af = new FileWriter(file)
        af.write(JsonProtocol.serialize(a.circuit.annotations.map(_.toFirrtl)))
        af.close()
        Some(a)
      case a => Some(a)
    }
  }

}
