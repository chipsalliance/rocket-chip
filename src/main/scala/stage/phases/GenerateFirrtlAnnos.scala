// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import java.io.{File, FileWriter}

import chisel3.stage.ChiselCircuitAnnotation
import chisel3.stage.phases.Elaborate
import firrtl.AnnotationSeq
import firrtl.annotations.JsonProtocol
import firrtl.options.Viewer.view
import firrtl.options.{Phase, PreservesAll}
import freechips.rocketchip.stage.RocketChipOptions

class GenerateFirrtlAnnos extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(classOf[Checks], classOf[Elaborate])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val rOpts = view[RocketChipOptions](annotations)
    val targetDir = rOpts.targetDir.get
    val file = new File(targetDir, s"${getLongName(annotations)}.anno.json")

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
