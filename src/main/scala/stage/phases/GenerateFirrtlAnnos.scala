// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import java.io.{File, FileWriter}

import chisel3.stage.phases.{Elaborate, MaybeAspectPhase}
import firrtl.AnnotationSeq
import firrtl.annotations.{DeletedAnnotation, JsonProtocol}
import firrtl.options.Viewer.view
import firrtl.options.{Phase, PreservesAll, StageOptions, Unserializable}
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.util.HasRocketChipStageUtils

/** Writes FIRRTL annotations into a file */
class GenerateFirrtlAnnos extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(classOf[Checks], classOf[Elaborate], classOf[MaybeAspectPhase])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val targetDir = view[StageOptions](annotations).targetDir
    val fileName = s"${view[RocketChipOptions](annotations).longName.get}.anno.json"

    val f = new File(targetDir, fileName)
    val fw = new FileWriter(f)
    annotations.flatMap {
      case a: Unserializable =>
        Some(a)
      case a @ DeletedAnnotation(_, _: Unserializable) =>
        /** [[DeletedAnnotation]]s of unserializable annotations cannot be serialized */
        Some(a)
      case a =>
        fw.write(JsonProtocol.serialize(Seq(a)))
        Some(a)
    }
    fw.close

    annotations
  }

}
