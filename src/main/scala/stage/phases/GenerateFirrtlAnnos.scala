// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import firrtl.AnnotationSeq
import firrtl.annotations.{DeletedAnnotation, JsonProtocol}
import firrtl.options.Viewer.view
import firrtl.options._
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.util.HasRocketChipStageUtils

/** Writes FIRRTL annotations into a file */
class GenerateFirrtlAnnos extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(Dependency[freechips.rocketchip.system.RocketChiselStage])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val targetDir = view[StageOptions](annotations).targetDir
    val fileName = s"${view[RocketChipOptions](annotations).longName.get}.anno.json"

    val annos = annotations.view.flatMap {
      // Remove TargetDirAnnotation so that we can pass as argument to FIRRTL
      // Remove CustomFileEmission, those are serialized automatically by Stages
      case (_: Unserializable | _: TargetDirAnnotation | _: CustomFileEmission) =>
        None
      case DeletedAnnotation(_, (_: Unserializable | _: CustomFileEmission)) =>
        None
      case a =>
        Some(a)
    }

    writeOutputFile(targetDir, fileName, JsonProtocol.serialize(annos))

    annotations
  }

}
