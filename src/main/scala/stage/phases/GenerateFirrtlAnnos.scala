// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import chisel3.stage.phases.{Convert, Elaborate, MaybeAspectPhase}
import firrtl.AnnotationSeq
import firrtl.annotations.{Annotation, DeletedAnnotation, JsonProtocol}
import firrtl.options.Viewer.view
import firrtl.options.{Dependency, Phase, PreservesAll, StageOptions, TargetDirAnnotation, Unserializable}
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.util.HasRocketChipStageUtils

/** Writes FIRRTL annotations into a file */
class GenerateFirrtlAnnos extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(Dependency[Checks], Dependency[Elaborate], Dependency[Convert], Dependency[MaybeAspectPhase])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val targetDir = view[StageOptions](annotations).targetDir
    val fileName = s"${view[RocketChipOptions](annotations).longName.get}.anno.json"

    val annos = scala.collection.mutable.Buffer[Annotation]()
    annotations.flatMap {
      case a: Unserializable =>
        Some(a)
      case a: TargetDirAnnotation =>
        /** Don't serialize, in case of downstream FIRRTL call */
        Some(a)
      case a @ DeletedAnnotation(_, _: Unserializable) =>
        /** [[DeletedAnnotation]]s of unserializable annotations cannot be serialized */
        Some(a)
      case a =>
        annos += a
        Some(a)
    }

    writeOutputFile(targetDir, fileName, JsonProtocol.serialize(annos))

    annotations
  }

}
