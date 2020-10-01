// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import firrtl.AnnotationSeq
import firrtl.options.{Dependency, Phase, PreservesAll, StageOptions}
import firrtl.options.Viewer.view
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.util.{ElaborationArtefacts, HasRocketChipStageUtils}

/** Writes [[ElaborationArtefacts]] into files */
class GenerateArtefacts extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(Dependency[freechips.rocketchip.system.RocketChiselStage])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val targetDir = view[StageOptions](annotations).targetDir

    ElaborationArtefacts.files.foreach { case (extension, contents) =>
      writeOutputFile(targetDir, s"${view[RocketChipOptions](annotations).longName.get}.${extension}", contents ())
    }

    annotations
  }

}
