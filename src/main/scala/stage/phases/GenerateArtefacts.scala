// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import chisel3.stage.phases.Elaborate
import firrtl.AnnotationSeq
import firrtl.options.{Phase, PreservesAll, StageOptions}
import firrtl.options.Viewer.view
import freechips.rocketchip.util.ElaborationArtefacts

class GenerateArtefacts extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(classOf[Checks], classOf[Elaborate])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val targetDir = view[StageOptions](annotations).targetDir

    ElaborationArtefacts.files.foreach { case (extension, contents) =>
      writeOutputFile(targetDir, s"${getLongName(annotations)}.${extension}", contents ())
    }

    annotations
  }

}
