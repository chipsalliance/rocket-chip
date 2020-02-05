// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import chisel3.stage.phases.Elaborate
import firrtl.AnnotationSeq
import firrtl.options.{Phase, PreservesAll}
import firrtl.options.Viewer.view
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.system.TestGeneration

class GenerateTestSuiteMakefrags extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(classOf[Checks], classOf[Elaborate])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val entOpts = view[RocketChipOptions](annotations)
    val targetDir = entOpts.targetDir.get
    val fileName = s"${getLongName(annotations)}.d"

    addTestSuites(annotations)
    writeOutputFile(targetDir, fileName, TestGeneration.generateMakefrag)

    annotations
  }

}
