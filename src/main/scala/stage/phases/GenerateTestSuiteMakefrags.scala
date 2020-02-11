// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import chisel3.stage.phases.Elaborate
import firrtl.AnnotationSeq
import firrtl.options.{Phase, PreservesAll, StageOptions}
import firrtl.options.Viewer.view
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.system.{RocketTestSuite, TestGeneration}
import freechips.rocketchip.util.HasRocketChipStageUtils

case class RocketTestSuiteAnnotation(suite: RocketTestSuite)

class GenerateTestSuiteMakefrags extends Phase with PreservesAll[Phase] with HasRocketChipStageUtils {

  override val prerequisites = Seq(classOf[Checks], classOf[Elaborate])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val targetDir = view[StageOptions](annotations).targetDir
    val fileName = s"${view[RocketChipOptions](annotations).longName}.d"

    val makefrag =
      annotations
        .collect{ case a: RocketTestSuiteAnnotation => a.suite }
        .groupBy(_.kind)
        .map { case (kind, s) => TestGeneration.gen(kind, s) }
        .mkString("\n")
    writeOutputFile(targetDir, fileName, makefrag)

    annotations
  }

}
