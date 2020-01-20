// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import firrtl.AnnotationSeq
import firrtl.options.{Phase, StageOptions}
import firrtl.options.Viewer.view

import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.system.{RocketTestSuite, TestGeneration}

import java.io.PrintWriter

case class RocketTestSuiteAnnotation(suite: RocketTestSuite)

class AddTestSuites extends Phase {

  def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val sopts = view[StageOptions](annotations)
    val ropts = view[RocketChipOptions](annotations)

    println(ropts.config, ropts.topModule)

    val w = new PrintWriter(sopts.getBuildFileName(ropts.longName.get, Some(".d")))
    val makefrag =
      annotations
        .collect{ case a: RocketTestSuiteAnnotation => a.suite }
        .groupBy(_.kind)
        .map { case (kind, s) => TestGeneration.gen(kind, s) }
        .mkString("\n")
    w.write(makefrag)
    w.close()

    annotations
  }


}
