// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import firrtl.AnnotationSeq
import firrtl.annotations.Annotation
import firrtl.options.{OptionsException, Phase, PreservesAll, TargetDirAnnotation}
import freechips.rocketchip.stage._

import scala.collection.mutable

/** Checks for the correct type and number of command line arguments */
class Checks extends Phase with PreservesAll[Phase] {

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val targetDir, topModule, configNames, outputBaseName = mutable.ListBuffer[Annotation]()

    annotations.foreach {
      case a: TargetDirAnnotation      => a +=: targetDir
      case a: TopModuleAnnotation      => a +=: topModule
      case a: ConfigsAnnotation        => a +=: configNames
      case a: OutputBaseNameAnnotation => a +=: outputBaseName
      case _ =>
    }

    def required(annoList: mutable.ListBuffer[Annotation], option: String): Unit = {
      if (annoList.size != 1) {
        throw new OptionsException(s"Exactly one $option required")
      }
    }

    def optional(annoList: mutable.ListBuffer[Annotation], option: String): Unit = {
      if (annoList.size > 1) {
        throw new OptionsException(s"Too many $option options have been specified")
      }
    }

    required(targetDir, "target directory")
    required(topModule, "top module")
    required(configNames, "configs string (','-delimited)")

    optional(outputBaseName, "output base name")

    annotations
  }

}
