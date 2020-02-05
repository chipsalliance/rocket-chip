// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import firrtl.AnnotationSeq
import firrtl.annotations.Annotation
import firrtl.options.{OptionsException, Phase, PreservesAll}
import freechips.rocketchip.stage._

import scala.collection.mutable

class Checks extends Phase with PreservesAll[Phase] {

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val targetDir, topModulePackage, topModuleClass, configsPackage, configs, outputBaseName = mutable.ListBuffer[Annotation]()

    annotations.foreach {
      case a: TargetDirectoryAnnotation => a +=: targetDir
      case a: TopPackageAnnotation      => a +=: topModulePackage
      case a: TopClassAnnotation        => a +=: topModuleClass
      case a: ConfigPackageAnnotation   => a +=: configsPackage
      case a: ConfigAnnotation          => a +=: configs
      case a: OutputBaseNameAnnotation  => a +=: outputBaseName
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
    required(topModulePackage, "top module")
    required(topModuleClass, "top class")
    required(configsPackage, "configs package")
    required(configs, "configs string ('_'-delimited)")

    optional(outputBaseName, "output base name")

    annotations
  }

}
