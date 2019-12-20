// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage.phases

import firrtl.AnnotationSeq
import firrtl.annotations.Annotation
import firrtl.options.{OptionsException, Phase}

import freechips.rocketchip.stage.{ConfigAnnotation, TopModuleAnnotation}

import scala.collection.mutable

class Checks extends Phase {

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val topM, conf = mutable.ListBuffer[Annotation]()
    annotations.foreach {
      case a: TopModuleAnnotation => a +=: topM
      case a: ConfigAnnotation    => a +=: conf
      case _                      =>
    }

    if (topM.size != 1) {
      val x = topM.map{ case TopModuleAnnotation(x) => x }
      val (msg, suggest) = topM.size match {
        case 0 => ("none found",                       "forget one of")
        case _ => (s"""found '${x.mkString(", ")}'""", "use multiple of")
      }
      throw new OptionsException(
        s"""|Exactly one top module be specified, but $msg. Did you $suggest the following?
            |    - an option or annotation: --top-module, TopModuleAnnotation""".stripMargin )}

    if (conf.size != 1) {
      val x = conf.map{ case ConfigAnnotation(x) => x }
      val (msg, suggest) = conf.size match {
        case 0 => ("none found",                       "forget one of")
        case _ => (s"""found '${x.mkString(", ")}'""", "use multiple of")
      }
      throw new OptionsException(
        s"""|Exactly one configuration must be specified, but $msg. Did you $suggest the following?
            |    - an option or annotation: --config, ConfigAnnotation""".stripMargin )}

    annotations
  }

}
