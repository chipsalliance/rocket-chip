// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage

import chipsalliance.rocketchip.config.Config
import chisel3.experimental.BaseModule
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import freechips.rocketchip.stage.phases.HasRocketChipStageUtils

sealed trait RocketChipOption extends Unserializable { this: Annotation => }

/* required options */

case class TopModuleAnnotation(clazz: Class[_ <: Any]) extends NoTargetAnnotation with RocketChipOption
private[stage] object TopModuleAnnotation extends HasShellOptions {
  override val options = Seq(
    new ShellOption[String](
      longOption = "top-module",
      toAnnotationSeq = a => Seq(TopModuleAnnotation(Class.forName(a).asInstanceOf[Class[_ <: BaseModule]])),
      helpText = "<top module>",
      shortOption = Some("T")
    )
  )
}

case class ConfigsAnnotation(configNames: String) extends NoTargetAnnotation with RocketChipOption
private[stage] object ConfigsAnnotation extends HasShellOptions {
  override val options = Seq(
    new ShellOption[String](
      longOption = "configs",
      toAnnotationSeq = a => Seq(ConfigsAnnotation(a)),
      helpText = "<comma-delimited configs>",
      shortOption = Some("C")
    )
  )
}

/* optional options */

case class OutputBaseNameAnnotation(outputBaseName: String) extends NoTargetAnnotation with RocketChipOption
private[stage] object OutputBaseNameAnnotation extends HasShellOptions {
  override val options = Seq(
    new ShellOption[String](
      longOption = "name",
      toAnnotationSeq = a => Seq(OutputBaseNameAnnotation(a)),
      helpText = "<base name of output files>",
      shortOption = Some("n")
    )
  )
}
