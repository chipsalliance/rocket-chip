// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage

import chisel3.experimental.BaseModule
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}

sealed trait RocketChipOption extends Unserializable { this: Annotation => }

/* required options */

/** Path to top module class */
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

/** Paths to config classes */
case class ConfigsAnnotation(configNames: Seq[String]) extends NoTargetAnnotation with RocketChipOption
private[stage] object ConfigsAnnotation extends HasShellOptions {
  override val options = Seq(
    new ShellOption[Seq[String]](
      longOption = "configs",
      toAnnotationSeq = a => Seq(ConfigsAnnotation(a)),
      helpText = "<comma-delimited configs>",
      shortOption = Some("C")
    )
  )
}

/* optional options */

/** Optional base name for generated files' filenames */
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
