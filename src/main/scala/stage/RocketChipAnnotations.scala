// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage

import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}

sealed trait RocketChipOption extends Unserializable { this: Annotation => }

/* required options */

case class TargetDirectoryAnnotation(targetDir: String) extends NoTargetAnnotation with RocketChipOption
private[stage] object TargetDirectoryAnnotation extends HasShellOptions {
  override val options = Seq(
    new ShellOption[String](
      longOption = "target",
      toAnnotationSeq = a => Seq(TargetDirectoryAnnotation(a)),
      helpText = "<target directory>",
      shortOption = Some("d")
    )
  )
}

case class TopPackageAnnotation(topModulePackage: String) extends NoTargetAnnotation with RocketChipOption
private[stage] object TopPackageAnnotation extends HasShellOptions {
  override val options = Seq(
    new ShellOption[String](
      longOption = "top-package",
      toAnnotationSeq = a => Seq(TopPackageAnnotation(a)),
      helpText = "<top module package>",
      shortOption = Some("T")
    )
  )
}

case class TopClassAnnotation(topModuleClass: String) extends NoTargetAnnotation with RocketChipOption
private[stage] object TopClassAnnotation extends HasShellOptions {
  override val options = Seq(
    new ShellOption[String](
      longOption = "top-class",
      toAnnotationSeq = a => Seq(TopClassAnnotation(a)),
      helpText = "<top module class name>",
      shortOption = Some("t")
    )
  )
}

case class ConfigPackageAnnotation(configsPackage: String) extends NoTargetAnnotation with RocketChipOption
private[stage] object ConfigPackageAnnotation extends HasShellOptions {
  override val options = Seq(
    new ShellOption[String](
      longOption = "configs-package",
      toAnnotationSeq = a => Seq(ConfigPackageAnnotation(a)),
      helpText = "<config classes' package>",
      shortOption = Some("C")
    )
  )
}

case class ConfigAnnotation(configs: String) extends NoTargetAnnotation with RocketChipOption
private[stage] object ConfigAnnotation extends HasShellOptions {
  override val options = Seq(
    new ShellOption[String](
      longOption = "configs",
      toAnnotationSeq = a => Seq(ConfigAnnotation(a)),
      helpText = "<'_'-delimited config class names>",
      shortOption = Some("c")
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
