// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage

import chisel3.experimental.BaseModule

import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}

import freechips.rocketchip.config.Config

sealed trait RocketChipOption extends Unserializable { this: Annotation => }

case class TopModuleAnnotation(clazz: Class[_ <: Any]) extends NoTargetAnnotation with RocketChipOption

private[stage] object TopModuleAnnotation extends HasShellOptions {

  override val options = Seq(
    new ShellOption[String](
      longOption = "top-module",
      toAnnotationSeq = a => Seq(TopModuleAnnotation(Class.forName(a).asInstanceOf[Class[_ <: BaseModule]])),
      helpText = "The top module to build, e.g., freechips.rocketchip.system.TestHarness",
      helpValueName = Some("<module>")
    )
  )

}

case class ConfigAnnotation(clazz: Class[_ <: Config]) extends NoTargetAnnotation with RocketChipOption

private[stage] object ConfigAnnotation extends HasShellOptions {

  override val options = Seq(
    new ShellOption[String](
      longOption = "config",
      toAnnotationSeq = a => Seq(ConfigAnnotation(Class.forName(a).asInstanceOf[Class[_ <: Config]])),
      helpText = "The configuration to use, e.g., freechips.rocketchip.system.DefaultConfig",
      helpValueName = Some("<config>")
    )
  )

}
