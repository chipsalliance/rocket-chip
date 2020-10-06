// See LICENSE for license details.

package freechips.rocketchip.linting.rule

import firrtl._
import firrtl.annotations._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

import freechips.rocketchip.linting.Violation
import freechips.rocketchip.transforms.naming.{OverrideDesiredNameAnnotation, RenameDesiredNames}

class LintConflictingModuleNamesSpec extends AnyPropSpec with Matchers {
  val transform = new LintConflictingModuleNames
  def lint(input: String, annos: Seq[Annotation]): CircuitState = {
    val state = CircuitState(Parser.parse(input), UnknownForm, annos)
    transform.runTransform(state)
  }

  def lint(state: CircuitState): CircuitState = {
    transform.runTransform(state)
  }

  val renameTransform = new RenameDesiredNames

  def rename(input: String, annos: Seq[Annotation]): CircuitState = {
    val state = CircuitState(Parser.parse(input), UnknownForm, annos)
    renameTransform.runTransform(state)
  }

  property("It should emit LintViolations for conflicting DesiredNameAnnotations") {
    val top = CircuitTarget("Foo")
    val input =
      """|circuit Foo:
         |  module Bar_1:
         |    output in1: UInt<1>
         |  module Bar_2:
         |    output in1: UInt<1>
         |    output in2: UInt<1>
         |  module Bar_3:
         |    output in1: UInt<1>
         |    output in2: UInt<1>
         |    output in3: UInt<1>
         |  module Foo:
         |    inst bar_1 of Bar_1
         |    inst bar_2 of Bar_2
         |    inst bar_3 of Bar_3
         |""".stripMargin
    val annos = Seq(
      DesiredNameAnnotation("Bar_1", top.module("Bar_1")),
      DesiredNameAnnotation("Bar_1", top.module("Bar_2"))
    )
    val conflictingModules = lint(input, annos).annotations.collect {
      case Violation(_: LintConflictingModuleNames, _, _, mods) => mods
    }
    conflictingModules should be (Seq(Set("Bar_1", "Bar_2")))
  }

  property("It should catch LintViolations not fixed by with RenameDesiredNames") {
    val top = CircuitTarget("Foo")
    val input =
      """|circuit Foo:
         |  module Bar_1:
         |    output in1: UInt<1>
         |    output in2: UInt<1>
         |  module Bar_2:
         |    output in1: UInt<2>
         |    output in2: UInt<2>
         |  module Foo:
         |    inst bar_1 of Bar_1
         |    inst bar_2 of Bar_2
         |""".stripMargin
    val annos = Seq(
      DesiredNameAnnotation("Bar_1", top.module("Bar_1")),
      DesiredNameAnnotation("Bar_1", top.module("Bar_2")),
      OverrideDesiredNameAnnotation("BarWith2Inputs", top.module("Bar_1")),
      OverrideDesiredNameAnnotation("BarWith2Inputs", top.module("Bar_2"))
    )

    val conflictingModules = lint(rename(input, annos)).annotations.collect {
      case Violation(_: LintConflictingModuleNames, _, _, mods) => mods
    }
    conflictingModules should be (Seq(Set("Bar_1", "Bar_2")))
  }

  property("RenameDesiredNames should be able to fix LintViolations") {
    val top = CircuitTarget("Foo")
    val input =
      """|circuit Foo:
         |  module Bar_1:
         |    output in1: UInt<1>
         |    output in2: UInt<1>
         |  module Bar_2:
         |    output in1: UInt<2>
         |    output in2: UInt<2>
         |  module Foo:
         |    inst bar_1 of Bar_1
         |    inst bar_2 of Bar_2
         |""".stripMargin
    val annos = Seq(
      DesiredNameAnnotation("Bar_1", top.module("Bar_1")),
      DesiredNameAnnotation("Bar_1", top.module("Bar_2")),
      OverrideDesiredNameAnnotation("BarWith2BoolInputs", top.module("Bar_1")),
      OverrideDesiredNameAnnotation("BarWith2UIntInputs", top.module("Bar_2"))
    )

    val conflictingModules = lint(rename(input, annos)).annotations.collect {
      case Violation(_: LintConflictingModuleNames, _, _, mods) => mods
    }
    conflictingModules should be (Seq())
  }
}
