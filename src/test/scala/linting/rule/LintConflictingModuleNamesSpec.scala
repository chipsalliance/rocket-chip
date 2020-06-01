// See LICENSE for license details.

package freechips.rocketchip.linting.rule

import firrtl._
import firrtl.ir.Port
import firrtl.annotations._
import firrtl.testutils.{FirrtlMatchers, FirrtlPropSpec}

import freechips.rocketchip.linting.Violation

class LintConflictingModuleNamesSpec extends FirrtlPropSpec with FirrtlMatchers {
  val transform = new LintConflictingModuleNames
  def lint(input: String, annos: Seq[Annotation]): CircuitState = {
    val state = CircuitState(Parser.parse(input), UnknownForm, annos)
    transform.runTransform(state)
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
}
