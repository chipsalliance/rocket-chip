// See LICENSE for license details.

package freechips.rocketchip.transforms.naming

import firrtl._
import firrtl.annotations._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

import freechips.rocketchip.linting.rule.DesiredNameAnnotation

case class StableNameAnnotation(target: IsModule) extends SingleTargetAnnotation[IsModule] {
  def duplicate(newTarget: IsModule): StableNameAnnotation = this.copy(target = newTarget)
}

case class UnstableNameAnnotation(target: IsModule) extends SingleTargetAnnotation[IsModule] {
  def duplicate(newTarget: IsModule): UnstableNameAnnotation = this.copy(target = newTarget)
}

class RenameDesiredNamesSpec extends AnyPropSpec with Matchers {
  val transform = new RenameDesiredNames

  case class TestCase(
    input: String,
    annos: Seq[Annotation]
  )

  def renameNames(testCase: TestCase): CircuitState = {
    renameNames(CircuitState(Parser.parse(testCase.input), UnknownForm, testCase.annos))
  }

  def renameNames(state: CircuitState): CircuitState = {
    transform.runTransform(state)
  }

  def test(testCases: TestCase *): Unit = {
    val firstState = renameNames(testCases.head)
    val firstUnstableNames = firstState.annotations.collect {
      case a: UnstableNameAnnotation => a
    }
    val firstStableNames = firstState.annotations.collect {
      case a: StableNameAnnotation => a
    }

    testCases.tail.foldLeft(firstStableNames, firstUnstableNames) {
      case ((stable, unstable), testCase) =>
        val state = renameNames(testCase)
        val currUnstableNames = state.annotations.collect {
          case a: UnstableNameAnnotation => a
        }
        val currStableNames = state.annotations.collect {
          case a: StableNameAnnotation => a
        }

        currStableNames should be (stable)
        currUnstableNames should not be (unstable)
        (currStableNames, currUnstableNames)
    }
  }

  property("It should rename modules if it can and ignore strategies which fail to result in unique names") {
    val top = CircuitTarget("Foo")
    val testCase = TestCase(
      """|circuit Foo:
         |  module Bar_1:
         |    output in1: UInt<1>
         |  module Bar_2:
         |    output in1: UInt<1>
         |    output in2: UInt<1>
         |  module Bar_3:
         |    output in1: UInt<1>
         |    output in2: UInt<1>
         |  module Foo:
         |    inst bar_1 of Bar_1
         |    inst bar_2 of Bar_2
         |    inst bar_3 of Bar_3
         |""".stripMargin,
      Seq(
        DesiredNameAnnotation("Bar_1", top.module("Bar_1")),
        OverrideDesiredNameAnnotation("BarWith1Input", top.module("Bar_1")),

        // these renames should fail (be ignored) because the ExactNamingStrategy fails to result in unique names.
        OverrideDesiredNameAnnotation("BarWith2Inputs", top.module("Bar_2")),
        OverrideDesiredNameAnnotation("BarWith2Inputs", top.module("Bar_3"))
      )
    )
    val outputState = renameNames(testCase)
    val check =
      """|circuit Foo:
         |  module BarWith1Input:
         |    output in1: UInt<1>
         |  module Bar_2:
         |    output in1: UInt<1>
         |    output in2: UInt<1>
         |  module Bar_3:
         |    output in1: UInt<1>
         |    output in2: UInt<1>
         |  module Foo:
         |    inst bar_1 of BarWith1Input
         |    inst bar_2 of Bar_2
         |    inst bar_3 of Bar_3
         |""".stripMargin

    outputState.circuit should be (Parser.parse(check))

    // it should also update DesiredNameAnnotation and delete successfull OverrideDesiredNameAnnotation
    // Unsuccessful OverrideDesiredNameAnnotations should remain
    outputState.annotations.filterNot(_.isInstanceOf[DeletedAnnotation]) should be (Seq(
      DesiredNameAnnotation("BarWith1Input", top.module("BarWith1Input")),
      OverrideDesiredNameAnnotation("BarWith2Inputs", top.module("Bar_2")),
      OverrideDesiredNameAnnotation("BarWith2Inputs", top.module("Bar_3"))
    ))
  }

  property("It should keep modules names stable between runs") {
    val top = CircuitTarget("Foo")
    test(
      TestCase(
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
           |""".stripMargin,
        Seq(
          UnstableNameAnnotation(top.module("Bar_1")),
          StableNameAnnotation(top.module("Bar_2")),
          StableNameAnnotation(top.module("Bar_3")),
          OverrideDesiredNameAnnotation("Bar_2", top.module("Bar_2")),
          OverrideDesiredNameAnnotation("Bar_3", top.module("Bar_3"))
        )
      ),
      TestCase(
        """|circuit Foo:
           |  module Bar_1:
           |    output in1: UInt<2>
           |  module Bar_5:
           |    output in1: UInt<1>
           |    output in2: UInt<1>
           |  module Bar_6:
           |    output in1: UInt<1>
           |    output in2: UInt<1>
           |    output in3: UInt<1>
           |  module Foo:
           |    inst bar_1 of Bar_1
           |    inst bar_5 of Bar_5
           |    inst bar_6 of Bar_6
           |""".stripMargin,
        Seq(
          UnstableNameAnnotation(top.module("Bar_1")),
          StableNameAnnotation(top.module("Bar_5")),
          StableNameAnnotation(top.module("Bar_6")),
          OverrideDesiredNameAnnotation("Bar_4", top.module("Bar_1")),
          OverrideDesiredNameAnnotation("Bar_2", top.module("Bar_5")),
          OverrideDesiredNameAnnotation("Bar_3", top.module("Bar_6"))
        )
      )
    )
  }

  property("It should error if renaming to an already existing module name") {
    val top = CircuitTarget("Foo")
    val testCase = TestCase(
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
         |""".stripMargin,
      Seq(OverrideDesiredNameAnnotation("Bar_1", top.module("Bar_2")))
    )
    an [Exception] should be thrownBy renameNames(testCase)
  }
}
