// See LICENSE for license details.

package freechips.rocketchip.firrtl

import _root_.firrtl._
import _root_.firrtl.annotations._
import _root_.firrtl.testutils.{FirrtlMatchers, FirrtlPropSpec}

case class StableNameAnnotation(target: IsModule) extends SingleTargetAnnotation[IsModule] {
  def duplicate(newTarget: IsModule): StableNameAnnotation = this.copy(target = newTarget)
}

case class UnstableNameAnnotation(target: IsModule) extends SingleTargetAnnotation[IsModule] {
  def duplicate(newTarget: IsModule): UnstableNameAnnotation = this.copy(target = newTarget)
}

class StabilizeModuleNamesSpec extends FirrtlPropSpec with FirrtlMatchers {
  val transform = new StabilizeModuleNames

  case class TestCase(
    input: String,
    annos: Seq[Annotation]
  )

  def stabilizeNames(testCase: TestCase): CircuitState = {
    stabilizeNames(CircuitState(Parser.parse(testCase.input), UnknownForm, testCase.annos))
  }

  def stabilizeNames(state: CircuitState): CircuitState = {
    transform.runTransform(state)
  }

  def test(testCases: TestCase *): Unit = {
    val firstState = stabilizeNames(testCases.head)
    val firstUnstableNames = firstState.annotations.collect {
      case a: UnstableNameAnnotation => a
    }
    val firstStableNames = firstState.annotations.collect {
      case a: StableNameAnnotation => a
    }

    testCases.tail.foldLeft(firstStableNames, firstUnstableNames) {
      case ((stable, unstable), testCase) =>
        val state = stabilizeNames(testCase)
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


  property("It should rename modules to original name if only one module") {
    val input =
    """|circuit Foo:
       |  module Bar_1:
       |    skip
       |  module Foo:
       |    inst bar_1 of Bar_1
       |    inst bar_2 of Bar_1
       |""".stripMargin

    val top = CircuitTarget("Foo")
    val annos = Seq(
      ModuleNameAnnotation("Bar", top.module("Foo").instOf("bar_1", "Bar_1")),
      ModuleNameAnnotation("Bar", top.module("Foo").instOf("bar_2", "Bar_1"))
    )
    val inputState = CircuitState(passes.ToWorkingIR.run(Parser.parse(input)), UnknownForm, annos)
    val outputState = stabilizeNames(inputState)
    val output = 
    """|circuit Foo:
       |  module Bar:
       |    skip
       |  module Foo:
       |    inst bar_1 of Bar
       |    inst bar_2 of Bar
       |""".stripMargin
    outputState.circuit.serialize should be (Parser.parse(output).serialize)
  }

  property("It should rename modules to stable IO structure names") {
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
         ModuleNameAnnotation("Bar", top.module("Bar_1")),
         ModuleNameAnnotation("Bar", top.module("Bar_2")),
         ModuleNameAnnotation("Bar", top.module("Bar_3"))
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
          ModuleNameAnnotation("Bar", top.module("Bar_1")),
          ModuleNameAnnotation("Bar", top.module("Bar_5")),
          ModuleNameAnnotation("Bar", top.module("Bar_6"))
        )
      )
    )
  }

  property("It should rename modules to stable content structure names") {
    val top = CircuitTarget("Foo")
    test(
      TestCase(
        """|circuit Foo:
           |  module Bar_1:
           |    node dummy = UInt<1>(0)
           |  module Bar_2:
           |    node dummy = UInt<2>(0)
           |  module Bar_3:
           |    node dummy = UInt<3>(0)
           |  module Foo:
           |    inst bar_1 of Bar_1
           |    inst bar_2 of Bar_2
           |    inst bar_3 of Bar_3
           |""".stripMargin,
        Seq(
         UnstableNameAnnotation(top.module("Bar_1")),
         StableNameAnnotation(top.module("Bar_2")),
         StableNameAnnotation(top.module("Bar_3")),
         ModuleNameAnnotation("Bar", top.module("Bar_1")),
         ModuleNameAnnotation("Bar", top.module("Bar_2")),
         ModuleNameAnnotation("Bar", top.module("Bar_3"))
        )
      ),
      TestCase(
        """|circuit Foo:
           |  module Bar_1:
           |    node dummy = UInt<7>(0)
           |  module Bar_5:
           |    node dummy = UInt<2>(0)
           |  module Bar_6:
           |    node dummy = UInt<3>(0)
           |  module Foo:
           |    inst bar_1 of Bar_1
           |    inst bar_5 of Bar_5
           |    inst bar_6 of Bar_6
           |""".stripMargin,
        Seq(
          UnstableNameAnnotation(top.module("Bar_1")),
          StableNameAnnotation(top.module("Bar_5")),
          StableNameAnnotation(top.module("Bar_6")),
          ModuleNameAnnotation("Bar", top.module("Bar_1")),
          ModuleNameAnnotation("Bar", top.module("Bar_5")),
          ModuleNameAnnotation("Bar", top.module("Bar_6"))
        )
      )
    )
  }

  property("It should rename modules to stable content names") {
    val top = CircuitTarget("Foo")
    test(
      TestCase(
        """|circuit Foo:
           |  module Bar_1:
           |    node dummy_1 = UInt<1>(0)
           |  module Bar_2:
           |    node dummy_2 = UInt<1>(0)
           |  module Bar_3:
           |    node dummy_3 = UInt<1>(0)
           |  module Foo:
           |    inst bar_1 of Bar_1
           |    inst bar_2 of Bar_2
           |    inst bar_3 of Bar_3
           |""".stripMargin,
        Seq(
         UnstableNameAnnotation(top.module("Bar_1")),
         StableNameAnnotation(top.module("Bar_2")),
         StableNameAnnotation(top.module("Bar_3")),
         ModuleNameAnnotation("Bar", top.module("Bar_1")),
         ModuleNameAnnotation("Bar", top.module("Bar_2")),
         ModuleNameAnnotation("Bar", top.module("Bar_3"))
        )
      ),
      TestCase(
        """|circuit Foo:
           |  module Bar_1:
           |    node dummy___1 = UInt<1>(0)
           |  module Bar_5:
           |    node dummy_2 = UInt<1>(0)
           |  module Bar_6:
           |    node dummy_3 = UInt<1>(0)
           |  module Foo:
           |    inst bar_1 of Bar_1
           |    inst bar_5 of Bar_5
           |    inst bar_6 of Bar_6
           |""".stripMargin,
        Seq(
          UnstableNameAnnotation(top.module("Bar_1")),
          StableNameAnnotation(top.module("Bar_5")),
          StableNameAnnotation(top.module("Bar_6")),
          ModuleNameAnnotation("Bar", top.module("Bar_1")),
          ModuleNameAnnotation("Bar", top.module("Bar_5")),
          ModuleNameAnnotation("Bar", top.module("Bar_6"))
        )
      )
    )
  }
}
