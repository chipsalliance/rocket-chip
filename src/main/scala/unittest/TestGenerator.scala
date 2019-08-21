// See LICENSE.SiFive for license details.

package freechips.rocketchip.unittest

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

abstract class LazyUnitTest(implicit p: Parameters) extends LazyModule
{ self =>
  protected def finished: Bool

  lazy val module = new LazyModuleImp(this) {
    val finished = IO(Bool(OUTPUT))
    finished := self.finished
  }
}

// FYI, you can call .finished on a Seq[LazyUnitTest]
class TestGenerator(gen: LazyModule => Seq[LazyUnitTest])
{
  def apply(lm: LazyModule) = gen(lm)
  def ++ (other: TestGenerator) = new TestGenerator(gen = lm => gen(lm) ++ other(lm))
}

object TestGenerator
{
  def apply(matcher: PartialFunction[LazyModule, Seq[LazyUnitTest]]): TestGenerator =
    new TestGenerator(gen = matcher.lift(_).getOrElse(Nil))
  def recurse(other: TestGenerator): TestGenerator = {
    def helper(lm: LazyModule, tail: Seq[LazyUnitTest]): Seq[LazyUnitTest] =
      lm.getChildren.foldLeft(other(lm) ++ tail) { case (tail, child) => helper(child, tail) }
    new TestGenerator(gen = helper(_, Nil))
  }
}
