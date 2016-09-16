// See LICENSE for license details.

package rocketchip

import firrtl._
import firrtl.ir._
import firrtl.passes.Pass
import firrtl.Annotations.AnnotationMap
import java.io.Writer

// The Rocket Chip pass manager: a simple pass manager that allows various
// FIRRTL passes to be inserted at various points throughout the FIRRTL build
// process on the way to Verilog.  It's expected that users will extend this
// class in order to specialize their passes.
class RocketChipPassManager extends firrtl.Compiler {
  private def convertToTransforms(ors: Seq[Either[Pass, Transform]]): Seq[Transform] = {
    class SinglePassTransform(pass: Pass) extends Transform with SimpleRun {
      def execute(circuit: Circuit, annotationMap: AnnotationMap): TransformResult =
        run(circuit, Seq(pass))
    }

    def convertToTransform(either: Either[Pass, Transform]): Transform =
      either match {
        case Right(t) => t
        case Left(p)  => new SinglePassTransform(p)
      }

    ors.map( or => convertToTransform(or) )
  }

  // This is some sugar to users don't need to actually know ther's a tagged
  // union in here to allow them to transparently provide either Pass or
  // Transform instances to the pass manager.
  implicit def l2Either[L, R](l: L): Either[L, R] = Left(l)
  implicit def r2Either[L, R](r: R): Either[L, R] = Right(r)

  // These functions are designed to be overridden by users.  They return a
  // sequence of transformations that will be applied at various FIRRTL levels.
  // A transform that operates at a FIRRTL level is expected to both produce
  // and consume FIRRTL at that level.
  def operateHigh():   Seq[Either[Pass, Transform]] = Seq()
  def operateMiddle(): Seq[Either[Pass, Transform]] = Seq()
  def operateLow():    Seq[Either[Pass, Transform]] = Seq()
  
  // You probably don't want to extend this, extend operate* above instead.
  def transforms(writer: Writer): Seq[Transform] =
    Seq(
      new Chisel3ToHighFirrtl(),
      new IRToWorkingIR(),
      new ResolveAndCheck()
    ) ++ convertToTransforms(operateHigh()) ++
    Seq(
      new HighFirrtlToMiddleFirrtl()
    ) ++ convertToTransforms(operateMiddle()) ++
    Seq(
      new MiddleFirrtlToLowFirrtl()
    ) ++ convertToTransforms(operateLow()) ++
    Seq(
      new EmitVerilogFromLowFirrtl(writer)
    )
}
