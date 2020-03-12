// See LICENSE.SiFive for license details.

package freechips.rocketchip.util.coverage

import java.nio.file.{Files, Paths}

import chisel3._
import chisel3.experimental.{ChiselAnnotation, RunFirrtlTransform, annotate}
import firrtl.{CircuitState, LowForm, Transform}
import firrtl.annotations.{Annotation, ReferenceTarget, SingleTargetAnnotation}
import firrtl.options.TargetDirAnnotation
import firrtl.stage.RunFirrtlTransformAnnotation

object Coverage {
  // Chisel/Aspect user API
  def cover(signal: Bits, message: String): Unit = {
    annotate(new ChiselAnnotation {
      /** Conversion to FIRRTL Annotation */
      override def toFirrtl: Annotation = CoverPointAnnotation(signal.toTarget, message)
    })
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = RunFirrtlTransformAnnotation(new EmitCoverageTransform)
    })
    dontTouch(signal)
  }
}

// FIRRTL Annotation which carries coverage information throughout compilation process
case class CoverPointAnnotation(target: ReferenceTarget, message: String) extends SingleTargetAnnotation[ReferenceTarget] {
  override def duplicate(n: ReferenceTarget): Annotation = this.copy(target = n)
  def serialize: String = {
    s"Cover($message, ${target.prettyPrint})"
  }
}

class EmitCoverageTransform extends Transform {
  override val inputForm = LowForm
  override val outputForm = LowForm

  override def execute(state: CircuitState): CircuitState = {
    val directory = state.annotations.collectFirst {
      case TargetDirAnnotation(dir) => dir
    }.getOrElse(".")

    val (coverages, remainingAnnotations) = state.annotations.partition {
      case _: CoverPointAnnotation => true
      case _ => false
    }
    val x = Paths.get(directory + "/coverages.sv")
    if(coverages.nonEmpty) {
      println(s"Running EmitCoverageTransform to $directory! $coverages")
      Files.write(x, coverages.map(_.serialize).mkString("\n").getBytes())
    }

    state.copy(annotations = remainingAnnotations)
  }
}




