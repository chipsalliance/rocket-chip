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
  // We can add many of these, maybe one for crosses, or different types of bins
  // Maybe each one creates the same CoverPointAnnotation, or maybe we create multiple
  //   annotation types. That's something you'll need to play with
  def cover(signal: Bits, message: String): Unit = {
    // Adds CoverPointAnnotation to the currently elaborated module's list of annotations
    annotate(new ChiselAnnotation {
      /** Conversion to FIRRTL Annotation */
      override def toFirrtl: Annotation = CoverPointAnnotation(signal.toTarget, message)
    })
    // Ensures our custom transorm EmitCoverageTransform is run by FIRRTL
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = RunFirrtlTransformAnnotation(new EmitCoverageTransform)
    })
    // Make sure the signal we are covering isn't removed by the FIRRTL compiler
    dontTouch(signal)
  }
}

/** FIRRTL Annotation which carries coverage information throughout compilation process
  * In here we should put the bins, crosses, all information for coverage generation
  */
case class CoverPointAnnotation(target: ReferenceTarget, message: String) extends SingleTargetAnnotation[ReferenceTarget] {
  override def duplicate(n: ReferenceTarget): Annotation = this.copy(target = n)
  override def serialize: String = {
    s"Cover($message, ${target.prettyPrint("")})"
  }
}

/** FIRRTL Transformation which simply writes to a file and deletes the annotations
  * Here is where we would have system verilog templates, and then we'd emit them
  */
class EmitCoverageTransform extends Transform {

  // These forms ensure this transform is executed late in FIRRTL's compilation flow
  // Renaming could still occur after this point - we need to wait for a FIRRTL
  //   bump to enable us to express a dependency on the Emitter, which would
  //   ensure this occurs with the emitter
  override val inputForm = LowForm
  override val outputForm = LowForm

  // The function executed by the FIRRTL compiler
  // Here is where we do whatever we need to do
  override def execute(state: CircuitState): CircuitState = {
    // Find the target directory, if it exists
    val directory = state.annotations.collectFirst {
      case TargetDirAnnotation(dir) => dir
    }.getOrElse(".")

    // Partition all annotations into either ours, or other remaining ones
    val (coverages, remainingAnnotations) = state.annotations.partition {
      case _: CoverPointAnnotation => true
      case _ => false
    }

    // Write all annotations to a file
    // We could definitely customize the name of this file
    val x = Paths.get(directory + "/coverages.sv")
    if(coverages.nonEmpty) {
      println(s"Running EmitCoverageTransform to $directory! $coverages")
      Files.write(x, coverages.map(_.serialize).mkString("\n").getBytes())
    }

    // Finally, return everything, but replace the input annotations with remaining annotations
    state.copy(annotations = remainingAnnotations)
  }
}




