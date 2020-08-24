// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import firrtl.annotations.{Annotation, ReferenceTarget, SingleTargetAnnotation}
import firrtl.{CircuitState, LowForm, Transform}
import chisel3.experimental.{ChiselAnnotation, annotate}
import firrtl.stage.RunFirrtlTransformAnnotation

/** Marks a signal with an annotation so it can be parsed by post-processors as a cutpoint */
object Cutpoint {
  def cutpoint (signal: Bits): Unit = {
    // create annotation for a cutpoint
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = CutpointAnnotation(signal.toTarget)
    })
    // run transform that prints out cutpoint annotation information
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = RunFirrtlTransformAnnotation(new PrintCutpointsTransform)
    })
  }
}

/** Annotation containing information about a cutpointed wire */
case class CutpointAnnotation(target: ReferenceTarget) extends SingleTargetAnnotation[ReferenceTarget] {
  override def duplicate(n: ReferenceTarget): Annotation = this.copy(target = n)
  override def serialize: String = {
    s"cutpoint(\n${target.prettyPrint("  ")})"
  }
}

/** Transform that prints out cutpoint information */
class PrintCutpointsTransform extends Transform {
  override val inputForm = LowForm
  override val outputForm = LowForm

  override def execute(state: CircuitState): CircuitState = {
    val annos = state.annotations.flatMap {
      case a: CutpointAnnotation =>
        println(a.serialize) // TODO dump to a file + format properly with paths
        None
      case a => Some(a)
    }

    state.copy(annotations = annos)
  }
}
