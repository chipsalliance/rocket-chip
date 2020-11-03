// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import firrtl.annotations.{Annotation, MultiTargetAnnotation, ReferenceTarget, Target}
import firrtl.{AnnotationSeq, CircuitState, DependencyAPIMigration, Transform}
import chisel3.experimental.{ChiselAnnotation, annotate}
import firrtl.annotations.TargetToken.{Instance, Ref}
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.options.CustomFileEmission

/** Marks a signal with a [[CutpointAnnotation]]. Optionally, a label can be
  * passed in.
  *
  * The Verilog paths to each cutpoint will be emitted in a metadata file,
  * called cutpoints.<label>.txt.
  */
object Cutpoint {
  def cutpoint (signal: Bits, label: String = ""): Unit = {
    // create annotation for a cutpoint
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = CutpointAnnotation(Seq(Seq(signal.toAbsoluteTarget)), label)
    })
  }
  // run transform that prints out cutpoint annotation information
  annotate(new ChiselAnnotation {
    override def toFirrtl: Annotation = RunFirrtlTransformAnnotation(new AggregateCutpointsTransform)
  })
}

/** Annotation containing target information for a cutpointed wire and label
  * information for constructing a file name. Cutpoint paths will be written
  * to the file with the constructed name.
  */
case class CutpointAnnotation(targets: Seq[Seq[ReferenceTarget]], label: String = "") extends MultiTargetAnnotation
    with CustomFileEmission {
  def duplicate(n: Seq[Seq[Target]]): Annotation = this.copy(targets = n.map {
    case t: Seq[ReferenceTarget] => t
    case t => firrtl.Utils.throwInternalError(s"Error in CutpointAnnotation: seeing ${t.getClass} where ReferenceTarget is expected")
  }, label = label)

  // construct file name
  def baseFileName(annotations: AnnotationSeq) = if (label.isEmpty) "cutpoints" else s"cutpoints.$label"
  def suffix: Option[String] = Some(".txt")
  // create bytes written to file
  def getBytes: Iterable[Byte] = {
    val cutpointVerilogPaths: Seq[String] = targets.flatten.map( t => toVerilogPath(t))
    cutpointVerilogPaths.mkString("\n").getBytes()
  }

  /** Construct a verilog path from [[ReferenceTarget]] information. */
  def toVerilogPath(target: ReferenceTarget): String = {
    val moduleString = target.moduleOpt.getOrElse("")
    val tokenString = target.tokens.map {
      case Ref(r)      => s"$r"
      case Instance(i) => s"$i."
      case _ => ""
    }.mkString("")
    s"$moduleString.$tokenString"
  }
}

/** Transform that aggregates [[CutpointAnnotation]]s based on their labels. */
class AggregateCutpointsTransform extends Transform with DependencyAPIMigration {
  override def optionalPrerequisites = firrtl.stage.Forms.BackendEmitters

  override def execute(state: CircuitState): CircuitState = {
    // flatten annotations into tuples of (label, cutpoint)
    val cutpointTargets: Seq[(String, ReferenceTarget)] = state.annotations.flatMap {
      case a: CutpointAnnotation =>
        Some(a.targets.flatten.map( t => (a.label, t) ))
      case _ => None
    }.flatten

    // consolidate cutpoints with the same label
    val annoMap: Map[String, Seq[ReferenceTarget]] = cutpointTargets.groupBy(_._1).map {
      case (label, cutpoint) => (label, cutpoint.map(_._2))
    }

    // create one [[CutpointAnnotation]] per label
    val cutpointAnnos = annoMap.map {
      case (label, cutpoints) => CutpointAnnotation(Seq(cutpoints), label)
    }

    // return remaining annotations
    val annos = state.annotations.flatMap {
      case _: CutpointAnnotation => None
      case a => Some(a)
    }
    state.copy(annotations = annos ++ cutpointAnnos)
  }
}
