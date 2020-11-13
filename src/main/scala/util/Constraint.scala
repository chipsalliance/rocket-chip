// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import firrtl.annotations.{Annotation, MultiTargetAnnotation, ReferenceTarget, SingleTargetAnnotation, Target}
import firrtl.{AnnotationSeq, CircuitState, DependencyAPIMigration, RenameMap, Transform}
import chisel3.experimental.{ChiselAnnotation, annotate}
import firrtl.annotations.TargetToken.{Instance, Ref}
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.options.CustomFileEmission
import firrtl.stage.TransformManager.TransformDependency

/** Marks a signal with a [[CutpointAnnotation]]. Optionally, a label can be
  * passed in.
  *
  * The Verilog paths to each cutpoint will be emitted in a metadata file,
  * called cutpoints.<label>.txt.
  */
object Constraint {
  def constraint (signal: Bits, label: String = "", property: String = ""): Unit = {
    // create annotation for a cutpoint
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = ConstraintAnnotation(signal.toAbsoluteTarget, label, property)
    })
    // run transform that prints out cutpoint annotation information
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = RunFirrtlTransformAnnotation(new AggregateConstraintsTransform)
    })
  }
}


case class ConstraintAnnotation(target: ReferenceTarget, label: String = "", property: String = "") extends SingleTargetAnnotation[ReferenceTarget] {
  override def duplicate(n: ReferenceTarget): Annotation = this.copy(n, label, property)
}

case class PerFileConstraintAnnotation(constraintAnnos: Seq[ConstraintAnnotation]) extends Annotation with CustomFileEmission {
  override def update(renames: RenameMap): Seq[Annotation] =
    Seq(PerFileConstraintAnnotation(constraintAnnos.flatMap(_.update(renames)).asInstanceOf[Seq[ConstraintAnnotation]]))

  // assuming all annos in constraintAnnos have the same filename
  override def baseFileName(annotations: AnnotationSeq): String = {
    println(s"label ${constraintAnnos.head.label}\n\n")
    if (constraintAnnos.head.label.isEmpty) "output" else s"output.${constraintAnnos.head.label}"
  }

  def suffix: Option[String] = Some(".tcl")

  def getBytes: Iterable[Byte] = {
    println("GET BYTES CALLED")
    val constraintStrings: Seq[String] = constraintAnnos.map( a => s"-cutpoint ${toVerilogPath(a.target)} ${a.property}")
    constraintStrings.mkString("\n").getBytes()
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
class AggregateConstraintsTransform extends Transform with DependencyAPIMigration {
  override def prerequisites = firrtl.stage.Forms.BackendEmitters

//  override def optionalPrerequisiteOf: Seq[TransformDependency] = Seq[firrtl.options.phases.WriteOutputAnnotations]

  override def execute(state: CircuitState): CircuitState = {
    // gather [[ConstraintAnnotation]]s
    val gatheredConstraints: Map[String, Seq[Annotation]] = state.annotations.groupBy {
      case a: ConstraintAnnotation => if(a.label.isEmpty) "constraints" else a.label
      case _ => ""
    }

    // create one [[PerFileConstraintAnnotation]] per label
    val perFileConstraintAnnos = gatheredConstraints.collect {
      case (label, seq) if label.nonEmpty => {
        println(s"label $label seq $seq")
        PerFileConstraintAnnotation(seq.asInstanceOf[Seq[ConstraintAnnotation]])
      }
    }

    println(s"PER FILE CONSTRAINTS: $perFileConstraintAnnos")

    // return remaining annotations
    val annos = gatheredConstraints.getOrElse("", Nil)
    println(s"ALL ANNOS: ${annos ++ perFileConstraintAnnos}")
    state.copy(annotations = annos ++ perFileConstraintAnnos)
  }
}
