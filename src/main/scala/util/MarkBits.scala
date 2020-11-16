// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import firrtl.annotations.{Annotation, NoTargetAnnotation, ReferenceTarget, SingleTargetAnnotation}
import firrtl.{AnnotationSeq, CircuitState, DependencyAPIMigration, RenameMap, Transform}
import chisel3.experimental.{ChiselAnnotation, annotate}
import firrtl.annotations.TargetToken.{Instance, Ref}
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.options.CustomFileEmission

object MarkBits {
  def mark (bits: Bits, markBitsAnno: MarkBitsAnnotation): Unit = {
    // create annotation for a Bits
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = TargetedMarkBitsAnnotation(bits.toAbsoluteTarget, markBitsAnno)
    })
    // run transform that prints out marked annotation information
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = RunFirrtlTransformAnnotation(new AggregateMarkedBitsTransform)
    })
  }
}

trait MarkBitsAnnotation extends NoTargetAnnotation {
  val fileName: String = ""

  /** Create string to write to output file for this target */
  def getBytes(path: String): String = path
}

case class TargetedMarkBitsAnnotation(target: ReferenceTarget, markBitsAnno: MarkBitsAnnotation) extends SingleTargetAnnotation[ReferenceTarget] {
  /** Create another instance of this Annotation */
  override def duplicate(n: ReferenceTarget): Annotation = this.copy(n, markBitsAnno)

  val fileName: String = markBitsAnno.fileName
  def getBytes(path: String): String = markBitsAnno.getBytes(path)
}

case class PerFileMarkBitsAnnotation(markBitsAnnos: Seq[TargetedMarkBitsAnnotation]) extends Annotation with CustomFileEmission {
  override def update(renames: RenameMap): Seq[Annotation] =
    Seq(PerFileMarkBitsAnnotation(markBitsAnnos.flatMap(_.update(renames)).asInstanceOf[Seq[TargetedMarkBitsAnnotation]]))

  // all annotations in the markBitsAnnos Seq should have the same (nonempty) file name TODO maybe add a check?
  override def baseFileName(annotations: AnnotationSeq): String =
    if (markBitsAnnos.head.fileName.isEmpty) "markedBits" else markBitsAnnos.head.fileName

  def suffix: Option[String] = if (markBitsAnnos.head.fileName.isEmpty) Some(".txt") else None

  def getBytes: Iterable[Byte] = {
    val lines: Seq[String] = markBitsAnnos.map(a => a.getBytes(toVerilogPath(a.target)))
    lines.mkString("\n").getBytes
  }

  /** Construct a verilog path from [[ReferenceTarget]] information */
  final def toVerilogPath(target: ReferenceTarget): String = {
    val moduleString = target.moduleOpt.getOrElse("")
    val tokenString = target.tokens.map {
      case Ref(r)      => s"$r"
      case Instance(i) => s"$i."
      case _ => ""
    }.mkString("")
    s"$moduleString.$tokenString"
  }
}

/** Transform that aggregates [[TargetedMarkBitsAnnotation]]s based on their labels. */
class AggregateMarkedBitsTransform extends Transform with DependencyAPIMigration {
  override def prerequisites = firrtl.stage.Forms.BackendEmitters

//  override def optionalPrerequisiteOf: Seq[TransformDependency] = Seq[firrtl.options.phases.WriteOutputAnnotations]

  override def execute(state: CircuitState): CircuitState = {
    // gather [[MarkBitsAnnotations]]s
    val gatheredMarkBitsAnnos: Map[String, Seq[Annotation]] = state.annotations.groupBy {
      case a: TargetedMarkBitsAnnotation => if(a.fileName.isEmpty) "markedBits" else a.fileName
      case _ => ""
    }

    // create one [[PerFileMarkBitsAnnotation]] per label
    val perFileAnnos = gatheredMarkBitsAnnos.collect {
      case (label, seq) if label.nonEmpty => {
        println(s"label $label seq $seq")
        PerFileMarkBitsAnnotation(seq.asInstanceOf[Seq[TargetedMarkBitsAnnotation]])
      }
    }

    println(s"PER FILE ANNOS: $perFileAnnos")

    // return remaining annotations
    val annos = gatheredMarkBitsAnnos.getOrElse("", Nil)
//    println(s"ALL ANNOS: ${annos ++ perFileAnnos}")
    state.copy(annotations = annos ++ perFileAnnos)
  }
}
