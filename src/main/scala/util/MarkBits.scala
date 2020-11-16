// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import firrtl.annotations.{Annotation, NoTargetAnnotation, ReferenceTarget, SingleTargetAnnotation}
import firrtl.{AnnotationSeq, CircuitState, DependencyAPIMigration, RenameMap, Transform}
import chisel3.experimental.{ChiselAnnotation, annotate}
import firrtl.annotations.TargetToken.{Instance, Ref}
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.options.CustomFileEmission

/** Marks a [[Bits]] with information in [[MarkBitsAnnotation]]. Optionally, a
  * file name can be passed in.
  *
  * The Verilog paths for each marked [[Bits]] can be emitted in a metadata file,
  * along with other information specified in the [[MarkBitsAnnotation]].
  */
object MarkBits {
  def mark (bits: Bits, markBitsAnno: MarkBitsAnnotation): Unit = {
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = TargetedMarkBitsAnnotation(bits.toAbsoluteTarget, markBitsAnno)
    })
    // run transform that prints out marked annotation information
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = RunFirrtlTransformAnnotation(new AggregateMarkedBitsTransform)
    })
  }
}

/** Annotations extending [[MarkBitsAnnotation]] will be emitted to the given
  * fileName, or else to a default file.
  */
trait MarkBitsAnnotation extends NoTargetAnnotation {
  val fileName: String = ""

  /** Create a string to write to the output file for the marked [[Bits]]
    *
    * @param path absolute path to the marked [[Bits]]
    */
  def markOutput(path: String): String = path
}

/** This is the FIRRTL equivalent to [[MarkBitsAnnotation]], and passes around
  * a [[ReferenceTarget]] instead of [[Bits]]
  */
case class TargetedMarkBitsAnnotation(target: ReferenceTarget, markBitsAnno: MarkBitsAnnotation) extends SingleTargetAnnotation[ReferenceTarget] {
  /** Create another instance of this Annotation */
  override def duplicate(n: ReferenceTarget): Annotation = this.copy(n, markBitsAnno)

  val fileName: String = markBitsAnno.fileName
  def markOutput(path: String): String = markBitsAnno.markOutput(path)
}

/** An annotation containing all [[TargetedMarkBitsAnnotation]] that share the
  * same output file.
  */
case class PerFileMarkBitsAnnotation(markBitsAnnos: Seq[TargetedMarkBitsAnnotation]) extends Annotation with CustomFileEmission {
  override def update(renames: RenameMap): Seq[Annotation] =
    Seq(PerFileMarkBitsAnnotation(markBitsAnnos.flatMap(_.update(renames)).asInstanceOf[Seq[TargetedMarkBitsAnnotation]]))

  override def baseFileName(annotations: AnnotationSeq): String = {
    val fileName = markBitsAnnos.head.fileName

    // check filenames of all annotations match and are not empty
    if (markBitsAnnos.forall(_.fileName != fileName))
      firrtl.Utils.throwInternalError(s"All file names in this Annotation are expected to match $fileName.")
    if (fileName.isEmpty)
      firrtl.Utils.throwInternalError("Encountered unexpected empty file name.")

    fileName
  }

  def suffix: Option[String] = None

  def getBytes: Iterable[Byte] = {
    val lines: Seq[String] = markBitsAnnos.map(a => a.markOutput(toVerilogPath(a.target)))
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

  override def execute(state: CircuitState): CircuitState = {
    // gather [[MarkBitsAnnotation]]s
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

    // return remaining annotations
    val annos = gatheredMarkBitsAnnos.getOrElse("", Nil)
    state.copy(annotations = annos ++ perFileAnnos)
  }
}
