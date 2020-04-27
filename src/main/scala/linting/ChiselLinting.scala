package freechips.rocketchip.linting

import firrtl._
import firrtl.ir._
import firrtl.traversals.Foreachers._
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{OptionsException, RegisteredLibrary, ShellOption}
import firrtl.stage.RunFirrtlTransformAnnotation
import chisel3.experimental.ChiselAnnotation


import scala.collection.mutable

case class LintError(info: FileInfo, modules: Set[String])
case class LintExceptions(seq: Seq[LintError]) extends FirrtlUserException(
  s"""Linting Report: ${seq.size} exceptions!
     |Please fix using @chiselName, *.suggestName(...). Otherwise, whitelist the file via the Chisel commandline option:
     |    --linting-whitelist <filename>.scala
     |or via annotations in your Chisel code:
     |    annotate(LintWhitelist("<filename>.scala")).
     |
     |Only showing first few exceptions:
     |${seq.zip(0 until 20)
           .map { case (lint: LintError, idx: Int) => s"$idx. ${lint.info} in ${lint.modules}"}
           .mkString("\n")}
     """.stripMargin
)

case class LintAnonymousRegsWhitelist(whiteList: Set[String]) extends NoTargetAnnotation with ChiselAnnotation {
  override def toFirrtl = this
}

final class LintAnonymousRegisters extends Transform with RegisteredLibrary {

  val options = Seq(
    new ShellOption[Unit](
      longOption = "lint-anon-regs",
      toAnnotationSeq = { _ => Seq(RunFirrtlTransform(this)) },
      helpText = "Enable linting anonymous registers for all files.",
    ),
    new ShellOption[Seq[String]](
      longOption = "lint-anon-regsW",
      toAnnotationSeq = {
        case whitelist: Seq[String] => Seq(
          RunFirrtlTransform(this),
          LintWhitelist(whitelist.toSet)
        )
      },
      helpText = "Enable linting anonymous registers for all files except provided files.",
      helpValueName = Some("<filename1>.scala[,<filename2>.scala]*")
    )
  )

  override def inputForm = HighForm

  override def outputForm = HighForm

  type Errors = mutable.HashMap[FileInfo, LintError]

  override def execute(state: CircuitState): CircuitState = {
    val errors = new Errors()
    val whitelist = state.annotations.foldLeft(Set.empty[String]) {
      (WL, anno) => anno match {
        case LintWhitelist(wl) => WL ++ wl
        case other => WL
      }
    }
    state.circuit.foreach(lintModule(errors))
    val es = errors.values.toSeq.sortBy { _.toString }.filter { lint => !isWhitelisted(lint.info, whitelist) }
    if(es.nonEmpty) throw LintExceptions(es) else state
  }

  /** Determines whether name is prepended with an underscore, indicating a bad name */
  def isTemporary(name: String): Boolean = name.nonEmpty && name.head == '_'

  /** Determines whether name is prepended with an underscore, indicating a bad name */
  def isTemporary(expr: Expression): Boolean = isTemporary(getName(expr))

  /** Returns the root reference name of an Expression
    * @throws Exception
    */
  def getName(expr: Expression): String = expr match {
    case r: WRef => r.name
    case f: WSubField => getName(f.expr)
    case i: WSubIndex => getName(i.expr)
    case a: WSubAccess => getName(a.expr)
    case other => throw new Exception(s"Unexpected match! $other")
  }

  /** Splits an info into non-nested Infos
    * Right now the FIRRTL parser concatenates the infos if using both .fir and .scala source locators, instead of using
    *   MultiInfo. This code will work with both the current concatenation and a future FIRRTL change to migrate to MultiInfo.
    */
  def flatten(info: Info): Seq[Info] = info match {
    case MultiInfo(seq) => seq.flatMap(flatten)
    case f: FileInfo =>
      val infoRegex = "\\s*(.*\\.scala \\d+:\\d+):.*\\.fir@\\d+\\.\\d+\\s*".r
      f.info.serialize match {
        case infoRegex(scala) => Seq(FileInfo(StringLit(scala)))
        case other => Seq(f)
      }
    case other => println(other); Seq(other)
  }

  /** Returns the first .scala source location contained inside info */
  def getScalaInfo(info: Info): Option[FileInfo] = flatten(info).collectFirst {
    case i: FileInfo if i.serialize.contains("scala") => i
  }

  /** Returns whether the given file is contained in the whiteList */
  def isWhitelisted(info: FileInfo, whiteList: Set[String]): Boolean = {
    val file = info.info.serialize.split(' ').head
    whiteList.contains(file)
  }

  private def lintModule(errors: Errors)(m: DefModule): Unit = {
    m.foreach(lintStatement(errors, m.name))
  }

  private def lintStatement(errors: Errors, mname: String)(s: Statement): Unit = {
    def updateErrors(info: Info, name: String): Unit = getScalaInfo(info) match {
      case None =>
      case Some(scalaInfo) =>
        val lint = errors.getOrElse(scalaInfo, LintError(scalaInfo, Set.empty))
        errors(scalaInfo) = lint.copy(modules = lint.modules + mname)
    }

    s foreach lintStatement(errors, mname)
    s match {
      case r: DefRegister  if isTemporary(r.name) => updateErrors(r.info, r.name)
      case other =>
    }
  }
}
