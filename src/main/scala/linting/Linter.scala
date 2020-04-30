// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting

import firrtl._
import firrtl.ir._
import firrtl.traversals.Foreachers._
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{OptionsException, RegisteredLibrary, ShellOption, Dependency, PreservesAll}
import firrtl.stage.RunFirrtlTransformAnnotation
import chisel3.experimental.{ChiselAnnotation, annotate}
import scala.collection.mutable


object Linter {

  def whitelist(lintNumber: Int, filenames: String*) = {
    lintMap.get(lintNumber.toString) match {
      case Some(_) => annotate(Whitelist(lintNumber, filenames.toSet))
      case None => 
    }
  }
  def whitelist(lintName: String, filenames: String*) = {
    lintMap.get(lintName) match {
      case Some(l) => annotate(Whitelist(l.lintNumber, filenames.toSet))
      case None => 
    }
  }

  lazy val linters = Seq(
    new LintAnonymousRegisters,
    new LintTruncatingWidths
  )

  private [linting] lazy val lintMap = linters.flatMap {
    l => Seq(l.lintNumber.toString -> l, l.lintName -> l)
  }.toMap

  /** Determines whether name is prepended with an underscore, indicating a bad name */
  private [linting] def isTemporary(name: String): Boolean = name.nonEmpty && name.head == '_'

  /** Determines whether name is prepended with an underscore, indicating a bad name */
  private [linting] def isTemporary(expr: Expression): Boolean = isTemporary(getName(expr))

  /** Returns the root reference name of an Expression
    * @throws Exception
    */
  private [linting] def getName(expr: Expression): String = expr match {
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
  private [linting] def flatten(info: Info): Seq[FileInfo] = info match {
    case MultiInfo(seq) => seq.flatMap(flatten)
    case f: FileInfo =>
      val infoRegex = "\\s*(.*\\.scala \\d+:\\d+):(.*\\.fir@\\d+\\.\\d+)\\s*".r
      f.info.serialize match {
        case infoRegex(scala, fir) => Seq(FileInfo(StringLit(scala)), FileInfo(StringLit(fir)))
        case other => Seq(f)
      }
    case other => Nil
  }

  /** Returns the first .scala source location contained inside info */
  private [linting] def getScalaInfo(info: Info): Option[FileInfo] = flatten(info).collectFirst {
    case i: FileInfo if i.serialize.contains("scala") => i
  }

  /** Returns whether the given file is contained in the whiteList */
  private [linting] def isWhitelisted(info: Info, whiteList: Set[String]): Boolean = {
    flatten(info).exists { i =>
      val file = i.info.serialize.split(' ').head
      whiteList.contains(file)
    } 
  }

  private [linting] def updateErrors(info: Info, message: String, errors: Errors, mname: String): Unit = {
      val mods = errors.getOrElse((info, message), Set.empty)
      errors((info, message)) = mods + mname
  }

  private [linting] type Errors = mutable.HashMap[(Info, String), Set[String]]
}

abstract class Linter extends Transform with RegisteredLibrary with DependencyAPIMigration with PreservesAll[Transform] {
  // Import useful utility functions
  import Linter._

  val lintNumber: Int
  val lintName: String
  val recommendedFix: String
  lazy val disableCLI: String = s"Omit $lintName or $lintNumber from --lint option"

  def scalaAPI(files: Seq[String]): String = {
    val setArg = files.map(f => s""""$f"""").mkString(",")
    s"""whitelist("$lintName", Set($setArg))"""
  }
  def whitelistAPI(files: Seq[String]): String = {
    val arg = files.mkString(",")
    s"""--${options.head.longOption} $arg"""
  }

  lazy val options = Seq(
    new ShellOption[String](
      longOption = s"lint-whitelist#$lintNumber",
      toAnnotationSeq = {
        case whitelist: String => Seq(
          RunFirrtlTransformAnnotation(this),
          Whitelist(lintNumber, whitelist.split(',').toSet)
        )
      },
      helpText = "Enable linting anonymous registers for all files except provided files.",
      helpValueName = Some("<filename1>.scala[,<filename2>.scala]*")
    )
  )

  override def optionalPrerequisiteOf: Seq[Dependency[Transform]] = Seq(Dependency[LintReporter])


  def collectWhitelist(annotations: AnnotationSeq): Set[String] = annotations.flatMap {
    case Whitelist(num, whitelist) if num == lintNumber => whitelist.toSeq
    case other => Nil
  }.toSet

  override def execute(state: CircuitState): CircuitState = {
    val errors = new Errors()
    val whitelist = collectWhitelist(state.annotations)
    state.circuit.foreach(lintModule(errors))
    val errorList = errors.collect {
      case ((info, message), mods) if !isWhitelisted(info, whitelist) => LintError(this, info, message, mods)
    }.toSeq.sortBy { _.toString }
    state.copy(annotations = errorList ++ state.annotations )
  }

  protected def lintModule(errors: Errors)(m: DefModule): Unit = {
    m.foreach(lintStatement(errors, m.name))
  }

  protected def lintStatement(errors: Errors, mname: String)(s: Statement): Unit = {
    s foreach lintStatement(errors, mname)
  }
}
