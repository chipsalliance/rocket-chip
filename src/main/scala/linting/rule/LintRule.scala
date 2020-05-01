// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting
package rule

import firrtl._
import firrtl.ir._
import firrtl.traversals.Foreachers._
import firrtl.options.{RegisteredLibrary, ShellOption, Dependency, PreservesAll}
import firrtl.stage.RunFirrtlTransformAnnotation

abstract class LintRule extends Transform with RegisteredLibrary with DependencyAPIMigration with PreservesAll[Transform] {
  val lintName: String

  val recommendedFix: String

  lazy val disableCLI: String = s"Omit $lintName from --lint option"

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
      longOption = s"lint-whitelist:$lintName",
      toAnnotationSeq = {
        case whitelist: String => Seq(
          RunFirrtlTransformAnnotation(this),
          Whitelist(lintName, whitelist.split(',').toSet)
        )
      },
      helpText = "Enable linting anonymous registers for all files except provided files.",
      helpValueName = Some("<filename1>.scala[,<filename2>.scala]*")
    )
  )

  override def optionalPrerequisiteOf: Seq[Dependency[Transform]] = Seq(Dependency[LintReporter])

  def collectWhitelist(annotations: AnnotationSeq): Set[String] = annotations.flatMap {
    case Whitelist(name, whitelist) if name == lintName => whitelist.toSeq
    case other => Nil
  }.toSet

  override def execute(state: CircuitState): CircuitState = {
    val errors = new Errors()
    val whitelist = collectWhitelist(state.annotations)
    state.circuit.foreach(lintModule(errors))
    val errorList = errors.collect {
      case ((info, message), mods) if !isWhitelisted(info, whitelist) => Violation(this, info, message, mods)
    }.toSeq.sortBy { _.toString }
    state.copy(annotations = errorList ++ state.annotations )
  }

  protected def lintModule(errors: Errors)(m: DefModule): Unit = {
    m.foreach(lintStatement(errors, m.name))
  }

  protected def lintStatement(errors: Errors, mname: String)(s: Statement): Unit = {
    s.foreach(lintStatement(errors, mname))
  }
}
