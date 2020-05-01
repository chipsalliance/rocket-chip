// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting
package rule

import firrtl._
import firrtl.ir._
import firrtl.traversals.Foreachers._
import firrtl.options.{RegisteredLibrary, ShellOption, Dependency, PreservesAll}
import firrtl.stage.RunFirrtlTransformAnnotation

/** Template class for lint rules
  * @note After extending this class, be sure to update the linter list in [[Linter]]
  */
abstract class LintRule extends Transform with RegisteredLibrary with DependencyAPIMigration with PreservesAll[Transform] {

  // Name of this rule. Cannot contain spaces!
  val lintName: String

  // Recommended fix to the user
  val recommendedFix: String

  lazy val disableCLI: String = s"Omit $lintName from --lint option"

  /** A string representation of using the Chisel/Scala API to whitelist files
    *
    * @param files a list of scala files to whitelist
    */
  def scalaAPI(files: Seq[String]): String = {
    val setArg = files.map(f => s""""$f"""").mkString(",")
    s"""whitelist("$lintName", Set($setArg))"""
  }

  /** A string representation of using the commandline API to whitelist files
    *
    * @param files a list of scala files to whitelist
    */
  def whitelistAPI(files: Seq[String]): String = {
    val arg = files.mkString(",")
    s"""--${options.head.longOption} $arg"""
  }

  /** A utiltiy functions to find whitelisted files in the given annotations
    *
    * @param annotations Input annotations to find all whitelisted files for this lint rule
    */
  def collectWhitelist(annotations: AnnotationSeq): Set[String] = annotations.flatMap {
    case Whitelist(name, whitelist) if name == lintName => whitelist.toSeq
    case other => Nil
  }.toSet


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

  // Run lint rules after deduplication
  override def optionalPrerequisites: Seq[Dependency[Transform]] = Seq(Dependency[firrtl.transforms.DedupModules])

  // Run lint rules before the Lint Reporter
  override def optionalPrerequisiteOf: Seq[Dependency[Transform]] = Seq(Dependency[LintReporter])

  override def execute(state: CircuitState): CircuitState = {
    val violations = new Violations()
    val whitelist = collectWhitelist(state.annotations)
    state.circuit.foreach(lintModule(violations))
    val errorList = violations.collect {
      case ((info, message), mods) if !isWhitelisted(info, whitelist) => Violation(this, info, message, mods)
    }.toSeq.sortBy { _.toString }
    state.copy(annotations = errorList ++ state.annotations )
  }

  // Can be overridden by subclass implementations
  protected def lintModule(violations: Violations)(m: DefModule): Unit = {
    m.foreach(lintStatement(violations, m.name))
  }

  // Can be overridden by subclass implementations
  protected def lintStatement(violations: Violations, mname: String)(s: Statement): Unit = {
    s.foreach(lintStatement(violations, mname))
  }
}
