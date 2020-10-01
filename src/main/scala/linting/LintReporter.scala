// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting

import firrtl._
import firrtl.options.{RegisteredLibrary, ShellOption, PreservesAll, Dependency}
import firrtl.stage.RunFirrtlTransformAnnotation

/** The final transform for all linting
  * Collects all computer lint violations and displays them
  * Optionally kills the compilation, or proceeds with a warning
  */
final class LintReporter extends Transform with RegisteredLibrary with DependencyAPIMigration with PreservesAll[Transform] {
  val displayTotal = "displayTotal=(\\d+)".r
  val perTotal = "display:([_a-zA-Z0-9\\-]+)=(\\d+)".r
  val perAllTotal = "display:\\*=(\\d+)".r

  lazy val options = Seq(
    new ShellOption[String](
      longOption = s"lint",
      toAnnotationSeq = {
        case "*" => RunFirrtlTransformAnnotation(this) +: (Linter.lintMap.values.map(RunFirrtlTransformAnnotation(_)).toSeq)
        case other => RunFirrtlTransformAnnotation(this) +: (other.split(',').toSeq.map { s =>
          Linter.lintMap.get(s) match {
            case Some(l) => RunFirrtlTransformAnnotation(l)
            case None => sys.error(s"Unknown linter argument: $s")
          }
        })
      },
      helpText = s"Enable linting for specified rules, where * is all rules. Available rules: ${Linter.linters.map(l => l.lintName).mkString(",")}.",
      helpValueName = Some("[*]|[<lintRule>,<lintRule>,...]")
    ),
    new ShellOption[String](
      longOption = "lint-options",
      toAnnotationSeq = { arg: String =>
        val displayOptions = arg.split(',').toSeq.foldLeft(DisplayOptions()) { (opt, str) =>
          str match {
            case "strict" => opt.copy(level = "strict")
            case "warn" => opt.copy(level = "warn")
            case displayTotal(n) => opt.copy(totalLimit = Some(n.toInt))
            case perTotal(lint, n) => opt.copy(perErrorLimit = opt.perErrorLimit + (Linter.lintMap(lint).lintName -> n.toInt))
            case perAllTotal(n) => opt.copy(perErrorLimit = Linter.linters.map(l => l.lintName -> n.toInt).toMap)
            case other => throw sys.error(s"Unrecognized option passed to --lint: $other")
          }
        }
        Seq(displayOptions)
      },
      helpText = "Customize linting options, including strict/warn or number of violations displayed.",
      helpValueName = Some("(strict|warn)[,displayTotal=<numError>][,display:<lintName>=<numError>]")
    )
  )

  // Run before ExpandWhens
  override def optionalPrerequisiteOf = Seq(Dependency[firrtl.passes.ExpandWhensAndCheck])

  override def execute(state: CircuitState): CircuitState = {
    val grouped = state.annotations.groupBy {
      case e: Violation => "v"
      case o: DisplayOptions => "o"
      case w: Whitelist => "w"
      case other => "a"
    }

    val violations = grouped.getOrElse("v", Nil).asInstanceOf[Seq[Violation]]
    val options = grouped.getOrElse("o", Nil).headOption.getOrElse(DisplayOptions()).asInstanceOf[DisplayOptions]
    val remainingAnnotations = grouped.getOrElse("a", Nil)

    if(violations.nonEmpty) {
      options.level match {
        case "strict" => throw LintException(violations.toSeq, options)
        case "warn" => println(LintException.buildMessage(violations.toSeq, options))
      }
    }

    state.copy(annotations = remainingAnnotations)
  }
}
