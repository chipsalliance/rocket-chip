package freechips.rocketchip.linting

import firrtl._
import firrtl.ir._
import firrtl.traversals.Foreachers._
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{OptionsException, RegisteredLibrary, ShellOption, PreservesAll, Dependency}
import firrtl.stage.RunFirrtlTransformAnnotation
import chisel3.experimental.ChiselAnnotation
import scala.collection.mutable

final class LintReporter extends Transform with RegisteredLibrary with DependencyAPIMigration with PreservesAll[Transform] {
  val displayTotal = "displayTotal=(\\d+)".r
  val perTotal = "display#(\\d+)=(\\d+)".r
  val perAllTotal = "display#\\*=(\\d+)".r

  lazy val options = Seq(
    new ShellOption[String](
      longOption = s"lint",
      toAnnotationSeq = {
        case "*" => Linter.lintMap.values.map(RunFirrtlTransformAnnotation(_)).toSeq
        case other => other.split(',').toSeq.map { s =>
          Linter.lintMap.get(s) match {
            case Some(l) => RunFirrtlTransformAnnotation(l)
            case None => sys.error(s"Unknown linter argument: $s")
          }
        }
      },
      helpText = "Enable linting for specified linting rules, where * is all rules.",
      helpValueName = Some("[<lintingNumber>|<lintName>|*][,<lintingNumber>|<lintName>]...")
    ),
    new ShellOption[String](
      longOption = "lint-options",
      toAnnotationSeq = { arg: String =>
        val displayOptions = arg.split(',').toSeq.foldLeft(LintDisplayOptions()) { (opt, str) =>
          str match {
            case "strict" => opt.copy(level = "strict")
            case "warn" => opt.copy(level = "warn")
            case displayTotal(n) => opt.copy(totalLimit = Some(n.toInt))
            case perTotal(lintNumber, n) => opt.copy(perErrorLimit = opt.perErrorLimit + (lintNumber.toInt -> n.toInt))
            case perAllTotal(n) => opt.copy(perErrorLimit = Linter.linters.map(l => l.lintNumber -> n.toInt).toMap)
            case other => throw sys.error(s"Unrecognized option passed to --lint: $other")
          }
        }
        Seq(RunFirrtlTransformAnnotation(this), displayOptions)
      },
      helpText = "Customize linting options, including error/warn or number of errors displayed.",
      helpValueName = Some("(strict|warn)[,displayTotal=<numError>][,display#<lintNumber>=<numError>]")
    )
  )

  override def optionalPrerequisites = Seq(Dependency[firrtl.transforms.DedupModules])

  override def optionalPrerequisiteOf = Seq(Dependency[firrtl.passes.ExpandWhensAndCheck])

  override def execute(state: CircuitState): CircuitState = {
    val grouped = state.annotations.groupBy {
      case e: LintError => "e"
      case o: LintDisplayOptions => "o"
      case other => "a"
    }

    val errors = grouped.getOrElse("e", Nil).asInstanceOf[Seq[LintError]]
    val options = grouped.getOrElse("o", Nil).headOption.getOrElse(LintDisplayOptions()).asInstanceOf[LintDisplayOptions]
    val remainingAnnotations = grouped.getOrElse("a", Nil)

    if(errors.nonEmpty) {
      options.level match {
        case "strict" => throw LintExceptions(errors.toSeq, options)
        case "warn" => println(LintExceptions.buildMessage(errors.toSeq, options))
      }
    }

    state.copy(annotations = remainingAnnotations)
  }
}
