// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting

import firrtl._
import firrtl.ir._
import firrtl.traversals.Foreachers._
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{OptionsException, RegisteredLibrary, ShellOption, Dependency}
import firrtl.stage.RunFirrtlTransformAnnotation
import chisel3.experimental.ChiselAnnotation
import scala.collection.mutable


final class LintTruncatingWidths extends Linter {
  // Import useful utility functions
  import Linter.{isTemporary, getName, getScalaInfo, isWhitelisted, Errors, updateErrors}

  val recommendedFix: String = "Truncate width prior to connections"
  override def optionalPrerequisites = Seq(
    Dependency(firrtl.passes.ExpandConnects),
    Dependency[firrtl.passes.InferWidths]
  )

  override def optionalPrerequisiteOf = super.optionalPrerequisiteOf :+ Dependency[firrtl.passes.ExpandWhensAndCheck]

  val lintNumber = 1

  val lintName: String = "trunc-widths"

  override protected def lintStatement(errors: Errors, mname: String)(s: Statement): Unit = {
    s match {
      case c@Connect(info, loc, expr) => (loc.tpe, expr.tpe) match {
        case (GroundType(IntWidth(locWidth)), GroundType(IntWidth(exprWidth))) if exprWidth > locWidth =>
          val message = s"${c.copy(info = NoInfo).serialize} // Connecting width ${exprWidth} to width ${locWidth}"
          getScalaInfo(info) match {
            case Some(scalaInfo: FileInfo) =>
              updateErrors(scalaInfo, message, errors, mname)
            case None => updateErrors(info, message, errors, mname)
          }
        case other =>
      }
      case other =>
    }
    super.lintStatement(errors, mname)(s)
  }
}
