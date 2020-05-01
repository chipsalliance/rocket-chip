// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting
package rule

import firrtl.ir._
import firrtl.options.Dependency

/** Reports all connections from a wider signal to a smaller signal
  * Includes subfields of bulk connections
  */
final class LintTruncatingWidths extends LintRule {

  override def optionalPrerequisites = Seq(
    Dependency(firrtl.passes.ExpandConnects), // Require expanding connects to see subfield bulk assignments
    Dependency[firrtl.passes.InferWidths]     // Require widths to have been inferred
  )

  // Run prior to expand whens to get better fileinfo information
  override def optionalPrerequisiteOf = super.optionalPrerequisiteOf :+ Dependency[firrtl.passes.ExpandWhensAndCheck]

  val lintName: String = "trunc-widths"

  val recommendedFix: String = "Truncate width prior to connections"

  override protected def lintStatement(violations: Violations, mname: String)(s: Statement): Unit = {
    s match {
      case c@Connect(info, loc, expr) => (loc.tpe, expr.tpe) match {
        case (GroundType(IntWidth(locWidth)), GroundType(IntWidth(exprWidth))) if exprWidth > locWidth =>
          val message = s"${c.copy(info = NoInfo).serialize} // Connecting width ${exprWidth} to width ${locWidth}"
          getScalaInfo(info) match {
            case Some(scalaInfo: FileInfo) =>
              updateViolations(scalaInfo, message, violations, mname)
            case None => updateViolations(info, message, violations, mname)
          }
        case other =>
      }
      case other =>
    }
    super.lintStatement(violations, mname)(s)
  }
}
