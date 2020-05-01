// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting
package rule

import firrtl.ir._
import firrtl.options.Dependency


/** Reports all anonymous registers in design
  * An anonymous register is one which is prefixed with an "_"
  */
final class LintAnonymousRegisters extends LintRule {

  val recommendedFix: String = "Use named intermediate val, or if that fails use @chiselName or *.suggestName(...)"

  val lintNumber = 0

  val lintName: String = "anon-regs"

  // Should run before LowerTypes so anonymous aggregate registers are reported as one register
  override def optionalPrerequisiteOf = super.optionalPrerequisiteOf :+ Dependency(firrtl.passes.LowerTypes)

  override protected def lintStatement(errors: Errors, mname: String)(s: Statement): Unit = {
    s match {
      case r: DefRegister  if isTemporary(r.name) =>
        // Report scala info, if its present. Otherwise, use existing Info
        getScalaInfo(r.info) match {
          case Some(scalaInfo: FileInfo) => updateErrors(scalaInfo, "", errors, mname)
          case None                      => updateErrors(r.info, "", errors, mname)
        }
      case other =>
    }
    super.lintStatement(errors, mname)(s)
  }
}
