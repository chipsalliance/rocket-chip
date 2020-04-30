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


final class LintAnonymousRegisters extends Linter {
  // Import useful utility functions
  import Linter.{isTemporary, getName, getScalaInfo, isWhitelisted, Errors, updateErrors}

  val recommendedFix: String = "Use @chiselName or *.suggestName(...)"

  val lintNumber = 0

  val lintName: String = "anon-regs"

  override def optionalPrerequisiteOf = super.optionalPrerequisiteOf :+ Dependency(firrtl.passes.LowerTypes)

  override protected def lintStatement(errors: Errors, mname: String)(s: Statement): Unit = {
    s match {
      case r: DefRegister  if isTemporary(r.name) =>
        getScalaInfo(r.info) match {
          case Some(scalaInfo: FileInfo) => updateErrors(scalaInfo, "", errors, mname)
          case None => updateErrors(r.info, "", errors, mname)
        }
      case other =>
    }
    super.lintStatement(errors, mname)(s)
  }
}
