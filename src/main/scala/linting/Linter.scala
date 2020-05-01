// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting

import firrtl._
import firrtl.ir._
import chisel3.experimental.annotate

object Linter {

  /** Use to whitelist specific files from specific linting rules
    */
  def whitelist(lintName: String, filenames: String*) = {
    require(lintMap.contains(lintName), s"Unknown lint name: $lintName")
    annotate(Whitelist(lintName, filenames.toSet))
  }

  private[linting] lazy val linters = Seq(
    new rule.LintAnonymousRegisters,
    new rule.LintTruncatingWidths
  )

  private [linting] lazy val lintMap = linters.flatMap {
    l => Seq(l.lintName -> l)
  }.toMap
}

