// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting

import chisel3.experimental.annotate

/** Chisel users: Use to whitelist files
  * Lint rule writers: update linters list whenever a new lint rule is created
  */
object Linter {

  /** Use to whitelist specific files from specific linting rules
    *
    * @param lintRuleName the name of the lint rule
    * @param filenames scala files to except from this linting rule
    */
  def whitelist(lintRuleName: String, filenames: String*) = {
    require(lintMap.contains(lintRuleName), s"Unknown lint name: $lintRuleName")
    annotate(Whitelist(lintRuleName, filenames.toSet))
  }

  // Update list for any new lint rule
  private[linting] lazy val linters = Seq(
    new rule.LintAnonymousRegisters,
    new rule.LintTruncatingWidths,
    new rule.LintConflictingModuleNames
  )

  private [linting] lazy val lintMap = linters.flatMap {
    l => Seq(l.lintName -> l)
  }.toMap
}
