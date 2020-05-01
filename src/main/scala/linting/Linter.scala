// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting

import firrtl._
import firrtl.ir._
import chisel3.experimental.annotate

object Linter {

  /** Use to whitelist specific files from specific linting rules
    */
  def whitelist(lintNumber: Int, filenames: String*) = {
    lintMap.get(lintNumber.toString) match {
      case Some(_) => annotate(Whitelist(lintNumber, filenames.toSet))
      case None => 
    }
  }
  def whitelist(lintName: String, filenames: String*) = {
    lintMap.get(lintName) match {
      case Some(l) => annotate(Whitelist(l.lintNumber, filenames.toSet))
      case None => 
    }
  }

  private[linting] lazy val linters = Seq(
    new rule.LintAnonymousRegisters,
    new rule.LintTruncatingWidths
  )

  private [linting] lazy val lintMap = linters.flatMap {
    l => Seq(l.lintNumber.toString -> l, l.lintName -> l)
  }.toMap
}

