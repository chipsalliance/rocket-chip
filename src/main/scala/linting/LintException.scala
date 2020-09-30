// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting

import firrtl.FirrtlUserException

/** Thrown to report all linting rule violations, according to the display options */
case class LintException(seq: Seq[Violation], lintDisplayOptions: DisplayOptions) extends FirrtlUserException(
  LintException.buildMessage(seq, lintDisplayOptions)
)

object LintException {
  private def makeNumber(max: Int, n: Int, prefix: String): String = {
    val nDigits = max.toString.size
    val nLeading = nDigits - n.toString.size
    prefix * nLeading + n.toString
  }

  private[linting] def buildMessage(seq: Seq[Violation], lintDisplayOptions: DisplayOptions): String = {
    val groupedErrors = seq.groupBy {
      case l: Violation => l.linter.lintName
    }
    val maxErrorNumber = groupedErrors.keys.max

    val (_, reports) = groupedErrors.toSeq.sortBy(_._1).reverse.foldRight((0, Seq.empty[String])) {
      case ((lintName: String, lintErrors: Seq[Violation]), (totalErrors: Int, reportsSoFar: Seq[String])) =>
        val le                  = lintErrors.head.linter
        val perErrorLimit       = lintDisplayOptions.perErrorLimit.getOrElse(lintName, lintErrors.size)
        val totalErrorLimit     = lintDisplayOptions.totalLimit.map(t => t - totalErrors).getOrElse(perErrorLimit)
        val remainingErrorLimit = totalErrorLimit.min(perErrorLimit)
        val scalaFiles          = lintErrors.flatMap(_.getScalaFiles).distinct
        val lintString          = lintName
        val header =
          s"""
             |Lint rule ${le.lintName}: ${lintErrors.size} exceptions!
             | - Recommended fix:
             |     ${le.recommendedFix}
             | - Whitelist file via Chisel cmdline arg:
             |     ${le.whitelistAPI(scalaFiles)}
             | - Whitelist file via Chisel scala API:
             |     ${le.scalaAPI(scalaFiles)}
             | - Disable this linting check:
             |     ${le.disableCLI}
             | - Modify display settings with:
             |     --lint-options ...,display:${lintName}=<number>,...
             |""".stripMargin

        val errors = lintErrors.zip(1 to remainingErrorLimit).map {
          case (lint: Violation, idx: Int) =>
            s"$lintString.${makeNumber(remainingErrorLimit.min(lintErrors.size),idx,"0")}:${lint.info} ${lint.message} in ${lint.modules}"
          }.mkString("\n")

        (totalErrors + remainingErrorLimit, (header + errors) +: reportsSoFar)
    }
    reports.reverse.mkString("\n")
  }
}
