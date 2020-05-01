// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting

import firrtl.FirrtlUserException
import firrtl.ir.FileInfo

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
      case l: Violation => l.linter.lintNumber
    }
    val maxErrorNumber = groupedErrors.keys.max

    val (_, reports) = groupedErrors.toSeq.sortBy(_._1).reverse.foldRight((0, Seq.empty[String])) {
      case ((lintNumber: Int, lintErrors: Seq[Violation]), (totalErrors: Int, reportsSoFar: Seq[String])) =>
        val le = lintErrors.head.linter
        val perErrorLimit = lintDisplayOptions.perErrorLimit.getOrElse(lintNumber, lintErrors.size)
        val totalErrorLimit = lintDisplayOptions.totalLimit.map(t => t - totalErrors).getOrElse(perErrorLimit)
        val actualNumErrors = totalErrorLimit.min(perErrorLimit)
        val scalaFiles = lintErrors.flatMap(_.getScalaFiles).distinct
        val lintString = makeNumber(maxErrorNumber, lintNumber, " ")
        val report =
          s"""
             |Lint Error #$lintNumber. ${le.lintName}: ${lintErrors.size} exceptions!
             | - Recommended fix:
             |     ${le.recommendedFix}
             | - Whitelist file via Chisel cmdline arg:
             |     ${le.whitelistAPI(scalaFiles)}
             | - Whitelist file via Chisel scala API:
             |     ${le.scalaAPI(scalaFiles)}
             | - Disable this linting check:
             |     ${le.disableCLI}
             |${lintErrors.zip(1 to actualNumErrors)
                   .map { case (lint: Violation, idx: Int) => s"#$lintString.${makeNumber(actualNumErrors - 1,idx,"0")}:${lint.info} ${lint.message} in ${lint.modules}"}
                   .mkString("\n")}""".stripMargin
        (totalErrors + actualNumErrors, report +: reportsSoFar)
    }
    reports.reverse.mkString("\n")
  }
}


