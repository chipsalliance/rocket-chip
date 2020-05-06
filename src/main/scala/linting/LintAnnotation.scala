// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting

import firrtl.ir.{Info, FileInfo}
import firrtl.annotations.NoTargetAnnotation
import chisel3.experimental.ChiselAnnotation

/** Parent trait for all linting annotations */
trait LintAnnotation extends NoTargetAnnotation with ChiselAnnotation {
  override def toFirrtl = this
}

/** Represents a linting violation under a given linter rule */
case class Violation(linter: rule.LintRule, info: Info, message: String, modules: Set[String]) extends LintAnnotation {
  def getScalaFiles: Seq[String] = {
    val scala = "(.*\\.scala).*".r
    rule.flatten(info).flatMap {
      case f: FileInfo => f.info.serialize match {
        case scala(file) => Some(file)
        case other => None
      }
      case other => None
    }
  }
}

/** A list of files to ignore lint violations on, for a given lint rule */
case class Whitelist(lintName: String, whiteList: Set[String]) extends LintAnnotation

/** A container of lint rule violation display options */
case class DisplayOptions(
    level: String  = "strict",
    totalLimit: Option[Int] = None,
    perErrorLimit: Map[String, Int] = Map.empty
) extends LintAnnotation
