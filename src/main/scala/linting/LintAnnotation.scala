// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting

import firrtl.ir.{Info, FileInfo}
import firrtl.annotations.NoTargetAnnotation
import chisel3.experimental.ChiselAnnotation

trait LintAnnotation extends NoTargetAnnotation with ChiselAnnotation {
  override def toFirrtl = this
}

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

case class Whitelist(lintNumber: Int, whiteList: Set[String]) extends LintAnnotation

case class DisplayOptions(
    level: String  = "strict",
    totalLimit: Option[Int] = None,
    perErrorLimit: Map[Int, Int] = Map.empty
) extends LintAnnotation
