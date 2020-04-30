// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting

import firrtl.annotations.NoTargetAnnotation
import firrtl.ir.{Info, FileInfo}
import chisel3.experimental.ChiselAnnotation

case class LintError(linter: Linter, info: Info, message: String, modules: Set[String]) extends NoTargetAnnotation {

  def getScalaFiles: Seq[String] = {
    val scala = "(.*\\.scala).*".r
    Linter.flatten(info).flatMap {
      case f: FileInfo => f.info.serialize match {
        case scala(file) => Some(file)
        case other => None
      }
      case other => None
    }
  }
}

case class Whitelist(lintNumber: Int, whiteList: Set[String]) extends NoTargetAnnotation with ChiselAnnotation {
  override def toFirrtl = this
}
