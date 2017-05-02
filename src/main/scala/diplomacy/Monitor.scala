// See LICENSE.SiFive for license details.

package diplomacy

import Chisel._
import chisel3.internal.sourceinfo.{SourceInfo, SourceLine}
import config._

abstract class MonitorBase(implicit sourceInfo: SourceInfo, p: Parameters) extends LazyModule()(p) {
  override val module: LazyModuleImp
}
