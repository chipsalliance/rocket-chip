// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import chisel3.internal.sourceinfo.{SourceInfo, SourceLine}
import freechips.rocketchip.config.Parameters

abstract class MonitorBase(implicit sourceInfo: SourceInfo, p: Parameters) extends LazyModule()(p) {
  override val module: LazyModuleImp
}
