// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.debug

import Chisel._
import chisel3.core.{Input, Output}
import freechips.rocketchip.diplomacy.{BundleBridgeSource}

case class DebugCustomParams(
  addrs: List[Int],
  width: Int
) {
  require (width % 8 == 0, s"Currently only support custom debug widths which are multiples of 8, not ${width}")
}

class DebugCustomIO(val p: DebugCustomParams) extends Bundle {
  val addr = Output(UInt(log2Up(p.addrs.reduce(_ max _)).W))
  val data = Input(UInt(p.width.W))
  val ready = Input(Bool())
  val valid = Output(Bool())
}
