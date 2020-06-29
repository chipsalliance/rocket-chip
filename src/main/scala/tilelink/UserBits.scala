// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._

class CWFRequestBundle(val blockBytes: Int) extends Bundle {
  val desired_start_byte = UInt(log2Ceil(blockBytes).W)
}

class CWFResponseBundle(val blockBytes: Int) extends Bundle {
  // Will be = desired_start_byte & (~0.U << z) for some z >= log2Ceil(beatBytes).
  val actual_start_byte = UInt(log2Ceil(blockBytes).W)
}

case object CWFRequest  extends ControlKey[CWFRequestBundle] ("cwf")
case object CWFResponse extends ControlKey[CWFResponseBundle]("cwf")

case class CWFRequestField(val blockBytes: Int) extends BundleField(CWFRequest) {
  def data = Output(new CWFRequestBundle(blockBytes))
  def default(x: CWFRequestBundle) {
    x.desired_start_byte := 0.U
  }
}

case class CWFResponseField(val blockBytes: Int) extends BundleField(CWFResponse) {
  def data = Output(new CWFResponseBundle(blockBytes))
  def default(x: CWFResponseBundle) {
    x.actual_start_byte := 0.U
  }
}
