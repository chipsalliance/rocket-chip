// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.debug
import Chisel._
import chisel3.experimental._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.amba.apb.{APBRegisterNode}

case object APBDebugRegistersKey extends Field[Map[Int, Seq[RegField]]](Map())

class APBDebugRegisters()(implicit p: Parameters) extends LazyModule {

  val node = APBRegisterNode(
    address = AddressSet(base=0xF00, mask=0xFF),
    beatBytes = 4,
    executable = false
  )

  lazy val module = new LazyModuleImp(this){
    node.regmap(p(APBDebugRegistersKey).toList:_*)

  }
}
