// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import chisel3.experimental.SourceInfo

import org.chipsalliance.cde.config._

import freechips.rocketchip.devices.debug.DebugModuleKey
import freechips.rocketchip.diplomacy.RegionType
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tile.{CoreModule, CoreBundle}
import freechips.rocketchip.tilelink.{TLSlavePortParameters, TLManagerParameters}

class PMAChecker(manager: TLSlavePortParameters)(implicit p: Parameters) extends CoreModule()(p) {
  val io = IO(new Bundle {
    val paddr = Input(UInt())

    val resp = Output(new Bundle {
      val cacheable = Bool()
      val r = Bool()
      val w = Bool()
      val pp = Bool()
      val al = Bool()
      val aa = Bool()
      val x = Bool()
      val eff = Bool()
    })
  })

  // PMA
  // check exist a slave can consume this address.
  val legal_address = manager.findSafe(io.paddr).reduce(_||_)
  // check utility to help check SoC property.
  def fastCheck(member: TLManagerParameters => Boolean) =
    legal_address && manager.fastProperty(io.paddr, member, (b:Boolean) => b.B)

  io.resp.cacheable := fastCheck(_.supportsAcquireB)
  io.resp.r := fastCheck(_.supportsGet)
  io.resp.w := fastCheck(_.supportsPutFull)
  io.resp.pp := fastCheck(_.supportsPutPartial)
  io.resp.al := fastCheck(_.supportsLogical)
  io.resp.aa := fastCheck(_.supportsArithmetic)
  io.resp.x := fastCheck(_.executable)
  io.resp.eff := fastCheck(Seq(RegionType.PUT_EFFECTS, RegionType.GET_EFFECTS) contains _.regionType)
}
