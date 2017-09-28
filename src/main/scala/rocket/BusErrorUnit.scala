// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import chisel3.util.Valid
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

trait BusErrors extends Bundle {
  def toErrorList: List[Option[Valid[UInt]]]
}

class L1BusErrors(implicit p: Parameters) extends CoreBundle()(p) with BusErrors {
  val icache = new ICacheErrors
  val dcache = new DCacheErrors

  def toErrorList = 
    List(None, None, icache.correctable, icache.uncorrectable,
         None, Some(dcache.bus), dcache.correctable, dcache.uncorrectable)
}

case class BusErrorUnitParams(addr: BigInt, size: Int = 4096)

class BusErrorUnit[T <: BusErrors](t: => T, params: BusErrorUnitParams)(implicit p: Parameters) extends LazyModule {
  val regWidth = 64
  val device = new SimpleDevice("bus-error-unit", Seq("sifive,buserror0"))
  val intNode = IntSourceNode(IntSourcePortSimple(resources = device.int))
  val node = TLRegisterNode(
    address   = Seq(AddressSet(params.addr, params.size-1)),
    device    = device,
    beatBytes = p(XLen)/8)

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val errors = t.flip
    })

    val sources = io.errors.toErrorList
    val mask = sources.map(_.nonEmpty.B).asUInt
    val cause = Reg(init = UInt(0, log2Ceil(sources.lastIndexWhere(_.nonEmpty) + 1)))
    val value = Reg(UInt(width = sources.flatten.map(_.bits.getWidth).max))
    require(value.getWidth <= regWidth)
    val enable = Reg(init = mask)
    val interrupt = Reg(init = UInt(0, sources.size))
    val accrued = Reg(init = UInt(0, sources.size))

    accrued := accrued | sources.map(_.map(_.valid).getOrElse(false.B)).asUInt

    for ((s, i) <- sources.zipWithIndex; if s.nonEmpty) {
      when (s.get.valid && enable(i) && cause === 0) {
        cause := i
        value := s.get.bits
      }
    }

    val (int_out, _) = intNode.out(0)
    int_out(0) := (accrued & interrupt).orR

    def reg(r: UInt) = RegField(regWidth, r)
    def maskedReg(r: UInt, m: UInt) = RegField(regWidth, r, RegWriteFn((v, d) => { when (v) { r := d & m }; true }))

    node.regmap(
      0 -> Seq(reg(cause),
               reg(value),
               maskedReg(enable, mask),
               maskedReg(interrupt, mask),
               maskedReg(accrued, mask)))
  }
}
