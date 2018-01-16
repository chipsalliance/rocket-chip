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
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util.property._

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
      val interrupt = Bool().asOutput
    })

    val sources = io.errors.toErrorList
    val cause = Reg(init = UInt(0, log2Ceil(sources.lastIndexWhere(_.nonEmpty) + 1)))
    val value = Reg(UInt(width = sources.flatten.map(_.bits.getWidth).max))
    require(value.getWidth <= regWidth)
    val enable = Reg(init = Vec(sources.map(_.nonEmpty.B)))
    val global_interrupt = Reg(init = Vec.fill(sources.size)(false.B))
    val accrued = Reg(init = Vec.fill(sources.size)(false.B))
    val local_interrupt = Reg(init = Vec.fill(sources.size)(false.B))

    for ((((s, en), acc), i) <- (sources zip enable zip accrued).zipWithIndex; if s.nonEmpty) {
      when (s.get.valid) {
        acc := true
        when (en && cause === 0) {
          cause := i
          value := s.get.bits
        }
        cover(en, s"BusErrorCause_$i", s"Core;;BusErrorCause $i covered")
      }
    }

    val (int_out, _) = intNode.out(0)
    io.interrupt := (accrued.asUInt & local_interrupt.asUInt).orR
    int_out(0) := (accrued.asUInt & global_interrupt.asUInt).orR

    def reg(r: UInt) = RegField.bytes(r, (r.getWidth + 7)/8)
    def reg(v: Vec[Bool]) = v.map(r => RegField(1, r))
    def numberRegs(x: Seq[Seq[RegField]]) = x.zipWithIndex.map { case (f, i) => (i * regWidth / 8) -> f }

    node.regmap(numberRegs(Seq(
      reg(cause),
      reg(value),
      reg(enable),
      reg(global_interrupt),
      reg(accrued),
      reg(local_interrupt))):_*)

    // hardwire mask bits for unsupported sources to 0
    for ((s, i) <- sources.zipWithIndex; if s.isEmpty) {
      enable(i) := false
      global_interrupt(i) := false
      accrued(i) := false
      local_interrupt(i) := false
    }
  }
}
