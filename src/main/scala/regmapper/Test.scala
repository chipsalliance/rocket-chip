// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import chisel3._
import chisel3.util.{Cat, log2Ceil}
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.util.{Pow2ClockDivider}

object LFSR16Seed
{
  def apply(seed: Int): UInt =
  {
    val width = 16
    val lfsr = RegInit(((seed*0x7231) % 65536).U(width.W))
    lfsr := Cat(lfsr(0)^lfsr(2)^lfsr(3)^lfsr(5), lfsr(width-1,1))
    lfsr
  }
}

class RRTestCombinational(val bits: Int, rvalid: Bool => Bool, wready: Bool => Bool) extends Module
{
  val io = IO(new Bundle {
    val rvalid = Output(Bool())
    val rready = Input(Bool())
    val rdata  = Output(UInt(bits.W))
    val wvalid = Input(Bool())
    val wready = Output(Bool())
    val wdata  = Input(UInt(bits.W))
  })

  val reg = RegInit(0.U(bits.W))

  val rvalid_s = rvalid(io.rready)
  val wready_s = wready(io.wvalid)
  io.rvalid := rvalid_s
  io.wready := wready_s

  io.rdata := Mux(rvalid_s && io.rready, reg, 0.U)
  when (io.wvalid && wready_s) { reg := io.wdata }
}

object RRTestCombinational
{
  private var seed = 42

  def always: Bool => Bool = _ => true.B

  def random: Bool => Bool = { ready =>
    seed = seed + 1
    LFSR16Seed(seed)(0)
  }

  def delay(x: Int): Bool => Bool = { ready =>
    val reg = RegInit(0.U(log2Ceil(x+1).W))
    val valid = reg === 0.U
    reg := Mux(ready && valid, x.U, Mux(valid, 0.U, reg - 1.U))
    valid
  }

  def combo(bits: Int, rvalid: Bool => Bool, wready: Bool => Bool): RegField = {
    lazy val combo = Module(new RRTestCombinational(bits, rvalid, wready))
    RegField(bits,
      RegReadFn(ready => {combo.io.rready := ready; (combo.io.rvalid, combo.io.rdata) }),
      RegWriteFn((valid, data) => {combo.io.wvalid := valid; combo.io.wdata := data; combo.io.wready }))
  }
}

class RRTestRequest(val bits: Int,
  rflow: (Bool, Bool, UInt) => (Bool, Bool, UInt),
  wflow: (Bool, Bool, UInt) => (Bool, Bool, UInt)) extends Module
{
  val io = IO(new Bundle {
    val rivalid = Input(Bool())
    val riready = Output(Bool())
    val rovalid = Output(Bool())
    val roready = Input(Bool())
    val rdata  = Output(UInt(bits.W))
    val wivalid = Input(Bool())
    val wiready = Output(Bool())
    val wovalid = Output(Bool())
    val woready = Input(Bool())
    val wdata  = Input(UInt(bits.W))
  })

  val (riready, rovalid, _)     = rflow(io.rivalid, io.roready, 0.U(1.W))
  val (wiready, wovalid, wdata) = wflow(io.wivalid, io.woready, io.wdata)
  val reg = RegInit(0.U(bits.W))

  io.riready := riready
  io.rovalid := rovalid
  io.wiready := wiready
  io.wovalid := wovalid

  val rofire = io.roready && rovalid
  val wofire = io.woready && wovalid

  io.rdata := Mux(rofire, reg, 0.U)
  when (wofire) { reg := wdata }
}

object RRTestRequest
{
  private var seed = 1231
  def pipe(x: Int): (Bool, Bool, UInt) => (Bool, Bool, UInt) = { (ivalid, oready, idata) =>
    val full = RegInit(VecInit.fill(x)(false.B))
    val ready = Wire(Vec(x, Bool()))
    val data = Reg(Vec(x, UInt(idata.getWidth.W)))
    // Construct a classic bubble-filling pipeline
    ready(x-1) := oready || !full(x-1)
    when (ready(0)) { data(0) := idata }
    when (ready(0)) { full(0) := ivalid }
    ((ready.init zip ready.tail) zip full.init) foreach { case ((self, next), full) =>
      self := next || !full
    }
    ((data.init zip data.tail) zip ready.tail) foreach { case ((prev, self), ready) =>
      when (ready) { self := prev }
    }
    ((full.init zip full.tail) zip ready.tail) foreach { case ((prev, self), ready) =>
      when (ready) { self := prev }
    }
    (ready(0), full(x-1), data(x-1))
  }

  def busy: (Bool, Bool, UInt) => (Bool, Bool, UInt) = {
    seed = seed + 1
    (ivalid, oready, idata) => {
      val lfsr = LFSR16Seed(seed)
      val busy = RegInit(false.B)
      val data = Reg(UInt(idata.getWidth.W))
      val progress = lfsr(0)
      val iready = progress && !busy
      val ovalid = progress && busy
      when (progress) {
        busy := Mux(busy, !oready, ivalid)
      }
      when (ivalid && iready) { data := idata }
      (iready, ovalid, data)
    }
  }

  def request(bits: Int,
    rflow: (Bool, Bool, UInt) => (Bool, Bool, UInt),
    wflow: (Bool, Bool, UInt) => (Bool, Bool, UInt)): RegField = {
    val request = Module(new RRTestRequest(bits, rflow, wflow))
    RegField(bits,
      RegReadFn { (rivalid, roready) => 
        request.io.rivalid := rivalid
        request.io.roready := roready
        (request.io.riready, request.io.rovalid, request.io.rdata) },
      RegWriteFn { (wivalid, woready, wdata) =>
        request.io.wivalid := wivalid
        request.io.woready := woready
        request.io.wdata := wdata
        (request.io.wiready, request.io.wovalid) })
  }
}

object RRTest0Map
{
  import RRTestCombinational._

  def aa(bits: Int) = combo(bits, always, always)
  def ar(bits: Int) = combo(bits, always, random)
  def ad(bits: Int) = combo(bits, always, delay(11))
  def ae(bits: Int) = combo(bits, always, delay(5))
  def ra(bits: Int) = combo(bits, random, always)
  def rr(bits: Int) = combo(bits, random, random)
  def rd(bits: Int) = combo(bits, random, delay(11))
  def re(bits: Int) = combo(bits, random, delay(5))
  def da(bits: Int) = combo(bits, delay(5), always)
  def dr(bits: Int) = combo(bits, delay(5), random)
  def dd(bits: Int) = combo(bits, delay(5), delay(5))
  def de(bits: Int) = combo(bits, delay(5), delay(11))
  def ea(bits: Int) = combo(bits, delay(11), always)
  def er(bits: Int) = combo(bits, delay(11), random)
  def ed(bits: Int) = combo(bits, delay(11), delay(5))
  def ee(bits: Int) = combo(bits, delay(11), delay(11))

  // All fields must respect byte alignment, or else it won't behave like an SRAM
  def map = Seq(
    0  -> Seq(aa(8), ar(8), ad(8), ae(8)),
    4  -> Seq(ra(8), rr(8), rd(8), re(8)),
    8  -> Seq(da(8), dr(8), dd(8), de(8)),
    12 -> Seq(ea(8), er(8), ed(8), ee(8)),
    16 -> Seq(aa(3), ar(5), ad(1), ae(7), ra(2), rr(6), rd(4), re(4)),
    20 -> Seq(da(3), dr(5), dd(1), de(7), ea(2), er(6), ed(4), ee(4)),
    24 -> Seq(aa(8), rr(8), dd(8), ee(8)),
    28 -> Seq(ar(8), rd(8), de(8), ea(8)))
}

object RRTest1Map
{
  import RRTestRequest._

  def pp(bits: Int) = request(bits, pipe(3), pipe(3))
  def pb(bits: Int) = request(bits, pipe(3), busy)
  def bp(bits: Int) = request(bits, busy, pipe(3))
  def bb(bits: Int) = request(bits, busy, busy)

  def map = RRTest0Map.map.take(6) ++ Seq(
    24 -> Seq(pp(8), pb(8), bp(8), bb(8)),
    28 -> Seq(pp(3), pb(5), bp(1), bb(7), pb(5), bp(3), pp(4), bb(4)))
}

abstract class RRTest0(address: BigInt)(implicit p: Parameters)
  extends RegisterRouter(RegisterRouterParams(
    name = "test0",
    compat = Nil,
    base = address,
    size = 32))
{
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) { regmap(RRTest0Map.map:_*) }
}

abstract class RRTest1(address: BigInt, concurrency: Int, undefZero: Boolean = true)(implicit p: Parameters)
  extends RegisterRouter(RegisterRouterParams(
    name = "test1",
    compat = Nil,
    base = address,
    size = 32,
    concurrency = concurrency,
    undefZero = undefZero))
{
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val clocks = Module(new Pow2ClockDivider(2))

    def x(bits: Int) = {
      val field = UInt(bits.W)

      val readCross = Module(new RegisterReadCrossing(field))
      readCross.io := DontCare
      readCross.io.master_clock  := clock
      readCross.io.master_reset  := reset
      readCross.io.master_bypass := false.B
      readCross.io.slave_clock   := clocks.io.clock_out
      readCross.io.slave_reset   := reset

      val writeCross = Module(new RegisterWriteCrossing(field))
      writeCross.io := DontCare
      writeCross.io.master_clock  := clock
      writeCross.io.master_reset  := reset
      writeCross.io.master_bypass := false.B
      writeCross.io.slave_clock   := clocks.io.clock_out
      writeCross.io.slave_reset   := reset

      readCross.io.slave_register := writeCross.io.slave_register
      RegField(bits, readCross.io.master_port, writeCross.io.master_port)
    }

    val map = RRTest1Map.map.drop(1) ++ Seq(0 -> Seq(x(8), x(8), x(8), x(8)))
    regmap(map:_*)
  }
}
