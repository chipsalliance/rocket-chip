// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

object LFSR16Seed
{
  def apply(seed: Int): UInt =
  {
    val width = 16
    val lfsr = Reg(init=UInt(seed, width))
    lfsr := Cat(lfsr(0)^lfsr(2)^lfsr(3)^lfsr(5), lfsr(width-1,1))
    lfsr
  }
}

class RRTestCombinational(val bits: Int, rvalid: Bool => Bool, wready: Bool => Bool) extends Module
{
  val io = new Bundle {
    val rvalid = Bool(OUTPUT)
    val rready = Bool(INPUT)
    val rdata  = UInt(OUTPUT, width = bits)
    val wvalid = Bool(INPUT)
    val wready = Bool(OUTPUT)
    val wdata  = UInt(INPUT, width = bits)
  }

  val rfire = io.rvalid && io.rready
  val wfire = io.wvalid && io.wready
  val reg = Reg(UInt(width = bits))

  io.rvalid := rvalid(rfire)
  io.wready := wready(wfire)

  io.rdata := Mux(rfire, reg, UInt(0))
  when (wfire) { reg := io.wdata }
}

object RRTestCombinational
{
  private var seed = 0
  def always: Bool => Bool = _ => Bool(true)
  def random: Bool => Bool = {
    seed = seed + 1
    val lfsr = LFSR16Seed(seed)
    _ => lfsr(0)
  }
  def delay(x: Int): Bool => Bool = { fire =>
    val reg = RegInit(UInt(0, width = log2Ceil(x+1)))
    val ready = reg === UInt(0)
    reg := Mux(fire, UInt(x), Mux(ready, UInt(0), reg - UInt(1)))
    ready
  }

  def combo(bits: Int, rvalid: Bool => Bool, wready: Bool => Bool): RegField = {
    val combo = Module(new RRTestCombinational(bits, rvalid, wready))
    RegField(bits,
      RegReadFn { ready => combo.io.rready := ready; (combo.io.rvalid, combo.io.rdata) },
      RegWriteFn { (valid, data) => combo.io.wvalid := valid; combo.io.wdata := data; combo.io.wready })
  }
}

class RRTestRequest(val bits: Int,
  rflow: (Bool, Bool, UInt) => (Bool, Bool, UInt),
  wflow: (Bool, Bool, UInt) => (Bool, Bool, UInt)) extends Module
{
  val io = new Bundle {
    val rivalid = Bool(INPUT)
    val riready = Bool(OUTPUT)
    val rovalid = Bool(OUTPUT)
    val roready = Bool(INPUT)
    val rdata  = UInt(OUTPUT, width = bits)
    val wivalid = Bool(INPUT)
    val wiready = Bool(OUTPUT)
    val wovalid = Bool(OUTPUT)
    val woready = Bool(INPUT)
    val wdata  = UInt(INPUT, width = bits)
  }

  val (riready, rovalid, _)     = rflow(io.rivalid, io.roready, UInt(0, width = 1))
  val (wiready, wovalid, wdata) = wflow(io.wivalid, io.woready, io.wdata)
  val reg = Reg(UInt(width = bits))

  io.riready := riready
  io.rovalid := rovalid
  io.wiready := wiready
  io.wovalid := wovalid

  val rofire = io.roready && rovalid
  val wofire = io.woready && wovalid

  io.rdata := Mux(rofire, reg, UInt(0))
  when (wofire) { reg := wdata }
}

object RRTestRequest
{
  private var seed = 0
  def pipe(x: Int): (Bool, Bool, UInt) => (Bool, Bool, UInt) = { (ivalid, oready, idata) =>
    val full = RegInit(Vec.fill(x)(Bool(false)))
    val ready = Wire(Vec.fill(x)(Bool()))
    val data = Reg(Vec.fill(x)(UInt(width = idata.getWidth)))
    // Construct a classic bubble-filling pipeline
    ready(x) := oready || !full(x)
    when (ready(0)) { data(0) := idata }
    ((ready.init zip ready.tail) zip full.init) foreach { case ((self, next), full) =>
      self := next || !full
    }
    ((data.init zip data.tail) zip ready.tail) foreach { case ((prev, self), ready) =>
      when (ready) { self := prev }
    }
    (ready(0), full(x), Mux(full(x) && oready, data(x), UInt(0)))
  }
  def busy: (Bool, Bool, UInt) => (Bool, Bool, UInt) = {
    seed = seed + 1
    val lfsr = LFSR16Seed(seed)
    (ivalid, oready, idata) => {
      val busy = RegInit(Bool(false))
      val progress = lfsr(0)
      when (progress) {
        busy := Mux(busy, !oready, ivalid)
      }
      (progress && !busy, progress && busy, idata)
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
