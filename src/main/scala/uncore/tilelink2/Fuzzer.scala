// See LICENSE for license details.
package uncore.tilelink2

import Chisel._
import chisel3.util.LFSR16
import junctions.unittests._

object LFSR64
{ 
  private var counter = 0 
  private def next: Int = {
    counter += 1
    counter
  }
  
  def apply(increment: Bool = Bool(true), seed: Int = next): UInt =
  { 
    val wide = 64
    val lfsr = RegInit(UInt((seed * 0xDEADBEEFCAFEBAB1L) >>> 1, width = wide))
    val xor = lfsr(0) ^ lfsr(1) ^ lfsr(3) ^ lfsr(4)
    when (increment) { lfsr := Cat(xor, lfsr(wide-1,1)) }
    lfsr
  }
}

object NoiseMaker
{
  def apply(wide: Int, increment: Bool = Bool(true)): UInt = {
    val lfsrs = Seq.fill((wide+63)/64) { LFSR64(increment) }
    Cat(lfsrs)(wide-1,0)
  }
}

object MaskMaker
{ 
  def apply(wide: Int, bits: UInt): UInt = 
    Vec.tabulate(wide) {UInt(_) < bits} .asUInt
}


class TLFuzzer(nOperations: Int) extends LazyModule
{
  val node = TLClientNode(TLClientParameters())

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val out = node.bundleOut
      val finished = Bool()
    }

    val out = io.out(0)
    val edge = node.edgesOut(0)

    val idx = Reg(init = UInt(nOperations-1, log2Up(nOperations)))
    val finished = RegInit(Bool(false))
    val valid    = RegInit(Bool(false))
    valid := Bool(true)
    io.finished  := finished

    val counter = RegInit(UInt(0, width = log2Up(edge.maxTransfer)))
    val inc = Wire(Bool())

    val addrBits = log2Up(edge.manager.maxAddress + 1)
    val amo_size  = UInt(2) + NoiseMaker(1, inc) // word or dword
    val size      = NoiseMaker(edge.bundle.sizeBits, inc)
    val addr_mask = MaskMaker(addrBits, size)
    val addr      = NoiseMaker(addrBits, inc) & ~addr_mask
    val wmask     = NoiseMaker(edge.manager.beatBytes, inc)
    val data      = NoiseMaker(edge.bundle.dataBits, inc)
    val arth_op   = NoiseMaker(3, inc)
    val log_op    = NoiseMaker(2, inc) 
    val src = UInt(0)

    val (glegal,  gbits)  = edge.Get(src, addr, size)
    val (pflegal, pfbits) = if(edge.manager.anySupportPutFull) { 
                              edge.Put(src, addr, size, data)
                            } else { (glegal, gbits) }
    val (pplegal, ppbits) = if(edge.manager.anySupportPutPartial) {
                              edge.Put(src, addr, size, data, wmask)
                            } else { (glegal, gbits) }
    val (alegal,  abits)  = if(edge.manager.anySupportArithmetic) {
                              edge.Arithmetic(src, addr, size, data, arth_op)
                            } else { (glegal, gbits) }
    val (llegal,  lbits)  = if(edge.manager.anySupportLogical) {
                             edge.Logical(src, addr, size, data, log_op)
                            } else { (glegal, gbits) }
    val (hlegal,  hbits)  = if(edge.manager.anySupportHint) {
                              edge.Hint(src, addr, size, UInt(0))
                            } else { (glegal, gbits) }

    val a_type_sel  = NoiseMaker(3, inc)

    val legal = MuxLookup(a_type_sel, glegal, Seq(
      UInt("b000") -> glegal,
      UInt("b001") -> pflegal,
      UInt("b010") -> pplegal,
      UInt("b011") -> alegal,
      UInt("b100") -> llegal,
      UInt("b101") -> hlegal))

    val bits = MuxLookup(a_type_sel, gbits, Seq(
      UInt("b000") -> gbits,
      UInt("b001") -> pfbits,
      UInt("b010") -> ppbits,
      UInt("b011") -> abits,
      UInt("b100") -> lbits,
      UInt("b101") -> hbits))

    out.a.valid := legal
    out.a.bits  := bits
    out.b.ready := Bool(true)
    out.c.valid := Bool(false)
    out.d.ready := Bool(true)
    out.e.valid := Bool(false)

    inc := !legal || (out.a.fire() && counter === UInt(0))
    when (out.a.fire()) {
      counter := counter - UInt(1)
      when (counter === UInt(0)) {
        counter := edge.numBeats(out.a.bits) - UInt(1)
	idx := idx - UInt(1)
      }
      when (idx === UInt(0)) { finished := Bool(true) }
    }
  }
}

class TLFuzzRAM extends LazyModule
{
  val ram  = LazyModule(new TLRAM(AddressSet(0, 0xfff)))
  val xbar = LazyModule(new TLXbar)
  val fuzz = LazyModule(new TLFuzzer(1000))

  connect(TLWidthWidget(TLHintHandler(fuzz.node), 16) -> xbar.node)
  connect(TLFragmenter(TLBuffer(xbar.node), 4, 256) -> ram.node)

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished
  }
}

class TLFuzzRAMTest extends UnitTest {
  val dut = LazyModule(new TLFuzzRAM).module
  io.finished := dut.io.finished
}
