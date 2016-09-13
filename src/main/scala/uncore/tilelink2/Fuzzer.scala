// See LICENSE for license details.
package uncore.tilelink2

import Chisel._
import chisel3.util.LFSR16
import junctions.unittests._

class IDMapGenerator(numIds: Int) extends Module {
  val w = log2Up(numIds)
  val io = new Bundle {
    val free = Decoupled(UInt(width = w)).flip
    val alloc = Decoupled(UInt(width = w))
  }

  // True indicates that the id is available
  val bitmap = RegInit(Vec.fill(numIds){Bool(true)})

  io.free.ready := Bool(true)
  assert(!io.free.valid || !bitmap(io.free.bits)) // No double freeing

  val mask = bitmap.scanLeft(Bool(false))(_||_).init
  val select = mask zip bitmap map { case(m,b) => !m && b }
  io.alloc.bits := OHToUInt(select)
  io.alloc.valid := bitmap.reduce(_||_)

  when (io.alloc.fire()) {
    bitmap(io.alloc.bits) := Bool(false)
  }

  when (io.free.fire()) {
    bitmap(io.free.bits) := Bool(true)
  }
}

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

class TLFuzzer(nOperations: Int, inFlight: Int = 32) extends LazyModule
{
  val node = TLClientNode(TLClientParameters(sourceId = IdRange(0,inFlight)))

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val out = node.bundleOut
      val finished = Bool()
    }

    val out = io.out(0)
    val edge = node.edgesOut(0)
    val endAddress   = edge.manager.maxAddress + 1
    val maxTransfer  = edge.manager.maxTransfer
    val beatBytes    = edge.manager.beatBytes
    val maxLgBeats   = log2Up(maxTransfer/beatBytes)
    val addressBits  = log2Up(endAddress)
    val sizeBits     = edge.bundle.sizeBits
    val dataBits     = edge.bundle.dataBits

    // Progress through operations
    val num_reqs = Reg(init = UInt(nOperations-1, log2Up(nOperations)))
    val num_resps = Reg(init = UInt(nOperations-1, log2Up(nOperations)))
    io.finished  := num_resps === UInt(0)

    // Progress within each operation
    val a = out.a.bits
    val a_beats1 = edge.numBeats1(a)
    val a_counter = RegInit(UInt(0, width = maxLgBeats))
    val a_counter1 = a_counter - UInt(1)
    val a_first = a_counter === UInt(0)
    val a_last  = a_counter === UInt(1) || a_beats1 === UInt(0)
    val req_done = out.a.fire() && a_last

    val d = out.d.bits
    val d_beats1 = edge.numBeats1(d)
    val d_counter = RegInit(UInt(0, width = maxLgBeats))
    val d_counter1 = d_counter - UInt(1)
    val d_first = d_counter === UInt(0)
    val d_last  = d_counter === UInt(1) || d_beats1 === UInt(0)
    val resp_done = out.d.fire() && d_last

    // Source ID generation
    val idMap = Module(new IDMapGenerator(inFlight))
    val alloc = Queue.irrevocable(idMap.io.alloc, 1, pipe = true)
    val src = alloc.bits
    alloc.ready := req_done
    idMap.io.free.valid := resp_done
    idMap.io.free.bits := out.d.bits.source

    // Increment random number generation for the following subfields
    val inc = Wire(Bool())
    val inc_beat = Wire(Bool())
    val arth_op   = NoiseMaker(3, inc)
    val log_op    = NoiseMaker(2, inc) 
    val amo_size  = UInt(2) + NoiseMaker(1, inc) // word or dword
    val size      = NoiseMaker(sizeBits, inc)
    val addr      = NoiseMaker(addressBits, inc) & ~UIntToOH1(size, addressBits)
    val mask      = NoiseMaker(beatBytes, inc_beat) & edge.mask(addr, size)
    val data      = NoiseMaker(dataBits, inc_beat)

    val (glegal,  gbits)  = edge.Get(src, addr, size)
    val (pflegal, pfbits) = if(edge.manager.anySupportPutFull) { 
                              edge.Put(src, addr, size, data)
                            } else { (glegal, gbits) }
    val (pplegal, ppbits) = if(edge.manager.anySupportPutPartial) {
                              edge.Put(src, addr, size, data, mask)
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

    out.a.valid := legal && alloc.valid && num_reqs =/= UInt(0)
    out.a.bits  := bits
    out.b.ready := Bool(true)
    out.c.valid := Bool(false)
    out.d.ready := Bool(true)
    out.e.valid := Bool(false)

    inc := !legal || req_done
    inc_beat := !legal || out.a.fire()

    when (out.a.fire()) {
      a_counter := Mux(a_first, a_beats1, a_counter1)
      when(a_last) { num_reqs := num_reqs - UInt(1) }
    }

    when (out.d.fire()) {
      d_counter := Mux(d_first, d_beats1, d_counter1)
      when(d_last) { num_resps := num_resps - UInt(1) }
    }
  }
}

class TLFuzzRAM extends LazyModule
{
  val model = LazyModule(new TLRAMModel)
  val ram  = LazyModule(new TLRAM(AddressSet(0, 0x3ff)))
  val gpio = LazyModule(new RRTest1(0x400))
  val xbar = LazyModule(new TLXbar)
  val fuzz = LazyModule(new TLFuzzer(5000))

  model.node := fuzz.node
  xbar.node := TLWidthWidget(TLHintHandler(model.node), 16)
  ram.node := TLFragmenter(TLBuffer(xbar.node), 4, 256)
  gpio.node := TLFragmenter(TLBuffer(xbar.node), 4, 32)

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished
  }
}

class TLFuzzRAMTest extends UnitTest {
  val dut = Module(LazyModule(new TLFuzzRAM).module)
  io.finished := dut.io.finished
}
