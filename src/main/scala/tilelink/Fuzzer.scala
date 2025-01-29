// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.diplomacy.{AddressSet, IdRange}
import freechips.rocketchip.util.{leftOR, UIntToOH1}

import freechips.rocketchip.util.DataToAugmentedData

class IDMapGenerator(numIds: Int) extends Module {
  require (numIds > 0)

  val w = log2Up(numIds)
  val io = IO(new Bundle {
    val free = Flipped(Decoupled(UInt(w.W)))
    val alloc = Decoupled(UInt(w.W))
  })

  io.free.ready := true.B

  // True indicates that the id is available
  val bitmap = RegInit(((BigInt(1) << numIds) -  1).U(numIds.W))

  val select = ~(leftOR(bitmap) << 1) & bitmap
  io.alloc.bits := OHToUInt(select)
  io.alloc.valid := bitmap.orR

  val clr = WireDefault(0.U(numIds.W))
  when (io.alloc.fire) { clr := UIntToOH(io.alloc.bits) }

  val set = WireDefault(0.U(numIds.W))
  when (io.free.fire) { set := UIntToOH(io.free.bits) }

  bitmap := (bitmap & ~clr) | set
  assert (!io.free.valid || !(bitmap & ~clr)(io.free.bits)) // No double freeing
}

object LFSR64
{ 
  def apply(increment: Bool = true.B): UInt =
  { 
    val wide = 64
    val lfsr = Reg(UInt(wide.W)) // random initial value based on simulation seed
    val xor = lfsr(0) ^ lfsr(1) ^ lfsr(3) ^ lfsr(4)
    when (increment) {
      lfsr := Mux(lfsr === 0.U, 1.U, Cat(xor, lfsr(wide-1,1)))
    }
    lfsr
  }
}

trait HasNoiseMakerIO
{
  val io = IO(new Bundle {
    val inc = Input(Bool())
    val random = Output(UInt())
  })
}

class LFSRNoiseMaker(wide: Int) extends Module with HasNoiseMakerIO
{
  val lfsrs = Seq.fill((wide+63)/64) { LFSR64(io.inc) }
  io.random := Cat(lfsrs)(wide-1,0)
}

object LFSRNoiseMaker {
  def apply(wide: Int, increment: Bool = true.B): UInt = {
    val nm = Module(new LFSRNoiseMaker(wide))
    nm.io.inc := increment
    nm.io.random
  }
}

/** TLFuzzer drives test traffic over TL2 links. It generates a sequence of randomized
  * requests, and issues legal ones into the DUT. TODO: Currently the fuzzer only generates
  * memory operations, not permissions transfers.
  * @param nOperations is the total number of operations that the fuzzer must complete for the test to pass (0 => run forever)
  * @param inFlight is the number of operations that can be in-flight to the DUT concurrently
  * @param noiseMaker is a function that supplies a random UInt of a given width every time inc is true
  */
class TLFuzzer(
  nOperations: Int,
  inFlight: Int = 32,
  noiseMaker: (Int, Bool, Int) => UInt = {
    (wide: Int, increment: Bool, abs_values: Int) =>
    LFSRNoiseMaker(wide=wide, increment=increment)
  },
  noModify: Boolean = false,
  overrideAddress: Option[AddressSet] = None,
  nOrdered: Option[Int] = None)(implicit p: Parameters) extends LazyModule
{

  val clientParams = if (nOrdered.isDefined) {
    val n = nOrdered.get
    require(n > 0, s"nOrdered must be > 0, not $n")
    require((inFlight % n) == 0, s"inFlight (${inFlight}) must be evenly divisible by nOrdered (${nOrdered}).")
    Seq.tabulate(n) {i =>
      TLClientParameters.v1(name =s"OrderedFuzzer$i",
        sourceId = IdRange(i * (inFlight/n),  (i + 1)*(inFlight/n)),
        requestFifo = true)
    }
  } else {
    Seq(TLClientParameters.v1(
      name = "Fuzzer",
      sourceId = IdRange(0,inFlight)
    ))
  }

  val node = TLClientNode(Seq(TLClientPortParameters.v1(clientParams)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val finished = Output(Bool())
    })

    val (out, edge) = node.out(0)

    // Extract useful parameters from the TL edge
    val maxTransfer  = edge.manager.maxTransfer
    val beatBytes    = edge.manager.beatBytes
    val maxLgBeats   = log2Up(maxTransfer/beatBytes)
    val addressBits  = log2Up(overrideAddress.map(_.max).getOrElse(edge.manager.maxAddress))
    val sizeBits     = edge.bundle.sizeBits
    val dataBits     = edge.bundle.dataBits

    // Progress through operations
    val num_reqs = RegInit(nOperations.U(log2Up(nOperations+1).W))
    val num_resps = RegInit(nOperations.U(log2Up(nOperations+1).W))
    if (nOperations>0) {
      io.finished := num_resps === 0.U
    } else {
      io.finished := false.B
    }

    // Progress within each operation
    val a = out.a.bits
    val (a_first, a_last, req_done) = edge.firstlast(out.a)

    val d = out.d.bits
    val (d_first, d_last, resp_done) = edge.firstlast(out.d)

    // Source ID generation
    val idMap = Module(new IDMapGenerator(inFlight))
    val src = idMap.io.alloc.bits holdUnless a_first
    // Increment random number generation for the following subfields
    val inc = Wire(Bool())
    val inc_beat = Wire(Bool())
    val arth_op_3 = noiseMaker(3, inc, 0)
    val arth_op   = Mux(arth_op_3 > 4.U, 4.U, arth_op_3)
    val log_op    = noiseMaker(2, inc, 0)
    val amo_size  = 2.U + noiseMaker(1, inc, 0) // word or dword
    val size      = noiseMaker(sizeBits, inc, 0)
    val rawAddr   = noiseMaker(addressBits, inc, 2)
    val addr      = overrideAddress.map(_.legalize(rawAddr)).getOrElse(rawAddr) & ~UIntToOH1(size, addressBits)
    val mask      = noiseMaker(beatBytes, inc_beat, 2) & edge.mask(addr, size)
    val data      = noiseMaker(dataBits, inc_beat, 2)

    // Actually generate specific TL messages when it is legal to do so
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
                              edge.Hint(src, addr, size, 0.U)
                            } else { (glegal, gbits) }

    val legal_dest = edge.manager.containsSafe(addr)

    // Pick a specific message to try to send
    val a_type_sel  = noiseMaker(3, inc, 0)

    val legal = legal_dest && MuxLookup(a_type_sel, glegal)(Seq(
      "b000".U -> glegal,
      "b001".U -> (pflegal && !noModify.B),
      "b010".U -> (pplegal && !noModify.B),
      "b011".U -> (alegal && !noModify.B),
      "b100".U -> (llegal && !noModify.B),
      "b101".U -> hlegal))

    val bits = MuxLookup(a_type_sel, gbits)(Seq(
      "b000".U -> gbits,
      "b001".U -> pfbits,
      "b010".U -> ppbits,
      "b011".U -> abits,
      "b100".U -> lbits,
      "b101".U -> hbits))

    // Wire up Fuzzer flow control
    val a_gen = if (nOperations>0) num_reqs =/= 0.U else true.B
    out.a.valid := !reset.asBool && a_gen && legal && (!a_first || idMap.io.alloc.valid)
    idMap.io.alloc.ready := a_gen && legal && a_first && out.a.ready
    idMap.io.free.valid := d_first && out.d.fire
    idMap.io.free.bits := out.d.bits.source

    out.a.bits  := bits
    out.b.ready := true.B
    out.c.valid := false.B
    out.d.ready := true.B
    out.e.valid := false.B

    // Increment the various progress-tracking states
    inc := !legal || req_done
    inc_beat := !legal || out.a.fire

    if (nOperations>0) {
      when (out.a.fire && a_last) {
        num_reqs := num_reqs - 1.U
      }

      when (out.d.fire && d_last) {
        num_resps := num_resps - 1.U
      }
    }
  }
}

object TLFuzzer
{
  def apply(
    nOperations: Int,
    inFlight: Int = 32,
    noiseMaker: (Int, Bool, Int) => UInt = {
      (wide: Int, increment: Bool, abs_values: Int) =>
      LFSRNoiseMaker(wide=wide, increment=increment)
    },
    noModify: Boolean = false,
    overrideAddress: Option[AddressSet] = None,
    nOrdered: Option[Int] = None)(implicit p: Parameters): TLOutwardNode =
  {
    val fuzzer = LazyModule(new TLFuzzer(nOperations, inFlight, noiseMaker, noModify, overrideAddress, nOrdered))
    fuzzer.node
  }
}

/** Synthesizable integration test */
import freechips.rocketchip.unittest._

class TLFuzzRAM(txns: Int)(implicit p: Parameters) extends LazyModule
{
  val model = LazyModule(new TLRAMModel("TLFuzzRAM"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x800, 0x7ff)))
  val ram2 = LazyModule(new TLRAM(AddressSet(0, 0x3ff), beatBytes = 16))
  val gpio = LazyModule(new TLRRTest1(0x400))
  val xbar = LazyModule(new TLXbar)
  val xbar2= LazyModule(new TLXbar)
  val fuzz = LazyModule(new TLFuzzer(txns))

  xbar2.node := TLAtomicAutomata() := model.node := fuzz.node
  ram2.node := TLFragmenter(16, 256) := xbar2.node
  xbar.node := TLWidthWidget(16) := TLHintHandler() := xbar2.node
  ram.node := TLFragmenter(4, 256) := TLBuffer() := xbar.node
  gpio.node := TLFragmenter(4, 32) := TLBuffer() := xbar.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLFuzzRAMTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new TLFuzzRAM(txns)).module)
  io.finished := dut.io.finished
  dut.io.start := io.start
}
