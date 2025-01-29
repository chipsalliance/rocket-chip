// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import chisel3._

import org.chipsalliance.cde.config.Parameters

import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp, SimpleLazyModule}

import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy.{AddressSet, BufferParams}
import freechips.rocketchip.regmapper.{RRTest0, RRTest1}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest._

class AXI4RRTest0(address: BigInt)(implicit p: Parameters)
  extends RRTest0(address)
  with HasAXI4ControlRegMap

class AXI4RRTest1(address: BigInt)(implicit p: Parameters)
  extends RRTest1(address, concurrency = 6, undefZero = false)
  with HasAXI4ControlRegMap

class AXI4LiteFuzzRAM(txns: Int)(implicit p: Parameters) extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("AXI4LiteFuzzRAM"))
  val xbar  = LazyModule(new TLXbar)
  val gpio  = LazyModule(new AXI4RRTest1(0x400))
  val ram   = LazyModule(new AXI4RAM(AddressSet(0x0, 0x3ff)))

  xbar.node := TLDelayer(0.1) := TLBuffer(BufferParams.flow) := TLDelayer(0.2) := model.node := fuzz.node
  ram.node  := AXI4UserYanker() := AXI4IdIndexer(0) := TLToAXI4(true ) := TLFragmenter(4, 16, holdFirstDeny=true) := xbar.node
  gpio.node := AXI4UserYanker() := AXI4IdIndexer(0) := TLToAXI4(false) := TLFragmenter(4, 16, holdFirstDeny=true) := xbar.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class AXI4LiteFuzzRAMTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new AXI4LiteFuzzRAM(txns)).module)
  io.finished := dut.io.finished
  dut.io.start := io.start
}

class AXI4LiteUserBitsFuzzRAM(txns: Int)(implicit p: Parameters) extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("AXI4LiteFuzzRAM"))
  val xbar  = LazyModule(new TLXbar)
  val gpio  = LazyModule(new AXI4RRTest1(0x400))
  val ram   = LazyModule(new AXI4RAM(AddressSet(0x0, 0x3ff)))

  xbar.node := TLDelayer(0.1) := TLBuffer(BufferParams.flow) := TLDelayer(0.2) := model.node := fuzz.node
  ram.node  := AXI4UserYanker() := AXI4IdIndexer(0) := TLToAXI4(true ) := TLFragmenter(4, 16, holdFirstDeny=true) := xbar.node
  gpio.node := AXI4UserYanker() := AXI4IdIndexer(0) := TLToAXI4(false) := TLFragmenter(4, 16, holdFirstDeny=true) := xbar.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class AXI4LiteUserBitsFuzzRAMTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new AXI4LiteUserBitsFuzzRAM(txns)).module)
  io.finished := dut.io.finished
  dut.io.start := io.start
}

class AXI4FullFuzzRAM(txns: Int)(implicit p: Parameters) extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("AXI4FullFuzzRAM"))
  val xbar  = LazyModule(new TLXbar)
  val gpio  = LazyModule(new AXI4RRTest0(0x400))
  val ram   = LazyModule(new AXI4RAM(AddressSet(0x0, 0x3ff)))

  xbar.node := TLDelayer(0.1) := TLBuffer(BufferParams.flow) := TLDelayer(0.2) := model.node := fuzz.node
  ram.node  := AXI4Fragmenter() := AXI4Deinterleaver(16) := TLToAXI4(false) := xbar.node
  gpio.node := AXI4Fragmenter() := AXI4Deinterleaver(16) := TLToAXI4(true ) := xbar.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class AXI4FullFuzzRAMTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new AXI4FullFuzzRAM(txns)).module)
  io.finished := dut.io.finished
  dut.io.start := io.start
}

trait HasFuzzTarget {
  val fuzzAddr = AddressSet(0x0, 0xfff)
  val pattern = Seq(AddressSet(0x8,   ~0x808), // ie: 0x8-0xf, 0x18-0x1f, ... 0x7f8-0x7ff
                    AddressSet(0x900, ~0x900)) // ie: 0x900-0x9ff, 0xb00-0xbff, ... 0xf00-0xfff
}

class AXI4FuzzManager(txns: Int)(implicit p: Parameters) extends LazyModule with HasFuzzTarget
{
  val node  = AXI4IdentityNode()
  val fuzz  = LazyModule(new TLFuzzer(txns, overrideAddress = Some(fuzzAddr)))
  val model = LazyModule(new TLRAMModel("AXI4FuzzManager"))

  (node
    := AXI4UserYanker()
    := AXI4Deinterleaver(64)
    := TLToAXI4()
    := TLDelayer(0.1)
    := TLBuffer(BufferParams.flow)
    := TLDelayer(0.1)
    := TLErrorEvaluator(pattern, testOn=true, testOff=true)
    := model.node
    := fuzz.node)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val finished = Output(Bool())
    })

    io.finished := fuzz.module.io.finished
  }
}

class AXI4FuzzSubordinate()(implicit p: Parameters) extends SimpleLazyModule with HasFuzzTarget
{
  val node = AXI4IdentityNode()
  val xbar = LazyModule(new TLXbar)
  val ram  = LazyModule(new TLRAM(fuzzAddr))
  val error= LazyModule(new TLError(DevNullParams(Seq(AddressSet(0x1800, 0xff)), maxAtomic = 8, maxTransfer = 256)))

  ram.node   := TLErrorEvaluator(pattern) := TLFragmenter(4, 16) := xbar.node
  error.node := xbar.node

  (xbar.node
    := TLFIFOFixer()
    := TLDelayer(0.1)
    := TLBuffer(BufferParams.flow)
    := TLDelayer(0.1)
    := AXI4ToTL()
    := AXI4UserYanker(Some(4))
    := AXI4Fragmenter()
    := AXI4IdIndexer(2)
    := node)
}

class AXI4FuzzBridge(txns: Int)(implicit p: Parameters) extends LazyModule
{
  val manager = LazyModule(new AXI4FuzzManager(txns))
  val subordinate  = LazyModule(new AXI4FuzzSubordinate)

  subordinate.node := manager.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := manager.module.io.finished
  }
}

class AXI4BridgeTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new AXI4FuzzBridge(txns)).module)
  io.finished := dut.io.finished
  dut.io.start := io.start
}
