// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest._

class RRTest0(address: BigInt)(implicit p: Parameters) extends AXI4RegisterRouter(address, 0, 32, 0, 4)(
  new AXI4RegBundle((), _)    with RRTest0Bundle)(
  new AXI4RegModule((), _, _) with RRTest0Module)

class RRTest1(address: BigInt)(implicit p: Parameters) extends AXI4RegisterRouter(address, 0, 32, 6, 4, false)(
  new AXI4RegBundle((), _)    with RRTest1Bundle)(
  new AXI4RegModule((), _, _) with RRTest1Module)

class AXI4LiteFuzzRAM(txns: Int)(implicit p: Parameters) extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("AXI4LiteFuzzRAM"))
  val xbar  = LazyModule(new TLXbar)
  val gpio  = LazyModule(new RRTest1(0x400))
  val ram   = LazyModule(new AXI4RAM(AddressSet(0x0, 0x3ff)))

  xbar.node := TLDelayer(0.1) := TLBuffer(BufferParams.flow) := TLDelayer(0.2) := model.node := fuzz.node
  ram.node  := AXI4UserYanker() := AXI4IdIndexer(0) := TLToAXI4(true ) := TLFragmenter(4, 16, holdFirstDeny=true) := xbar.node
  gpio.node := AXI4UserYanker() := AXI4IdIndexer(0) := TLToAXI4(false) := TLFragmenter(4, 16, holdFirstDeny=true) := xbar.node

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class AXI4LiteFuzzRAMTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new AXI4LiteFuzzRAM(txns)).module)
  io.finished := dut.io.finished
}

class AXI4FullFuzzRAM(txns: Int)(implicit p: Parameters) extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("AXI4FullFuzzRAM"))
  val xbar  = LazyModule(new TLXbar)
  val gpio  = LazyModule(new RRTest0(0x400))
  val ram   = LazyModule(new AXI4RAM(AddressSet(0x0, 0x3ff)))

  xbar.node := TLDelayer(0.1) := TLBuffer(BufferParams.flow) := TLDelayer(0.2) := model.node := fuzz.node
  ram.node  := AXI4Fragmenter() := AXI4Deinterleaver(16) := TLToAXI4(false) := xbar.node
  gpio.node := AXI4Fragmenter() := AXI4Deinterleaver(16) := TLToAXI4(true ) := xbar.node

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class AXI4FullFuzzRAMTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new AXI4FullFuzzRAM(txns)).module)
  io.finished := dut.io.finished
}

trait HasFuzzTarget {
  val fuzzAddr = AddressSet(0x0, 0xfff)
  val pattern = Seq(AddressSet(0x8,   ~0x808), // ie: 0x8-0xf, 0x18-0x1f, ... 0x7f8-0x7ff
                    AddressSet(0x900, ~0x900)) // ie: 0x900-0x9ff, 0xb00-0xbff, ... 0xf00-0xfff
}

class AXI4FuzzMaster(txns: Int)(implicit p: Parameters) extends LazyModule with HasFuzzTarget
{
  val node  = AXI4IdentityNode()
  val fuzz  = LazyModule(new TLFuzzer(txns, overrideAddress = Some(fuzzAddr)))
  val model = LazyModule(new TLRAMModel("AXI4FuzzMaster"))

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

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val finished = Bool(OUTPUT)
    })

    io.finished := fuzz.module.io.finished
  }
}

class AXI4FuzzSlave()(implicit p: Parameters) extends SimpleLazyModule with HasFuzzTarget
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
  val master = LazyModule(new AXI4FuzzMaster(txns))
  val slave  = LazyModule(new AXI4FuzzSlave)

  slave.node := master.node

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := master.module.io.finished
  }
}

class AXI4BridgeTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new AXI4FuzzBridge(txns)).module)
  io.finished := dut.io.finished
}
