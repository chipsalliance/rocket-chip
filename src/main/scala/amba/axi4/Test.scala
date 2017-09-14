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

  model.node := fuzz.node
  xbar.node  := TLDelayer(0.1)(TLBuffer(BufferParams.flow)(TLDelayer(0.2)(model.node)))
  ram.node   := AXI4UserYanker()(AXI4IdIndexer(0)(TLToAXI4(4, true )(TLFragmenter(4, 16)(xbar.node))))
  gpio.node  := AXI4UserYanker()(AXI4IdIndexer(0)(TLToAXI4(4, false)(TLFragmenter(4, 16)(xbar.node))))

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

  model.node := fuzz.node
  xbar.node  := TLDelayer(0.1)(TLBuffer(BufferParams.flow)(TLDelayer(0.2)(model.node)))
  ram.node   := AXI4Fragmenter()(AXI4Deinterleaver(16)(TLToAXI4(4,false)(xbar.node)))
  gpio.node  := AXI4Fragmenter()(AXI4Deinterleaver(16)(TLToAXI4(4,true )(xbar.node)))

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
}

class AXI4FuzzMaster(txns: Int)(implicit p: Parameters) extends LazyModule with HasFuzzTarget
{
  val node  = AXI4IdentityNode()
  val fuzz  = LazyModule(new TLFuzzer(txns, overrideAddress = Some(fuzzAddr)))
  val model = LazyModule(new TLRAMModel("AXI4FuzzMaster"))

  model.node := fuzz.node
  node :=
    AXI4UserYanker()(
    AXI4Deinterleaver(64)(
    TLToAXI4(4)(
    TLDelayer(0.1)(
    TLBuffer(BufferParams.flow)(
    TLDelayer(0.1)(
    model.node))))))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val finished = Bool(OUTPUT)
    })

    io.finished := fuzz.module.io.finished
  }
}

class AXI4FuzzSlave()(implicit p: Parameters) extends LazyModule with HasFuzzTarget
{
  val node = AXI4IdentityNode()
  val xbar = LazyModule(new TLXbar)
  val ram  = LazyModule(new TLRAM(fuzzAddr))
  val error= LazyModule(new TLError(ErrorParams(Seq(AddressSet(0x1800, 0xff)), maxTransfer = 256)))

  ram.node   := TLFragmenter(4, 16)(xbar.node)
  error.node := xbar.node

  xbar.node :=
    TLFIFOFixer()(
    TLDelayer(0.1)(
    TLBuffer(BufferParams.flow)(
    TLDelayer(0.1)(
    AXI4ToTL()(
    AXI4UserYanker(Some(4))(
    AXI4Fragmenter()(
    AXI4IdIndexer(2)(
    node))))))))

  lazy val module = new LazyModuleImp(this) { }
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
