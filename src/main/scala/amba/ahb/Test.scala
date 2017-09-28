// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.tilelink.TLTestRAM
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest._

class RRTest0(address: BigInt)(implicit p: Parameters) extends AHBRegisterRouter(address, 0, 32, 0, 4)(
  new AHBRegBundle((), _)    with RRTest0Bundle)(
  new AHBRegModule((), _, _) with RRTest0Module)

class RRTest1(address: BigInt)(implicit p: Parameters) extends AHBRegisterRouter(address, 0, 32, 1, 4, false)(
  new AHBRegBundle((), _)    with RRTest1Bundle)(
  new AHBRegModule((), _, _) with RRTest1Module)

class AHBFuzzNative(aFlow: Boolean, txns: Int)(implicit p: Parameters) extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("AHBFuzzNative"))
  val xbar  = LazyModule(new AHBFanout)
  val ram   = LazyModule(new AHBRAM(AddressSet(0x0, 0xff)))
  val gpio  = LazyModule(new RRTest0(0x100))

  model.node := fuzz.node
  xbar.node := TLToAHB(aFlow)(TLDelayer(0.1)(model.node))
  ram.node  := xbar.node
  gpio.node := xbar.node

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class AHBNativeTest(aFlow: Boolean, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new AHBFuzzNative(aFlow, txns)).module)
  io.finished := dut.io.finished
}

class AHBFuzzMaster(aFlow: Boolean, txns: Int)(implicit p: Parameters) extends LazyModule
{
  val node  = AHBIdentityNode()
  val fuzz  = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("AHBFuzzMaster"))

  model.node := fuzz.node
  node :=
    TLToAHB(aFlow)(
    TLDelayer(0.2)(
    TLBuffer(BufferParams.flow)(
    TLDelayer(0.2)(
    model.node))))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val finished = Bool(OUTPUT)
    })

    io.finished := fuzz.module.io.finished
  }
}

class AHBFuzzSlave()(implicit p: Parameters) extends LazyModule
{
  val node = AHBIdentityNode()
  val ram  = LazyModule(new TLTestRAM(AddressSet(0x0, 0xfff)))

  ram.node :=
    TLFragmenter(4, 16)(
    TLDelayer(0.2)(
    TLBuffer(BufferParams.flow)(
    TLDelayer(0.2)(
    AHBToTL()(
    node)))))

  lazy val module = new LazyModuleImp(this) { }
}

class AHBFuzzBridge(aFlow: Boolean, txns: Int)(implicit p: Parameters) extends LazyModule
{
  val master = LazyModule(new AHBFuzzMaster(aFlow, txns))
  val slave  = LazyModule(new AHBFuzzSlave)

  slave.node := master.node

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := master.module.io.finished
  }
}

class AHBBridgeTest(aFlow: Boolean, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new AHBFuzzBridge(aFlow, txns)).module)
  io.finished := dut.io.finished
}
