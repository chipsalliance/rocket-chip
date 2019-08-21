// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest._

class RRTest0(address: BigInt)(implicit p: Parameters) extends APBRegisterRouter(address, 0, 32, 0, 4)(
  new APBRegBundle((), _)    with RRTest0Bundle)(
  new APBRegModule((), _, _) with RRTest0Module)

class RRTest1(address: BigInt)(implicit p: Parameters) extends APBRegisterRouter(address, 0, 32, 1, 4, false)(
  new APBRegBundle((), _)    with RRTest1Bundle)(
  new APBRegModule((), _, _) with RRTest1Module)

class APBFuzzBridge(aFlow: Boolean, txns: Int)(implicit p: Parameters) extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("APBFuzzMaster"))
  val xbar  = LazyModule(new APBFanout)
  val ram   = LazyModule(new APBRAM(AddressSet(0x0, 0xff), fuzzReady = true, fuzzError = true))
  val gpio  = LazyModule(new RRTest0(0x100))

  ram.node  := xbar.node
  gpio.node := xbar.node
  (xbar.node
    := TLToAPB(aFlow)
    := TLDelayer(0.2)
    := TLBuffer(BufferParams.flow)
    := TLDelayer(0.2)
    := TLFragmenter(4, 8)
    := model.node
    := fuzz.node)

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class APBBridgeTest(aFlow: Boolean, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new APBFuzzBridge(aFlow, txns)).module)
  io.finished := dut.io.finished
}
