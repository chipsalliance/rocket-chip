// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{RRTest0, RRTest1}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest._

class APBRRTest0(address: BigInt)(implicit p: Parameters) 
  extends RRTest0(address)
  with HasAPBControlRegMap

class APBRRTest1(address: BigInt)(implicit p: Parameters)
  extends RRTest1(address, concurrency = 1, undefZero = false)
  with HasAPBControlRegMap

class APBFuzzBridge(aFlow: Boolean, txns: Int)(implicit p: Parameters) extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("APBFuzzMaster"))
  val xbar  = LazyModule(new APBFanout)
  val ram   = LazyModule(new APBRAM(AddressSet(0x0, 0xff), fuzzReady = true, fuzzError = true))
  val gpio  = LazyModule(new APBRRTest0(0x100))

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

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class APBBridgeTest(aFlow: Boolean, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new APBFuzzBridge(aFlow, txns)).module)
  io.finished := dut.io.finished
  dut.io.start := io.start
}
