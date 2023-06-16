// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.devices.tilelink.TLTestRAM
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{RRTest0, RRTest1}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest._

class AHBRRTest0(address: BigInt)(implicit p: Parameters)
  extends RRTest0(address)
  with HasAHBControlRegMap

class AHBRRTest1(address: BigInt)(implicit p: Parameters) 
  extends RRTest1(address, concurrency = 1, undefZero = false)
  with HasAHBControlRegMap

class AHBFuzzNative(aFlow: Boolean, txns: Int)(implicit p: Parameters) extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("AHBFuzzNative"))
  val arb   = LazyModule(new AHBArbiter)
  val xbar  = LazyModule(new AHBFanout)
  val ram   = LazyModule(new AHBRAM(AddressSet(0x0, 0xff)))
  val gpio  = LazyModule(new AHBRRTest0(0x100))

  xbar.node := arb.node := TLToAHB(aFlow) := TLDelayer(0.1) := model.node := fuzz.node
  ram.node  := xbar.node
  gpio.controlNode := xbar.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class AHBNativeTest(aFlow: Boolean, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new AHBFuzzNative(aFlow, txns)).module)
  io.finished := dut.io.finished
  dut.io.start := io.start
}

trait HasFuzzTarget {
  val fuzzAddr = AddressSet(0x0, 0xfff)
  val pattern = Seq(AddressSet(0x8,   ~0x808), // ie: 0x8-0xf, 0x18-0x1f, ... 0x7f8-0x7ff
                    AddressSet(0x900, ~0x900)) // ie: 0x900-0x9ff, 0xb00-0xbff, ... 0xf00-0xfff
}

class AHBFuzzMaster(aFlow: Boolean, txns: Int)(implicit p: Parameters) extends LazyModule with HasFuzzTarget
{
  val node  = AHBSlaveIdentityNode()
  val arb   = LazyModule(new AHBArbiter)
  val fuzz  = LazyModule(new TLFuzzer(txns, overrideAddress = Some(fuzzAddr)))
  val model = LazyModule(new TLRAMModel("AHBFuzzMaster", ignoreCorruptData=false))

  (node
     := arb.node
     := TLToAHB(aFlow)
     := TLDelayer(0.2)
     := TLBuffer(BufferParams.flow)
     := TLDelayer(0.2)
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

class AHBFuzzSlave()(implicit p: Parameters) extends SimpleLazyModule with HasFuzzTarget
{
  val node = AHBSlaveIdentityNode()
  val ram  = LazyModule(new TLTestRAM(fuzzAddr, trackCorruption=false))

  (ram.node
    := TLErrorEvaluator(pattern)
    := TLFragmenter(4, 16)
    := TLDelayer(0.2)
    := TLBuffer(BufferParams.flow)
    := TLDelayer(0.2)
    := AHBToTL()
    := node)
}

class AHBFuzzBridge(aFlow: Boolean, txns: Int)(implicit p: Parameters) extends LazyModule
{
  val master = LazyModule(new AHBFuzzMaster(aFlow, txns))
  val slave  = LazyModule(new AHBFuzzSlave)

  slave.node := master.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := master.module.io.finished
  }
}

class AHBBridgeTest(aFlow: Boolean, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new AHBFuzzBridge(aFlow, txns)).module)
  io.finished := dut.io.finished
  dut.io.start := io.start
}
