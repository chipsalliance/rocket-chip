// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.tilelink.TLTestRAM
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest._

class RRTest0(address: BigInt)(implicit p: Parameters) extends NAMESPACERegisterRouter(address, 0, 32, 0, 4)(
  new NAMESPACERegBundle((), _)    with RRTest0Bundle)(
  new NAMESPACERegModule((), _, _) with RRTest0Module)

class RRTest1(address: BigInt)(implicit p: Parameters) extends NAMESPACERegisterRouter(address, 0, 32, 1, 4, false)(
  new NAMESPACERegBundle((), _)    with RRTest1Bundle)(
  new NAMESPACERegModule((), _, _) with RRTest1Module)

class NAMESPACEFuzzNative(aFlow: Boolean, txns: Int)(implicit p: Parameters) extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("NAMESPACEFuzzNative"))
  val xbar  = LazyModule(new NAMESPACEFanout)
  val ram   = LazyModule(new NAMESPACERAM(AddressSet(0x0, 0xff)))
  val gpio  = LazyModule(new RRTest0(0x100))

  xbar.node := TLToNAMESPACE(aFlow) := TLDelayer(0.1) := model.node := fuzz.node
  ram.node  := xbar.node
  gpio.node := xbar.node

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class NAMESPACENativeTest(aFlow: Boolean, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new NAMESPACEFuzzNative(aFlow, txns)).module)
  io.finished := dut.io.finished
}

trait HasFuzzTarget {
  val fuzzAddr = AddressSet(0x0, 0xfff)
  val pattern = Seq(AddressSet(0x8,   ~0x808), // ie: 0x8-0xf, 0x18-0x1f, ... 0x7f8-0x7ff
                    AddressSet(0x900, ~0x900)) // ie: 0x900-0x9ff, 0xb00-0xbff, ... 0xf00-0xfff
}

class NAMESPACEFuzzMaster(aFlow: Boolean, txns: Int)(implicit p: Parameters) extends LazyModule with HasFuzzTarget
{
  val node  = NAMESPACEIdentityNode()
  val fuzz  = LazyModule(new TLFuzzer(txns, overrideAddress = Some(fuzzAddr)))
  val model = LazyModule(new TLRAMModel("NAMESPACEFuzzMaster", ignoreCorruptData=true))

  (node
     := TLToNAMESPACE(aFlow)
     := TLDelayer(0.2)
     := TLBuffer(BufferParams.flow)
     := TLDelayer(0.2)
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

class NAMESPACEFuzzSlave()(implicit p: Parameters) extends SimpleLazyModule with HasFuzzTarget
{
  val node = NAMESPACEIdentityNode()
  val ram  = LazyModule(new TLTestRAM(fuzzAddr, trackCorruption=false))

  (ram.node
    := TLErrorEvaluator(pattern)
    := TLFragmenter(4, 16)
    := TLDelayer(0.2)
    := TLBuffer(BufferParams.flow)
    := TLDelayer(0.2)
    := NAMESPACEToTL()
    := node)
}

class NAMESPACEFuzzBridge(aFlow: Boolean, txns: Int)(implicit p: Parameters) extends LazyModule
{
  val master = LazyModule(new NAMESPACEFuzzMaster(aFlow, txns))
  val slave  = LazyModule(new NAMESPACEFuzzSlave)

  slave.node := master.node

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := master.module.io.finished
  }
}

class NAMESPACEBridgeTest(aFlow: Boolean, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new NAMESPACEFuzzBridge(aFlow, txns)).module)
  io.finished := dut.io.finished
}
