// See LICENSE.SiFive for license details.

package uncore.ahb

import Chisel._
import config._
import diplomacy._
import uncore.tilelink2._
import unittest._

class RRTest0(address: BigInt)(implicit p: Parameters) extends AHBRegisterRouter(address, 0, 32, 0, 4)(
  new AHBRegBundle((), _)    with RRTest0Bundle)(
  new AHBRegModule((), _, _) with RRTest0Module)

class RRTest1(address: BigInt)(implicit p: Parameters) extends AHBRegisterRouter(address, 0, 32, 1, 4, false)(
  new AHBRegBundle((), _)    with RRTest1Bundle)(
  new AHBRegModule((), _, _) with RRTest1Module)

class AHBFuzzNative(aFlow: Boolean)(implicit p: Parameters) extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(5000))
  val model = LazyModule(new TLRAMModel("AHBFuzzNative"))
  var xbar  = LazyModule(new AHBFanout)
  val ram   = LazyModule(new AHBRAM(AddressSet(0x0, 0xff)))
  val gpio  = LazyModule(new RRTest0(0x100))

  model.node := fuzz.node
  xbar.node := TLToAHB(aFlow)(TLDelayer(0.1)(model.node))
  ram.node  := xbar.node
  gpio.node := xbar.node

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished
  }
}

class AHBNativeTest(aFlow: Boolean, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new AHBFuzzNative(aFlow)).module)
  io.finished := dut.io.finished
}

class AHBFuzzMaster(aFlow: Boolean)(implicit p: Parameters) extends LazyModule
{
  val node  = AHBOutputNode()
  val fuzz  = LazyModule(new TLFuzzer(5000))
  val model = LazyModule(new TLRAMModel("AHBFuzzMaster"))

  model.node := fuzz.node
  node :=
    TLToAHB(aFlow)(
    TLDelayer(0.2)(
    TLBuffer(BufferParams.flow)(
    TLDelayer(0.2)(
    model.node))))

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val out = node.bundleOut
      val finished = Bool(OUTPUT)
    }

    io.finished := fuzz.module.io.finished
  }
}

class AHBFuzzSlave()(implicit p: Parameters) extends LazyModule
{
  val node = AHBInputNode()
  val ram  = LazyModule(new TLTestRAM(AddressSet(0x0, 0xfff)))

  ram.node :=
    TLFragmenter(4, 16)(
    TLDelayer(0.2)(
    TLBuffer(BufferParams.flow)(
    TLDelayer(0.2)(
    AHBToTL()(
    node)))))

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
    }
  }
}

class AHBFuzzBridge(aFlow: Boolean)(implicit p: Parameters) extends LazyModule
{
  val master = LazyModule(new AHBFuzzMaster(aFlow))
  val slave  = LazyModule(new AHBFuzzSlave)

  slave.node := master.node

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := master.module.io.finished
  }
}

class AHBBridgeTest(aFlow: Boolean, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new AHBFuzzBridge(aFlow)).module)
  io.finished := dut.io.finished
}
