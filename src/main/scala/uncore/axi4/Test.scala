// See LICENSE.SiFive for license details.

package uncore.axi4

import Chisel._
import config._
import diplomacy._
import uncore.tilelink2._
import unittest._

class RRTest0(address: BigInt)(implicit p: Parameters) extends AXI4RegisterRouter(address, 0, 32, 0, 4)(
  new AXI4RegBundle((), _)    with RRTest0Bundle)(
  new AXI4RegModule((), _, _) with RRTest0Module)

class RRTest1(address: BigInt)(implicit p: Parameters) extends AXI4RegisterRouter(address, 0, 32, 6, 4, false)(
  new AXI4RegBundle((), _)    with RRTest1Bundle)(
  new AXI4RegModule((), _, _) with RRTest1Module)

class AXI4LiteFuzzRAM()(implicit p: Parameters) extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(5000))
  val model = LazyModule(new TLRAMModel("AXI4LiteFuzzRAM"))
  val xbar  = LazyModule(new TLXbar)
  val gpio  = LazyModule(new RRTest1(0x400))
  val ram   = LazyModule(new AXI4RAM(AddressSet(0x0, 0x3ff)))

  model.node := fuzz.node
  xbar.node  := TLDelayer(0.1)(TLBuffer(BufferParams.flow)(TLDelayer(0.2)(model.node)))
  ram.node   := AXI4Fragmenter()(TLToAXI4(0, true )(xbar.node))
  gpio.node  := AXI4Fragmenter()(TLToAXI4(0, false)(xbar.node))

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished
  }
}

class AXI4LiteFuzzRAMTest()(implicit p: Parameters) extends UnitTest(500000) {
  val dut = Module(LazyModule(new AXI4LiteFuzzRAM).module)
  io.finished := dut.io.finished
}

class AXI4FullFuzzRAM()(implicit p: Parameters) extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(5000))
  val model = LazyModule(new TLRAMModel("AXI4FullFuzzRAM"))
  val xbar  = LazyModule(new TLXbar)
  val gpio  = LazyModule(new RRTest0(0x400))
  val ram   = LazyModule(new AXI4RAM(AddressSet(0x0, 0x3ff)))

  model.node := fuzz.node
  xbar.node  := TLDelayer(0.1)(TLBuffer(BufferParams.flow)(TLDelayer(0.2)(model.node)))
  ram.node   := AXI4Fragmenter()(TLToAXI4(4,false)(xbar.node))
  gpio.node  := AXI4Fragmenter()(TLToAXI4(4,true )(xbar.node))

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished
  }
}

class AXI4FullFuzzRAMTest(implicit p: Parameters) extends UnitTest(500000) {
  val dut = Module(LazyModule(new AXI4FullFuzzRAM).module)
  io.finished := dut.io.finished
}

class AXI4FuzzMaster()(implicit p: Parameters) extends LazyModule
{
  val node  = AXI4OutputNode()
  val fuzz  = LazyModule(new TLFuzzer(5000))
  val model = LazyModule(new TLRAMModel("AXI4FuzzMaster"))

  model.node := fuzz.node
  node :=
    TLToAXI4(4)(
    TLDelayer(0.1)(
    TLBuffer(BufferParams.flow)(
    TLDelayer(0.1)(
    model.node))))

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val out = node.bundleOut
      val finished = Bool(OUTPUT)
    }

    io.finished := fuzz.module.io.finished
  }
}

class AXI4FuzzSlave()(implicit p: Parameters) extends LazyModule
{
  val node = AXI4InputNode()
  val ram  = LazyModule(new TLTestRAM(AddressSet(0x0, 0xfff)))

  ram.node :=
    TLFragmenter(4, 16)(
    TLDelayer(0.1)(
    TLBuffer(BufferParams.flow)(
    TLDelayer(0.1)(
    AXI4ToTL()(
    AXI4UserYanker(4)(
    AXI4Fragmenter()(
    AXI4IdIndexer(4)(
    node))))))))

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
    }
  }
}

class AXI4FuzzBridge()(implicit p: Parameters) extends LazyModule
{
  val master = LazyModule(new AXI4FuzzMaster)
  val slave  = LazyModule(new AXI4FuzzSlave)

  slave.node := master.node

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := master.module.io.finished
  }
}

class AXI4BridgeTest()(implicit p: Parameters) extends UnitTest(500000) {
  val dut = Module(LazyModule(new AXI4FuzzBridge).module)
  io.finished := dut.io.finished
}
