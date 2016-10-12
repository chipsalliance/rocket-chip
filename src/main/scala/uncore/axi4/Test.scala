// See LICENSE for license details.
package uncore.axi4

import Chisel._
import diplomacy._
import uncore.tilelink2._
import unittest._

class RRTest0(address: BigInt) extends AXI4RegisterRouter(address, 0, 32, 0, 4)(
  new AXI4RegBundle((), _)    with RRTest0Bundle)(
  new AXI4RegModule((), _, _) with RRTest0Module)

class RRTest1(address: BigInt) extends AXI4RegisterRouter(address, 0, 32, 6, 4, false)(
  new AXI4RegBundle((), _)    with RRTest1Bundle)(
  new AXI4RegModule((), _, _) with RRTest1Module)

class AXI4LiteFuzzRAM extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(5000))
  val model = LazyModule(new TLRAMModel("AXI4LiteFuzzRAM"))
  val xbar  = LazyModule(new TLXbar)
  val gpio  = LazyModule(new RRTest1(0x400))
  val ram   = LazyModule(new AXI4RAM(AddressSet(0x0, 0x3ff)))

  model.node := fuzz.node
  xbar.node  := model.node
  ram.node   := AXI4Fragmenter(lite=true)(TLToAXI4(0, true )(xbar.node))
  gpio.node  := AXI4Fragmenter(lite=true)(TLToAXI4(0, false)(xbar.node))

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished
  }
}

class AXI4LiteFuzzRAMTest extends UnitTest(500000) {
  val dut = Module(LazyModule(new AXI4LiteFuzzRAM).module)
  io.finished := dut.io.finished
}

class AXI4FullFuzzRAM extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(5000))
  val model = LazyModule(new TLRAMModel("AXI4FullFuzzRAM"))
  val xbar  = LazyModule(new TLXbar)
  val gpio  = LazyModule(new RRTest0(0x400))
  val ram   = LazyModule(new AXI4RAM(AddressSet(0x0, 0x3ff)))

  model.node := fuzz.node
  xbar.node  := model.node
  ram.node   := AXI4Fragmenter(lite=false)(TLToAXI4(4,false)(xbar.node))
  gpio.node  := AXI4Fragmenter(lite=false)(TLToAXI4(4,true )(xbar.node))

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished
  }
}

class AXI4FullFuzzRAMTest extends UnitTest(500000) {
  val dut = Module(LazyModule(new AXI4FullFuzzRAM).module)
  io.finished := dut.io.finished
}
