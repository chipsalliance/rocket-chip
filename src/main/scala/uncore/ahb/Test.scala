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

class AHBFuzzBridge()(implicit p: Parameters) extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(5000))
  val model = LazyModule(new TLRAMModel("AHBFuzzMaster"))
  var xbar  = LazyModule(new AHBFanout)
  val ram   = LazyModule(new AHBRAM(AddressSet(0x0, 0xff)))
  val gpio  = LazyModule(new RRTest0(0x100))

  model.node := fuzz.node
  xbar.node := TLToAHB()(TLDelayer(0.1)(model.node))
  ram.node  := xbar.node
  gpio.node := xbar.node

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished
  }
}

class AHBBridgeTest()(implicit p: Parameters) extends UnitTest(500000) {
  val dut = Module(LazyModule(new AHBFuzzBridge).module)
  io.finished := dut.io.finished
}
