// See LICENSE.SiFive for license details.

package uncore.apb

import Chisel._
import config._
import diplomacy._
import uncore.tilelink2._
import unittest._

class RRTest0(address: BigInt)(implicit p: Parameters) extends APBRegisterRouter(address, 0, 32, 0, 4)(
  new APBRegBundle((), _)    with RRTest0Bundle)(
  new APBRegModule((), _, _) with RRTest0Module)

class RRTest1(address: BigInt)(implicit p: Parameters) extends APBRegisterRouter(address, 0, 32, 1, 4, false)(
  new APBRegBundle((), _)    with RRTest1Bundle)(
  new APBRegModule((), _, _) with RRTest1Module)

class APBFuzzBridge()(implicit p: Parameters) extends LazyModule
{
  val fuzz  = LazyModule(new TLFuzzer(5000))
  val model = LazyModule(new TLRAMModel("APBFuzzMaster"))
  var xbar  = LazyModule(new APBFanout)
  val ram   = LazyModule(new APBRAM(AddressSet(0x0, 0xff)))
  val gpio  = LazyModule(new RRTest0(0x100))

  model.node := fuzz.node
  xbar.node := TLToAPB()(TLDelayer(0.1)(model.node))
  ram.node  := xbar.node
  gpio.node := xbar.node

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished
  }
}

class APBBridgeTest()(implicit p: Parameters) extends UnitTest(500000) {
  val dut = Module(LazyModule(new APBFuzzBridge).module)
  io.finished := dut.io.finished
}
