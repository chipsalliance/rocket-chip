// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import config._
import diplomacy._
import regmapper._

/** This is an example of how to create and test a simple
  * TL slave device that uses the regmapper library to
  * manage access to its registers.
  */

case class ExampleParams(num: Int, address: BigInt)

trait ExampleBundle
{
  val params: ExampleParams
  val gpio = UInt(width = params.num)
}

trait ExampleModule extends HasRegMap
{
  val params: ExampleParams
  val io: ExampleBundle
  val interrupts: Vec[Bool]

  val state = RegInit(UInt(0))
  val pending = RegInit(UInt(0xf, width = 4))

  io.gpio := state
  interrupts := pending.toBools

  regmap(
    0 -> Seq(
      RegField(params.num, state)),
    4 -> Seq(
      RegField.w1ToClear(4, pending, state)))
}

// Create a concrete TL2 version of the abstract Example slave
class TLExample(params: ExampleParams)(implicit p: Parameters) extends TLRegisterRouter(params.address, 4)(
  new TLRegBundle(params, _)    with ExampleBundle)(
  new TLRegModule(params, _, _) with ExampleModule)

////////

/** This is an example of how to create a blackboxed slave device
  * that implements TL2-UL.
  */

/** This wrapper negotiates parameters diplomatically with the external TileLink network */
class TLExampleBlackBoxedSlave(base: BigInt, size: Int, executable: Boolean = true, beatBytes: Int = 4)(implicit p: Parameters) extends LazyModule {
  val node = TLManagerNode(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = List(AddressSet(base, size-1)),
      regionType         = RegionType.UNCACHED,
      executable         = executable,
      supportsGet        = TransferSizes(1, beatBytes),
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes))),
    beatBytes = beatBytes))

  // We require the address range to include an entire beat (for the write mask)
  require (size > beatBytes)

  // The concrete module implementation simply instantiates the black box
  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
    }
    val in = io.in(0)
    val edge = node.edgesIn(0)

    // Create and wire the blackbox
    val bb = new TLULSlaveExample(size, edge)
    in.a <> bb.io.slave_a
    in.d <> bb.io.slave_d

    // Tie off unused channels
    in.b.valid := Bool(false)
    in.c.ready := Bool(true)
    in.e.ready := Bool(true)
  }
}

/** The actual paramterized blackbox; there must exist a verilog module
  * with an exactly matching name. */
import chisel3.core.IntParam
class TLULSlaveExample(size: Int, edge: TLEdgeIn) extends BlackBox(Map(
  "SIZEBYTES"   -> IntParam(size),
  "SIZEBITS"    -> IntParam(edge.bundle.sizeBits),
  "SOURCEBITS"  -> IntParam(edge.bundle.sourceBits),
  "ADDRESSBITS" -> IntParam(edge.bundle.addressBits),
  "MASKBITS"    -> IntParam(edge.bundle.dataBits/8),
  "DATABITS"    -> IntParam(edge.bundle.dataBits),
  "SINKBITS"    -> IntParam(edge.bundle.sinkBits),
  "ADDRLOBITS"  -> IntParam(edge.bundle.addrLoBits)
)) {
  val io = new Bundle {
    val slave_a = Decoupled(new TLBundleA(edge.bundle))
    val slave_d = Decoupled(new TLBundleD(edge.bundle))
  }
}


/** Synthesizeable unit test */
import unittest._

class TLExampleBlackBoxedSlaveFuzz(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(5000))
  val model = LazyModule(new TLRAMModel)
  val ram  = LazyModule(new TLExampleBlackBoxedSlave(0, 64, beatBytes = 4))

  model.node := fuzz.node
  ram.node := model.node

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished
  }
}

class TLExampleBlackBoxedSlaveFuzzTest(ramBeatBytes: Int)(implicit p: Parameters) extends UnitTest(timeout = 500000) {
  io.finished := Module(LazyModule(new TLExampleBlackBoxedSlaveFuzz).module).io.finished
}
