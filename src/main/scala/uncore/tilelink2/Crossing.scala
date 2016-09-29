// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import junctions._

class TLAsyncCrossing(depth: Int = 8, sync: Int = 3) extends LazyModule
{
  val node = TLIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in        = node.bundleIn
      val in_clock  = Clock(INPUT)
      val in_reset  = Bool(INPUT)
      val out       = node.bundleOut
      val out_clock = Clock(INPUT)
      val out_reset = Bool(INPUT)
    }

    // Transfer all TL2 bundles from/to the same domains
    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      out.a <> AsyncIrrevocableCrossing(io.in_clock, io.in_reset, in.a, io.out_clock, io.out_reset, depth, sync)
      in.d <> AsyncIrrevocableCrossing(io.out_clock, io.out_reset, out.d, io.in_clock, io.in_reset, depth, sync)

      if (edgeOut.manager.anySupportAcquire && edgeOut.client.anySupportProbe) {
        in.b <> AsyncIrrevocableCrossing(io.out_clock, io.out_reset, out.b, io.in_clock, io.in_reset, depth, sync)
        out.c <> AsyncIrrevocableCrossing(io.in_clock, io.in_reset, in.c, io.out_clock, io.out_reset, depth, sync)
        out.e <> AsyncIrrevocableCrossing(io.in_clock, io.in_reset, in.e, io.out_clock, io.out_reset, depth, sync)
      } else {
        in.b.valid := Bool(false)
        in.c.ready := Bool(true)
        in.e.ready := Bool(true)
        out.b.ready := Bool(true)
        out.c.valid := Bool(false)
        out.e.valid := Bool(false)
      }
    }
  }
}

/** Synthesizeable unit tests */
import unittest._

class TLRAMCrossing extends LazyModule {
  val model = LazyModule(new TLRAMModel)
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x3ff)))
  val fuzz = LazyModule(new TLFuzzer(5000))
  val cross = LazyModule(new TLAsyncCrossing)

  model.node := fuzz.node
  cross.node := TLFragmenter(4, 256)(model.node)
  val monitor = (ram.node := cross.node)

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished

    // Shove the RAM into another clock domain
    val clocks = Module(new util.Pow2ClockDivider(2))
    ram.module.clock := clocks.io.clock_out

    // ... and safely cross TL2 into it
    cross.module.io.in_clock := clock
    cross.module.io.in_reset := reset
    cross.module.io.out_clock := clocks.io.clock_out
    cross.module.io.out_reset := reset

    // Push the Monitor into the right clock domain
    monitor.foreach { m =>
      m.module.clock := clocks.io.clock_out
      m.module.reset := reset
    }
  }
}

class TLRAMCrossingTest extends UnitTest(timeout = 500000) {
  io.finished := Module(LazyModule(new TLRAMCrossing).module).io.finished
}
