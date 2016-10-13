// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import diplomacy._
import util._

class TLAsyncCrossingSource(sync: Int = 3) extends LazyModule
{
  val node = TLAsyncSourceNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      val sink_reset_n = out.a.sink_reset_n
      val bce = edgeIn.manager.anySupportAcquire && edgeIn.client.anySupportProbe
      val depth = edgeOut.manager.depth

      out.a <> ToAsyncBundle(in.a, depth, sync)
      in.d <> FromAsyncBundle(out.d, sync)

      assert (!in.a.valid || sink_reset_n, "A channel request sent to a missing manager")

      if (bce) {
        in.b <> FromAsyncBundle(out.b, sync)
        out.c <> ToAsyncBundle(in.c, depth, sync)
        out.e <> ToAsyncBundle(in.e, depth, sync)

        assert (!in.c.valid || sink_reset_n, "C channel response sent to a missing manager")
        assert (!in.e.valid || sink_reset_n, "E channel response sent to a missing manager")
      } else {
        in.b.valid := Bool(false)
        in.c.ready := Bool(true)
        in.e.ready := Bool(true)
        out.b.ridx := UInt(0)
        out.c.widx := UInt(0)
        out.e.widx := UInt(0)
      }
    }
  }
}

class TLAsyncCrossingSink(depth: Int = 8, sync: Int = 3) extends LazyModule
{
  val node = TLAsyncSinkNode(depth)

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      val source_reset_n = in.a.source_reset_n
      val bce = edgeOut.manager.anySupportAcquire && edgeOut.client.anySupportProbe

      out.a <> FromAsyncBundle(in.a, sync)
      in.d <> ToAsyncBundle(out.d, depth, sync)

      assert (!out.d.valid || source_reset_n, "D channel respose sent to missing client")

      if (bce) {
        in.b <> ToAsyncBundle(out.b, depth, sync)
        out.c <> FromAsyncBundle(in.c, sync)
        out.e <> FromAsyncBundle(in.e, sync)

        assert (!out.b.valid || source_reset_n, "B channel request sent to missing client")
      } else {
        in.b.widx := UInt(0)
        in.c.ridx := UInt(0)
        in.e.ridx := UInt(0)
        out.b.ready := Bool(true)
        out.c.valid := Bool(false)
        out.e.valid := Bool(false)
      }
    }
  }
}

object TLAsyncCrossingSource
{
  // applied to the TL source node; y.node := TLAsyncCrossingSource()(x.node)
  def apply(sync: Int = 3)(x: TLOutwardNode)(implicit sourceInfo: SourceInfo): TLAsyncOutwardNode = {
    val source = LazyModule(new TLAsyncCrossingSource(sync))
    source.node := x
    source.node
  }
}

object TLAsyncCrossingSink
{
  // applied to the TL source node; y.node := TLAsyncCrossingSink()(x.node)
  def apply(depth: Int = 8, sync: Int = 3)(x: TLAsyncOutwardNode)(implicit sourceInfo: SourceInfo): TLOutwardNode = {
    val sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
    sink.node := x
    sink.node
  }
}

class TLAsyncCrossing(depth: Int = 8, sync: Int = 3) extends LazyModule
{
  val nodeIn = TLInputNode()
  val nodeOut = TLOutputNode()
  val node = NodeHandle(nodeIn, nodeOut)

  val source = LazyModule(new TLAsyncCrossingSource(sync))
  val sink = LazyModule(new TLAsyncCrossingSink(depth, sync))

  val _    = (sink.node := source.node) // no monitor
  val in   = (source.node := nodeIn)
  val out  = (nodeOut := sink.node)

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in        = nodeIn.bundleIn
      val in_clock  = Clock(INPUT)
      val in_reset  = Bool(INPUT)
      val out       = nodeOut.bundleOut
      val out_clock = Clock(INPUT)
      val out_reset = Bool(INPUT)
    }

    source.module.clock := io.in_clock
    source.module.reset := io.in_reset
    in.foreach { lm =>
      lm.module.clock := io.in_clock
      lm.module.reset := io.in_reset
    }

    sink.module.clock := io.out_clock
    sink.module.reset := io.out_reset
    out.foreach { lm =>
      lm.module.clock := io.out_clock
      lm.module.reset := io.out_reset
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
