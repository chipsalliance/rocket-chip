// See LICENSE.SiFive for license details.

// If you know two clocks are related with a N:1 or 1:N relationship, you
// can cross the clock domains with lower latency than an AsyncQueue.
// This clock crossing behaves almost identically to a TLBuffer(2):
//   - It adds one cycle latency to each clock domain.
//   - All outputs of TLRational are registers (bits, valid, and ready).
//   - It costs 3*bits registers as opposed to 2*bits in a TLBuffer(2)

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._
import util._

class TLRationalCrossingSource(implicit p: Parameters) extends LazyModule
{
  val node = TLRationalSourceNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      val bce = edgeIn.manager.anySupportAcquireB && edgeIn.client.anySupportProbe

      out.a <> ToRational(in.a)
      in.d <> FromRational(out.d)

      if (bce) {
        in.b <> FromRational(out.b)
        out.c <> ToRational(in.c)
        out.e <> ToRational(in.e)
      } else {
        in.b.valid   := Bool(false)
        in.c.ready   := Bool(true)
        in.e.ready   := Bool(true)
        out.b.ready  := Bool(true)
        out.c.valid  := Bool(false)
        out.e.valid  := Bool(false)
        out.b.sink   := UInt(0)
        out.c.source := UInt(0)
        out.e.source := UInt(0)
      }
    }
  }
}

class TLRationalCrossingSink(implicit p: Parameters) extends LazyModule
{
  val node = TLRationalSinkNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      val bce = edgeOut.manager.anySupportAcquireB && edgeOut.client.anySupportProbe

      out.a <> FromRational(in.a)
      in.d <> ToRational(out.d)

      if (bce) {
        in.b <> ToRational(out.b)
        out.c <> FromRational(in.c)
        out.e <> FromRational(in.e)
      } else {
        out.b.ready := Bool(true)
        out.c.valid := Bool(false)
        out.e.valid := Bool(false)
        in.b.valid  := Bool(false)
        in.c.ready  := Bool(true)
        in.e.ready  := Bool(true)
        in.b.source := UInt(0)
        in.c.sink   := UInt(0)
        in.e.sink   := UInt(0)
      }
    }
  }
}

object TLRationalCrossingSource
{
  // applied to the TL source node; y.node := TLRationalCrossingSource()(x.node)
  def apply()(x: TLOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): TLRationalOutwardNode = {
    val source = LazyModule(new TLRationalCrossingSource)
    source.node := x
    source.node
  }
}

object TLRationalCrossingSink
{
  // applied to the TL source node; y.node := TLRationalCrossingSink()(x.node)
  def apply()(x: TLRationalOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): TLOutwardNode = {
    val sink = LazyModule(new TLRationalCrossingSink)
    sink.node := x
    sink.node
  }
}

class TLRationalCrossing(implicit p: Parameters) extends LazyModule
{
  val nodeIn = TLInputNode()
  val nodeOut = TLOutputNode()
  val node = NodeHandle(nodeIn, nodeOut)

  val source = LazyModule(new TLRationalCrossingSource)
  val sink = LazyModule(new TLRationalCrossingSink)

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

class TLRAMRationalCrossing(implicit p: Parameters) extends LazyModule {
  val fuzz  = LazyModule(new TLFuzzer(5000))
  val model = LazyModule(new TLRAMModel)
  val cross = LazyModule(new TLRationalCrossing)
  val delay = LazyModule(new TLDelayer(0.25))
  val ram   = LazyModule(new TLRAM(AddressSet(0x0, 0x3ff)))

  model.node := fuzz.node
  cross.node := TLDelayer(0.25)(TLFragmenter(4, 256)(model.node))
  val monitor1 = (delay.node := cross.node)
  val monitor2 = (ram.node := delay.node)
  val monitors = monitor1.toList ++ monitor2.toList

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished

    // Shove the RAM into another clock domain
    val clocks = Module(new util.Pow2ClockDivider(2))
    ram.module.clock := clocks.io.clock_out
    delay.module.clock := clocks.io.clock_out

    // ... and safely cross TL2 into it
    cross.module.io.in_clock := clock
    cross.module.io.in_reset := reset
    cross.module.io.out_clock := clocks.io.clock_out
    cross.module.io.out_reset := reset

    // Push the Monitors into the right clock domain
    monitors.foreach { m => m.module.clock := clocks.io.clock_out }
  }
}

class TLRAMRationalCrossingTest(implicit p: Parameters) extends UnitTest(timeout = 500000) {
  io.finished := Module(LazyModule(new TLRAMRationalCrossing).module).io.finished
}
