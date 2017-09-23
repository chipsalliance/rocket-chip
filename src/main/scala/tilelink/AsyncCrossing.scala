// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class TLAsyncCrossingSource(sync: Int = 3)(implicit p: Parameters) extends LazyModule
{
  val node = TLAsyncSourceNode(sync)

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val sink_reset_n = out.a.sink_reset_n
      val bce = edgeIn.manager.anySupportAcquireB && edgeIn.client.anySupportProbe
      val depth = edgeOut.manager.depth

      out.a <> ToAsyncBundle(in.a, depth, sync)
      in.d <> FromAsyncBundle(out.d, sync)

      if (bce) {
        in.b <> FromAsyncBundle(out.b, sync)
        out.c <> ToAsyncBundle(in.c, depth, sync)
        out.e <> ToAsyncBundle(in.e, depth, sync)
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

class TLAsyncCrossingSink(depth: Int = 8, sync: Int = 3)(implicit p: Parameters) extends LazyModule
{
  val node = TLAsyncSinkNode(depth, sync)

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val source_reset_n = in.a.source_reset_n
      val bce = edgeOut.manager.anySupportAcquireB && edgeOut.client.anySupportProbe

      out.a <> FromAsyncBundle(in.a, sync)
      in.d <> ToAsyncBundle(out.d, depth, sync)

      if (bce) {
        in.b <> ToAsyncBundle(out.b, depth, sync)
        out.c <> FromAsyncBundle(in.c, sync)
        out.e <> FromAsyncBundle(in.e, sync)
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
  def apply(sync: Int = 3)(x: TLOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): TLAsyncOutwardNode = {
    val source = LazyModule(new TLAsyncCrossingSource(sync))
    source.node :=? x
    source.node
  }
}

object TLAsyncCrossingSink
{
  // applied to the TL source node; y.node := TLAsyncCrossingSink()(x.node)
  def apply(depth: Int = 8, sync: Int = 3)(x: TLAsyncOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): TLOutwardNode = {
    val sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
    sink.node :=? x
    sink.node
  }
}

class TLAsyncCrossing(depth: Int = 8, sync: Int = 3)(implicit p: Parameters) extends LazyModule
{
  val source = LazyModule(new TLAsyncCrossingSource(sync))
  val sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
  val node = NodeHandle(source.node, sink.node)

  sink.node := source.node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val in_clock  = Clock(INPUT)
      val in_reset  = Bool(INPUT)
      val out_clock = Clock(INPUT)
      val out_reset = Bool(INPUT)
    })

    source.module.clock := io.in_clock
    source.module.reset := io.in_reset
    sink.module.clock := io.out_clock
    sink.module.reset := io.out_reset
  }
}

/** Synthesizeable unit tests */
import freechips.rocketchip.unittest._

class TLRAMAsyncCrossing(txns: Int)(implicit p: Parameters) extends LazyModule {
  val model = LazyModule(new TLRAMModel("AsyncCrossing"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x3ff)))
  val fuzz = LazyModule(new TLFuzzer(txns))
  val cross = LazyModule(new TLAsyncCrossing)

  model.node := fuzz.node
  cross.node := TLFragmenter(4, 256)(TLDelayer(0.1)(model.node))
  ram.node := cross.node

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished

    // Shove the RAM into another clock domain
    val clocks = Module(new Pow2ClockDivider(2))
    ram.module.clock := clocks.io.clock_out

    // ... and safely cross TL2 into it
    cross.module.io.in_clock := clock
    cross.module.io.in_reset := reset
    cross.module.io.out_clock := clocks.io.clock_out
    cross.module.io.out_reset := reset
  }
}

class TLRAMAsyncCrossingTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  io.finished := Module(LazyModule(new TLRAMAsyncCrossing(txns)).module).io.finished
}
