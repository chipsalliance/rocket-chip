// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.coreplex.{CrossingWrapper, AsynchronousCrossing}

class AXI4AsyncCrossingSource(sync: Int = 3)(implicit p: Parameters) extends LazyModule
{
  val node = AXI4AsyncSourceNode(sync)

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val depth = edgeOut.slave.depth

      out.ar <> ToAsyncBundle(in.ar, depth, sync)
      out.aw <> ToAsyncBundle(in.aw, depth, sync)
      out. w <> ToAsyncBundle(in. w, depth, sync)
      in .r  <> FromAsyncBundle(out.r, sync)
      in .b  <> FromAsyncBundle(out.b, sync)
    }
  }
}

class AXI4AsyncCrossingSink(depth: Int = 8, sync: Int = 3)(implicit p: Parameters) extends LazyModule
{
  val node = AXI4AsyncSinkNode(depth, sync)

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.ar <> FromAsyncBundle(in.ar, sync)
      out.aw <> FromAsyncBundle(in.aw, sync)
      out. w <> FromAsyncBundle(in. w, sync)
      in .r  <> ToAsyncBundle(out.r, depth, sync)
      in .b  <> ToAsyncBundle(out.b, depth, sync)
    }
  }
}

object AXI4AsyncCrossingSource
{
  // applied to the AXI4 source node; y.node := AXI4AsyncCrossingSource()(x.node)
  def apply(sync: Int = 3)(x: AXI4OutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): AXI4AsyncOutwardNode = {
    val source = LazyModule(new AXI4AsyncCrossingSource(sync))
    source.node :=? x
    source.node
  }
}

object AXI4AsyncCrossingSink
{
  // applied to the AXI4 source node; y.node := AXI4AsyncCrossingSink()(x.node)
  def apply(depth: Int = 8, sync: Int = 3)(x: AXI4AsyncOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): AXI4OutwardNode = {
    val sink = LazyModule(new AXI4AsyncCrossingSink(depth, sync))
    sink.node :=? x
    sink.node
  }
}

class AXI4AsyncCrossing(depth: Int = 8, sync: Int = 3)(implicit p: Parameters) extends LazyModule
{
  val source = LazyModule(new AXI4AsyncCrossingSource(sync))
  val sink = LazyModule(new AXI4AsyncCrossingSink(depth, sync))
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

class AXI4RAMAsyncCrossing(txns: Int)(implicit p: Parameters) extends LazyModule {
  val model = LazyModule(new TLRAMModel("AsyncCrossing"))
  val fuzz = LazyModule(new TLFuzzer(txns))
  val toaxi = LazyModule(new TLToAXI4)
  val island = LazyModule(new CrossingWrapper(AsynchronousCrossing(8)))
  val ram  = island { LazyModule(new AXI4RAM(AddressSet(0x0, 0x3ff))) }

  model.node := fuzz.node
  toaxi.node := model.node
  ram.node := island.crossAXI4In := toaxi.node

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished

    // Shove the RAM into another clock domain
    val clocks = Module(new Pow2ClockDivider(2))
    island.module.clock := clocks.io.clock_out
  }
}

class AXI4RAMAsyncCrossingTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  io.finished := Module(LazyModule(new AXI4RAMAsyncCrossing(txns)).module).io.finished
}
