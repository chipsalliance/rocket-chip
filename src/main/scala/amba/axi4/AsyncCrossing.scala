// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.CrossingWrapper
import freechips.rocketchip.util._

class AXI4AsyncCrossingSource(sync: Option[Int])(implicit p: Parameters) extends LazyModule
{
  def this(x: Int)(implicit p: Parameters) = this(Some(x))
  def this()(implicit p: Parameters) = this(None)

  val node = AXI4AsyncSourceNode(sync)

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val psync = sync.getOrElse(edgeOut.slave.async.sync)
      val params = edgeOut.slave.async.copy(sync = psync)
      out.ar <> ToAsyncBundle(in.ar, params)
      out.aw <> ToAsyncBundle(in.aw, params)
      out. w <> ToAsyncBundle(in. w, params)
      in .r  <> FromAsyncBundle(out.r, psync)
      in .b  <> FromAsyncBundle(out.b, psync)
    }
  }
}

class AXI4AsyncCrossingSink(params: AsyncQueueParams = AsyncQueueParams())(implicit p: Parameters) extends LazyModule
{
  val node = AXI4AsyncSinkNode(params)

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.ar <> FromAsyncBundle(in.ar, params.sync)
      out.aw <> FromAsyncBundle(in.aw, params.sync)
      out. w <> FromAsyncBundle(in. w, params.sync)
      in .r  <> ToAsyncBundle(out.r, params)
      in .b  <> ToAsyncBundle(out.b, params)
    }
  }
}

object AXI4AsyncCrossingSource
{
  def apply()(implicit p: Parameters): AXI4AsyncSourceNode = apply(None)
  def apply(sync: Int)(implicit p: Parameters): AXI4AsyncSourceNode = apply(Some(sync))
  def apply(sync: Option[Int])(implicit p: Parameters): AXI4AsyncSourceNode = {
    val axi4asource = LazyModule(new AXI4AsyncCrossingSource(sync))
    axi4asource.node
  }
}

object AXI4AsyncCrossingSink
{
  def apply(params: AsyncQueueParams = AsyncQueueParams())(implicit p: Parameters) = {
    val axi4asink = LazyModule(new AXI4AsyncCrossingSink(params))
    axi4asink.node
  }
}

@deprecated("AXI4AsyncCrossing is fragile. Use AXI4AsyncCrossingSource and AXI4AsyncCrossingSink", "rocket-chip 1.2")
class AXI4AsyncCrossing(params: AsyncQueueParams = AsyncQueueParams())(implicit p: Parameters) extends LazyModule
{
  val source = LazyModule(new AXI4AsyncCrossingSource())
  val sink = LazyModule(new AXI4AsyncCrossingSink(params))
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
  val island = LazyModule(new CrossingWrapper(AsynchronousCrossing()))
  val ram  = island { LazyModule(new AXI4RAM(AddressSet(0x0, 0x3ff))) }

  model.node := fuzz.node
  toaxi.node := model.node
  island.crossAXI4In(ram.node) := toaxi.node

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished

    // Shove the RAM into another clock domain
    val clocks = Module(new Pow2ClockDivider(2))
    island.module.clock := clocks.io.clock_out
  }
}

class AXI4RAMAsyncCrossingTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new AXI4RAMAsyncCrossing(txns)).module)
  io.finished := dut.io.finished
}
