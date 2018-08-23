// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.CrossingWrapper
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._

class TLAsyncCrossingSource(sync: Option[Int])(implicit p: Parameters) extends LazyModule
{
  def this(x: Int)(implicit p: Parameters) = this(Some(x))
  def this()(implicit p: Parameters) = this(None)

  val node = TLAsyncSourceNode(sync)

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val bce = edgeIn.manager.anySupportAcquireB && edgeIn.client.anySupportProbe
      val psync = sync.getOrElse(edgeOut.manager.async.sync)
      val params = edgeOut.manager.async.copy(sync = psync)

      out.a <> ToAsyncBundle(in.a, params)
      in.d <> FromAsyncBundle(out.d, psync)
      cover(in.a, "TL_ASYNC_CROSSING_SOURCE_A", "MemorySystem;;TLAsyncCrossingSource Channel A")
      cover(in.d, "TL_ASYNC_CROSSING_SOURCE_D", "MemorySystem;;TLAsyncCrossingSource Channel D")

      if (bce) {
        in.b <> FromAsyncBundle(out.b, psync)
        out.c <> ToAsyncBundle(in.c, params)
        out.e <> ToAsyncBundle(in.e, params)
        cover(in.b, "TL_ASYNC_CROSSING_SOURCE_B", "MemorySystem;;TLAsyncCrossingSource Channel B")
        cover(in.c, "TL_ASYNC_CROSSING_SOURCE_C", "MemorySystem;;TLAsyncCrossingSource Channel C")
        cover(in.e, "TL_ASYNC_CROSSING_SOURCE_E", "MemorySystem;;TLAsyncCrossingSource Channel E")
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

class TLAsyncCrossingSink(params: AsyncQueueParams = AsyncQueueParams())(implicit p: Parameters) extends LazyModule
{
  val node = TLAsyncSinkNode(params)

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val bce = edgeOut.manager.anySupportAcquireB && edgeOut.client.anySupportProbe

      out.a <> FromAsyncBundle(in.a, params.sync)
      in.d <> ToAsyncBundle(out.d, params)
      cover(out.a, "TL_ASYNC_CROSSING_SINK_A", "MemorySystem;;TLAsyncCrossingSink Channel A")
      cover(out.d, "TL_ASYNC_CROSSING_SINK_D", "MemorySystem;;TLAsyncCrossingSink Channel D")

      if (bce) {
        in.b <> ToAsyncBundle(out.b, params)
        out.c <> FromAsyncBundle(in.c, params.sync)
        out.e <> FromAsyncBundle(in.e, params.sync)
        cover(out.b, "TL_ASYNC_CROSSING_SINK_B", "MemorySystem;;TLAsyncCrossingSinkChannel B")
        cover(out.c, "TL_ASYNC_CROSSING_SINK_C", "MemorySystem;;TLAsyncCrossingSink Channel C")
        cover(out.e, "TL_ASYNC_CROSSING_SINK_E", "MemorySystem;;TLAsyncCrossingSink Channel E")
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
  def apply()(implicit p: Parameters): TLAsyncSourceNode = apply(None)
  def apply(sync: Int)(implicit p: Parameters): TLAsyncSourceNode = apply(Some(sync))
  def apply(sync: Option[Int])(implicit p: Parameters): TLAsyncSourceNode =
  {
    val asource = LazyModule(new TLAsyncCrossingSource(sync))
    asource.node
  }
}

object TLAsyncCrossingSink
{
  def apply(params: AsyncQueueParams = AsyncQueueParams())(implicit p: Parameters) =
  {
    val asink = LazyModule(new TLAsyncCrossingSink(params))
    asink.node
  }
}

@deprecated("TLAsyncCrossing is fragile. Use TLAsyncCrossingSource and TLAsyncCrossingSink", "rocket-chip 1.2")
class TLAsyncCrossing(params: AsyncQueueParams = AsyncQueueParams())(implicit p: Parameters) extends LazyModule
{
  val source = LazyModule(new TLAsyncCrossingSource())
  val sink = LazyModule(new TLAsyncCrossingSink(params))
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

class TLRAMAsyncCrossing(txns: Int, params: AsynchronousCrossing = AsynchronousCrossing())(implicit p: Parameters) extends LazyModule {
  val model = LazyModule(new TLRAMModel("AsyncCrossing"))
  val fuzz = LazyModule(new TLFuzzer(txns))
  val island = LazyModule(new CrossingWrapper(params))
  val ram  = island { LazyModule(new TLRAM(AddressSet(0x0, 0x3ff))) }

  island.crossTLIn(ram.node) := TLFragmenter(4, 256) := TLDelayer(0.1) := model.node := fuzz.node

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished

    // Shove the RAM into another clock domain
    val clocks = Module(new Pow2ClockDivider(2))
    island.module.clock := clocks.io.clock_out
  }
}

class TLRAMAsyncCrossingTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut_wide   = Module(LazyModule(new TLRAMAsyncCrossing(txns)).module)
  val dut_narrow = Module(LazyModule(new TLRAMAsyncCrossing(txns, AsynchronousCrossing(safe = false, narrow = true))).module)
  io.finished := dut_wide.io.finished && dut_narrow.io.finished
}
