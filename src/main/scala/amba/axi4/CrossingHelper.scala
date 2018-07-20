// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

class AXI4CrossingHelper(parent: LazyModule with LazyScope, name: String) extends CrossingHelper(parent, name)
{
  def this(parent: LazyModule with LazyScope)(implicit valName: ValName) = this(parent, valName.name)

  def crossSyncInOut(out: Boolean)(params: BufferParams = BufferParams.default)(implicit p: Parameters): AXI4Node = {
    val axi4_sync_xing = LazyModule(new AXI4Buffer(params))
    crossingCheck(out, axi4_sync_xing.node, axi4_sync_xing.node)
    if (!out) parent { AXI4NameNode(name) :*=* axi4_sync_xing.node }
    else      parent { axi4_sync_xing.node :*=* AXI4NameNode(name) }
  }

  def crossAsyncInOut(out: Boolean)(depth: Int = 8, sync: Int = 3)(implicit p: Parameters): AXI4Node = {
    lazy val axi4_async_xing_source = LazyModule(new AXI4AsyncCrossingSource(sync))
    lazy val axi4_async_xing_sink = LazyModule(new AXI4AsyncCrossingSink(depth, sync))
    val source = if (out) parent { AXI4AsyncNameNode(name) :*=* axi4_async_xing_source.node } else axi4_async_xing_source.node
    val sink = if (out) axi4_async_xing_sink.node else parent { axi4_async_xing_sink.node :*=* AXI4AsyncNameNode(name) }
    crossingCheck(out, axi4_async_xing_source.node, axi4_async_xing_sink.node)
    sink :*=* source
    NodeHandle(source, sink)
  }

  def crossSyncIn (params: BufferParams = BufferParams.default)(implicit p: Parameters): AXI4Node = crossSyncInOut(false)(params)
  def crossSyncOut(params: BufferParams = BufferParams.default)(implicit p: Parameters): AXI4Node = crossSyncInOut(true )(params)
  def crossAsyncIn (depth: Int = 8, sync: Int = 3)(implicit p: Parameters): AXI4Node = crossAsyncInOut(false)(depth, sync)
  def crossAsyncOut(depth: Int = 8, sync: Int = 3)(implicit p: Parameters): AXI4Node = crossAsyncInOut(true )(depth, sync)

  def crossIn(crossingType: ClockCrossingType)(implicit p: Parameters): AXI4Node = crossingType match {
    case x: SynchronousCrossing  => crossSyncIn(x.params)
    case x: AsynchronousCrossing => crossAsyncIn(x.depth, x.sync)
    case x: RationalCrossing     => throw new IllegalArgumentException("AXI4 Rational crossing unimplemented")
  }

  def crossOut(crossingType: ClockCrossingType)(implicit p: Parameters): AXI4Node = crossingType match {
    case x: SynchronousCrossing  => crossSyncOut(x.params)
    case x: AsynchronousCrossing => crossAsyncOut(x.depth, x.sync)
    case x: RationalCrossing     => throw new IllegalArgumentException("AXI4 Rational crossing unimplemented")
  }
}
