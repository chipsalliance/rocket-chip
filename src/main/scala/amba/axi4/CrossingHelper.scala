// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

class AXI4CrossingHelper(parent: LazyModule with LazyScope, crossingType: ClockCrossingType, name: String)
    extends CrossingHelper(parent, name)
{
  def this(parent: LazyModule with LazyScope, crossingType: ClockCrossingType)
          (implicit valName: ValName) = this(parent, crossingType, valName.name)

  def crossAXI4SyncInOut(out: Boolean)(params: BufferParams = BufferParams.default)(implicit p: Parameters): AXI4Node = {
    val axi4_sync_xing = LazyModule(new AXI4Buffer(params))
    crossingCheck(out, axi4_sync_xing.node, axi4_sync_xing.node)
    if (!out) parent { AXI4NameNode(name) :*=* axi4_sync_xing.node }
    else      parent { axi4_sync_xing.node :*=* AXI4NameNode(name) }
  }

  def crossAXI4AsyncInOut(out: Boolean)(depth: Int = 8, sync: Int = 3)(implicit p: Parameters): AXI4Node = {
    lazy val axi4_async_xing_source = LazyModule(new AXI4AsyncCrossingSource(sync))
    lazy val axi4_async_xing_sink = LazyModule(new AXI4AsyncCrossingSink(depth, sync))
    val source = if (out) parent { AXI4AsyncNameNode(name) :*=* axi4_async_xing_source.node } else axi4_async_xing_source.node
    val sink = if (out) axi4_async_xing_sink.node else parent { axi4_async_xing_sink.node :*=* AXI4AsyncNameNode(name) }
    crossingCheck(out, axi4_async_xing_source.node, axi4_async_xing_sink.node)
    sink :*=* source
    NodeHandle(source, sink)
  }

  def crossAXI4SyncIn (params: BufferParams = BufferParams.default)(implicit p: Parameters): AXI4Node = crossAXI4SyncInOut(false)(params)
  def crossAXI4SyncOut(params: BufferParams = BufferParams.default)(implicit p: Parameters): AXI4Node = crossAXI4SyncInOut(true )(params)
  def crossAXI4AsyncIn (depth: Int = 8, sync: Int = 3)(implicit p: Parameters): AXI4Node = crossAXI4AsyncInOut(false)(depth, sync)
  def crossAXI4AsyncOut(depth: Int = 8, sync: Int = 3)(implicit p: Parameters): AXI4Node = crossAXI4AsyncInOut(true )(depth, sync)

  def crossAXI4In(implicit p: Parameters): AXI4Node = crossingType match {
    case x: SynchronousCrossing  => crossAXI4SyncIn(x.params)
    case x: AsynchronousCrossing => crossAXI4AsyncIn(x.depth, x.sync)
    case x: RationalCrossing     => throw new IllegalArgumentException("AXI4 Rational crossing unimplemented")
  }

  def crossAXI4Out(implicit p: Parameters): AXI4Node = crossingType match {
    case x: SynchronousCrossing  => crossAXI4SyncOut(x.params)
    case x: AsynchronousCrossing => crossAXI4AsyncOut(x.depth, x.sync)
    case x: RationalCrossing     => throw new IllegalArgumentException("AXI4 Rational crossing unimplemented")
  }
}

class AXI4CrossingWrapper(val crossing: ClockCrossingType)(implicit p: Parameters) extends SimpleLazyModule with LazyScope {
  protected lazy val axi4_xing = new AXI4CrossingHelper(this, crossing)
  def crossAXI4In (implicit p: Parameters): AXI4Node= axi4_xing.crossAXI4In
  def crossAXI4Out(implicit p: Parameters): AXI4Node= axi4_xing.crossAXI4Out
}
