// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.RationalDirection

class TLCrossingHelper(parent: LazyModule with LazyScope, name: String) extends CrossingHelper(parent, name)
{
  def this(parent: LazyModule with LazyScope)(implicit valName: ValName) = this(parent, valName.name)

  def crossSyncInOut(out: Boolean)(params: BufferParams = BufferParams.default)(implicit p: Parameters): TLNode = {
    lazy val sync_xing = LazyModule(new TLBuffer(params))
    crossingCheck(out, sync_xing.node, sync_xing.node)
    if (!out) parent { TLNameNode(name) :*=* sync_xing.node }
    else      parent { sync_xing.node :*=* TLNameNode(name) }
  }

  def crossAsyncInOut(out: Boolean)(depth: Int = 8, sync: Int = 3)(implicit p: Parameters): TLNode = {
    lazy val async_xing_source = LazyModule(new TLAsyncCrossingSource(sync))
    lazy val async_xing_sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
    val source = if (out) parent { TLAsyncNameNode(name) :*=* async_xing_source.node } else async_xing_source.node
    val sink = if (out) async_xing_sink.node else parent { async_xing_sink.node :*=* TLAsyncNameNode(name) }
    crossingCheck(out, async_xing_source.node, async_xing_sink.node)
    sink :*=* source
    NodeHandle(source, sink)
  }

  def crossRationalInOut(out: Boolean)(direction: RationalDirection)(implicit p: Parameters): TLNode = {
    lazy val rational_xing_source = LazyModule(new TLRationalCrossingSource)
    lazy val rational_xing_sink = LazyModule(new TLRationalCrossingSink(if (out) direction else direction.flip))
    val source = if (out) parent { TLRationalNameNode(name) :*=* rational_xing_source.node } else rational_xing_source.node
    val sink = if (out) rational_xing_sink.node else parent { rational_xing_sink.node :*=* TLRationalNameNode(name) }
    crossingCheck(out, rational_xing_source.node, rational_xing_sink.node)
    sink :*=* source
    NodeHandle(source, sink)
  }

  def crossSyncIn (params: BufferParams = BufferParams.default)(implicit p: Parameters): TLNode = crossSyncInOut(false)(params)
  def crossSyncOut(params: BufferParams = BufferParams.default)(implicit p: Parameters): TLNode = crossSyncInOut(true )(params)
  def crossAsyncIn (depth: Int = 8, sync: Int = 3)(implicit p: Parameters): TLNode = crossAsyncInOut(false)(depth, sync)
  def crossAsyncOut(depth: Int = 8, sync: Int = 3)(implicit p: Parameters): TLNode = crossAsyncInOut(true )(depth, sync)
  def crossRationalIn (direction: RationalDirection)(implicit p: Parameters): TLNode = crossRationalInOut(false)(direction)
  def crossRationalOut(direction: RationalDirection)(implicit p: Parameters): TLNode = crossRationalInOut(true )(direction)

  def crossIn(crossingType: ClockCrossingType)(implicit p: Parameters): TLNode = crossingType match {
    case x: SynchronousCrossing  => crossSyncIn(x.params)
    case x: AsynchronousCrossing => crossAsyncIn(x.depth, x.sync)
    case x: RationalCrossing     => crossRationalIn(x.direction)
  }

  def crossOut(crossingType: ClockCrossingType)(implicit p: Parameters): TLNode = crossingType match {
    case x: SynchronousCrossing  => crossSyncOut(x.params)
    case x: AsynchronousCrossing => crossAsyncOut(x.depth, x.sync)
    case x: RationalCrossing     => crossRationalOut(x.direction)
  }
}
