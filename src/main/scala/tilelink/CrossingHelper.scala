// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.RationalDirection

class TLCrossingHelper(parent: LazyModule with LazyScope, crossingType: ClockCrossingType, name: String)
    extends CrossingHelper(parent, name)
{
  def this(parent: LazyModule with LazyScope, crossingType: ClockCrossingType)
          (implicit valName: ValName) = this(parent, crossingType, valName.name)

  def crossTLSyncInOut(out: Boolean)(params: BufferParams = BufferParams.default)(implicit p: Parameters): TLNode = {
    lazy val sync_xing = LazyModule(new TLBuffer(params))
    crossingCheck(out, sync_xing.node, sync_xing.node)
    if (!out) parent { TLNameNode(name) :*=* sync_xing.node }
    else      parent { sync_xing.node :*=* TLNameNode(name) }
  }

  def crossTLAsyncInOut(out: Boolean)(depth: Int = 8, sync: Int = 3)(implicit p: Parameters): TLNode = {
    lazy val async_xing_source = LazyModule(new TLAsyncCrossingSource(sync))
    lazy val async_xing_sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
    val source = if (out) parent { TLAsyncNameNode(name) :*=* async_xing_source.node } else async_xing_source.node
    val sink = if (out) async_xing_sink.node else parent { async_xing_sink.node :*=* TLAsyncNameNode(name) }
    crossingCheck(out, async_xing_source.node, async_xing_sink.node)
    sink :*=* source
    NodeHandle(source, sink)
  }

  def crossTLRationalInOut(out: Boolean)(direction: RationalDirection)(implicit p: Parameters): TLNode = {
    lazy val rational_xing_source = LazyModule(new TLRationalCrossingSource)
    lazy val rational_xing_sink = LazyModule(new TLRationalCrossingSink(if (out) direction else direction.flip))
    val source = if (out) parent { TLRationalNameNode(name) :*=* rational_xing_source.node } else rational_xing_source.node
    val sink = if (out) rational_xing_sink.node else parent { rational_xing_sink.node :*=* TLRationalNameNode(name) }
    crossingCheck(out, rational_xing_source.node, rational_xing_sink.node)
    sink :*=* source
    NodeHandle(source, sink)
  }

  def crossTLSyncIn (params: BufferParams = BufferParams.default)(implicit p: Parameters): TLNode = crossTLSyncInOut(false)(params)
  def crossTLSyncOut(params: BufferParams = BufferParams.default)(implicit p: Parameters): TLNode = crossTLSyncInOut(true )(params)
  def crossTLAsyncIn (depth: Int = 8, sync: Int = 3)(implicit p: Parameters): TLNode = crossTLAsyncInOut(false)(depth, sync)
  def crossTLAsyncOut(depth: Int = 8, sync: Int = 3)(implicit p: Parameters): TLNode = crossTLAsyncInOut(true )(depth, sync)
  def crossTLRationalIn (direction: RationalDirection)(implicit p: Parameters): TLNode = crossTLRationalInOut(false)(direction)
  def crossTLRationalOut(direction: RationalDirection)(implicit p: Parameters): TLNode = crossTLRationalInOut(true )(direction)

  def crossTLIn(implicit p: Parameters): TLNode = crossingType match {
    case x: SynchronousCrossing  => crossTLSyncIn(x.params)
    case x: AsynchronousCrossing => crossTLAsyncIn(x.depth, x.sync)
    case x: RationalCrossing     => crossTLRationalIn(x.direction)
  }

  def crossTLOut(implicit p: Parameters): TLNode = crossingType match {
    case x: SynchronousCrossing  => crossTLSyncOut(x.params)
    case x: AsynchronousCrossing => crossTLAsyncOut(x.depth, x.sync)
    case x: RationalCrossing     => crossTLRationalOut(x.direction)
  }
}

class TLCrossingWrapper(val crossing: ClockCrossingType)(implicit p: Parameters) extends SimpleLazyModule with LazyScope {
  protected lazy val tl_xing = new TLCrossingHelper(this, crossing)
  def crossTLIn (implicit p: Parameters): TLNode= tl_xing.crossTLIn
  def crossTLOut(implicit p: Parameters): TLNode= tl_xing.crossTLOut
}
