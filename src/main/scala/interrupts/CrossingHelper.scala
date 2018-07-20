// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

class IntCrossingHelper(parent: LazyModule with LazyScope, name: String) extends CrossingHelper(parent, name)
{
  def this(parent: LazyModule with LazyScope)(implicit valName: ValName) = this(parent, valName.name)

  def crossSyncInOut(out: Boolean)(alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = {
    lazy val int_sync_xing_source = LazyModule(new IntSyncCrossingSource(alreadyRegistered))
    lazy val int_sync_xing_sink = LazyModule(new IntSyncCrossingSink(0))
    val source = if (out) parent { IntSyncNameNode(name) :*=* int_sync_xing_source.node } else int_sync_xing_source.node
    val sink = if (out) int_sync_xing_sink.node else parent { int_sync_xing_sink.node :*=* IntSyncNameNode(name) }
    crossingCheck(out, int_sync_xing_source.node, int_sync_xing_sink.node)
    sink :*=* source
    NodeHandle(source, sink)
  }

  def crossAsyncInOut(out: Boolean)(sync: Int = 3, alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = {
    lazy val int_async_xing_source = LazyModule(new IntSyncCrossingSource(alreadyRegistered))
    lazy val int_async_xing_sink = LazyModule(new IntSyncCrossingSink(sync))
    val source = if (out) parent {  IntSyncNameNode(name) :*=* int_async_xing_source.node } else int_async_xing_source.node
    val sink = if (out) int_async_xing_sink.node else parent { int_async_xing_sink.node :*=* IntSyncNameNode(name) }
    crossingCheck(out, int_async_xing_source.node, int_async_xing_sink.node)
    sink :*=* source
    NodeHandle(source, sink)
  }

  def crossRationalInOut(out: Boolean)(alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = {
    lazy val int_rational_xing_source = LazyModule(new IntSyncCrossingSource(alreadyRegistered))
    lazy val int_rational_xing_sink = LazyModule(new IntSyncCrossingSink(1))
    val source = if (out) parent { IntSyncNameNode(name) :*=* int_rational_xing_source.node } else int_rational_xing_source.node
    val sink = if (out) int_rational_xing_sink.node else parent {  int_rational_xing_sink.node :*=* IntSyncNameNode(name) }
    crossingCheck(out, int_rational_xing_source.node, int_rational_xing_sink.node)
    sink :*=* source
    NodeHandle(source, sink)
  }

  def crossSyncIn (alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossSyncInOut(false)(alreadyRegistered)
  def crossSyncOut(alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossSyncInOut(true )(alreadyRegistered)
  def crossAsyncIn (sync: Int = 3, alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossAsyncInOut(false)(sync, alreadyRegistered)
  def crossAsyncOut(sync: Int = 3, alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossAsyncInOut(true )(sync, alreadyRegistered)
  def crossRationalIn (alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossRationalInOut(false)(alreadyRegistered)
  def crossRationalOut(alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossRationalInOut(true )(alreadyRegistered)

  def crossIn(alreadyRegistered: Boolean, crossingType: ClockCrossingType)(implicit p: Parameters): IntNode = crossingType match {
    case x: SynchronousCrossing  => crossSyncIn(alreadyRegistered)
    case x: AsynchronousCrossing => crossAsyncIn(x.sync, alreadyRegistered)
    case x: RationalCrossing     => crossRationalIn(alreadyRegistered)
  }

  def crossOut(alreadyRegistered: Boolean, crossingType: ClockCrossingType)(implicit p: Parameters): IntNode = crossingType match {
    case x: SynchronousCrossing  => crossSyncOut(alreadyRegistered)
    case x: AsynchronousCrossing => crossAsyncOut(x.sync, alreadyRegistered)
    case x: RationalCrossing     => crossRationalOut(alreadyRegistered)
  }

  def crossIn (crossingType: ClockCrossingType)(implicit p: Parameters): IntNode = crossIn (false, crossingType)
  def crossOut(crossingType: ClockCrossingType)(implicit p: Parameters): IntNode = crossOut(false, crossingType)
}
