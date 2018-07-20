// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

class IntCrossingHelper(parent: LazyModule with LazyScope, name: String) extends CrossingHelper(parent, name)
{
  def this(parent: LazyModule with LazyScope)(implicit valName: ValName) = this(parent, valName.name)

  def crossIntSyncInOut(out: Boolean)(alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = {
    lazy val int_sync_xing_source = LazyModule(new IntSyncCrossingSource(alreadyRegistered))
    lazy val int_sync_xing_sink = LazyModule(new IntSyncCrossingSink(0))
    val source = if (out) parent { IntSyncNameNode(name) :*=* int_sync_xing_source.node } else int_sync_xing_source.node
    val sink = if (out) int_sync_xing_sink.node else parent { int_sync_xing_sink.node :*=* IntSyncNameNode(name) }
    crossingCheck(out, int_sync_xing_source.node, int_sync_xing_sink.node)
    sink :*=* source
    NodeHandle(source, sink)
  }

  def crossIntAsyncInOut(out: Boolean)(sync: Int = 3, alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = {
    lazy val int_async_xing_source = LazyModule(new IntSyncCrossingSource(alreadyRegistered))
    lazy val int_async_xing_sink = LazyModule(new IntSyncCrossingSink(sync))
    val source = if (out) parent {  IntSyncNameNode(name) :*=* int_async_xing_source.node } else int_async_xing_source.node
    val sink = if (out) int_async_xing_sink.node else parent { int_async_xing_sink.node :*=* IntSyncNameNode(name) }
    crossingCheck(out, int_async_xing_source.node, int_async_xing_sink.node)
    sink :*=* source
    NodeHandle(source, sink)
  }

  def crossIntRationalInOut(out: Boolean)(alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = {
    lazy val int_rational_xing_source = LazyModule(new IntSyncCrossingSource(alreadyRegistered))
    lazy val int_rational_xing_sink = LazyModule(new IntSyncCrossingSink(1))
    val source = if (out) parent { IntSyncNameNode(name) :*=* int_rational_xing_source.node } else int_rational_xing_source.node
    val sink = if (out) int_rational_xing_sink.node else parent {  int_rational_xing_sink.node :*=* IntSyncNameNode(name) }
    crossingCheck(out, int_rational_xing_source.node, int_rational_xing_sink.node)
    sink :*=* source
    NodeHandle(source, sink)
  }

  def crossIntSyncIn (alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossIntSyncInOut(false)(alreadyRegistered)
  def crossIntSyncOut(alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossIntSyncInOut(true )(alreadyRegistered)
  def crossIntAsyncIn (sync: Int = 3, alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossIntAsyncInOut(false)(sync, alreadyRegistered)
  def crossIntAsyncOut(sync: Int = 3, alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossIntAsyncInOut(true )(sync, alreadyRegistered)
  def crossIntRationalIn (alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossIntRationalInOut(false)(alreadyRegistered)
  def crossIntRationalOut(alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossIntRationalInOut(true )(alreadyRegistered)

  def crossIntIn(alreadyRegistered: Boolean, crossingType: ClockCrossingType)(implicit p: Parameters): IntNode = crossingType match {
    case x: SynchronousCrossing  => crossIntSyncIn(alreadyRegistered)
    case x: AsynchronousCrossing => crossIntAsyncIn(x.sync, alreadyRegistered)
    case x: RationalCrossing     => crossIntRationalIn(alreadyRegistered)
  }

  def crossIntOut(alreadyRegistered: Boolean, crossingType: ClockCrossingType)(implicit p: Parameters): IntNode = crossingType match {
    case x: SynchronousCrossing  => crossIntSyncOut(alreadyRegistered)
    case x: AsynchronousCrossing => crossIntAsyncOut(x.sync, alreadyRegistered)
    case x: RationalCrossing     => crossIntRationalOut(alreadyRegistered)
  }

  def crossIntIn (crossingType: ClockCrossingType)(implicit p: Parameters): IntNode = crossIntIn (false, crossingType)
  def crossIntOut(crossingType: ClockCrossingType)(implicit p: Parameters): IntNode = crossIntOut(false, crossingType)
}
