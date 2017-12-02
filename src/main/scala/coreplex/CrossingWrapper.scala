// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._

/** Enumerates the three types of clock crossing between tiles and system bus */
sealed trait CoreplexClockCrossing
{
  def sameClock = this match {
    case _: SynchronousCrossing => true
    case _ => false
  }
}
case class SynchronousCrossing(params: BufferParams = BufferParams.default) extends CoreplexClockCrossing
case class RationalCrossing(direction: RationalDirection = FastToSlow) extends CoreplexClockCrossing
case class AsynchronousCrossing(depth: Int, sync: Int = 3) extends CoreplexClockCrossing

private case class CrossingCheck(out: Boolean, source: BaseNode, sink: BaseNode)

trait HasCrossingMethods extends LazyModule with LazyScope
{
  // Detect incorrect crossing connectivity

  private var checks: List[CrossingCheck] = Nil
  private def inside(node: BaseNode) = node.parents.exists(_ eq this)
  override def instantiate() {
    super.instantiate()
    checks.foreach { case CrossingCheck(out, source, sink) =>
      source.inputs.foreach { case (syncSource, _) =>
        require (inside(syncSource) == out, s"${syncSource.name} must ${if(out)""else"not "}be inside ${name} (wrong .cross direction?)")
      }
      sink.outputs.foreach { case (syncSink, _) =>
        require (inside(syncSink) != out, s"${syncSink.name} must ${if(out)"not "else""}be inside ${name} (wrong .cross direction?)")
      }
    }
  }

  // TileLink

  def crossTLSyncInOut(out: Boolean)(params: BufferParams = BufferParams.default)(implicit p: Parameters): TLNode = {
    val node = this { LazyModule(new TLBuffer(params)).node }
    checks = CrossingCheck(out, node, node) :: checks
    node
  }

  def crossTLAsyncInOut(out: Boolean)(depth: Int = 8, sync: Int = 3)(implicit p: Parameters): TLNode = {
    lazy val asource = LazyModule(new TLAsyncCrossingSource(sync))
    lazy val asink = LazyModule(new TLAsyncCrossingSink(depth, sync))
    val source = if (out) this { asource } else asource
    val sink = if (out) asink else this { asink }
    sink.node :*=* source.node
    checks = CrossingCheck(out, source.node, sink.node) :: checks
    NodeHandle(source.node, sink.node)
  }

  def crossTLRationalInOut(out: Boolean)(direction: RationalDirection)(implicit p: Parameters): TLNode = {
    lazy val rsource = LazyModule(new TLRationalCrossingSource)
    lazy val rsink = LazyModule(new TLRationalCrossingSink(if (out) direction else direction.flip))
    val source = if (out) this { rsource } else rsource
    val sink = if (out) rsink else this { rsink }
    sink.node :*=* source.node
    checks = CrossingCheck(out, source.node, sink.node) :: checks
    NodeHandle(source.node, sink.node)
  }

  def crossTLSyncIn (params: BufferParams = BufferParams.default)(implicit p: Parameters): TLNode = crossTLSyncInOut(false)(params)
  def crossTLSyncOut(params: BufferParams = BufferParams.default)(implicit p: Parameters): TLNode = crossTLSyncInOut(true )(params)
  def crossTLAsyncIn (depth: Int = 8, sync: Int = 3)(implicit p: Parameters): TLNode = crossTLAsyncInOut(false)(depth, sync)
  def crossTLAsyncOut(depth: Int = 8, sync: Int = 3)(implicit p: Parameters): TLNode = crossTLAsyncInOut(true )(depth, sync)
  def crossTLRationalIn (direction: RationalDirection)(implicit p: Parameters): TLNode = crossTLRationalInOut(false)(direction)
  def crossTLRationalOut(direction: RationalDirection)(implicit p: Parameters): TLNode = crossTLRationalInOut(true )(direction)

  def crossTLIn(arg: CoreplexClockCrossing)(implicit p: Parameters): TLNode = arg match {
    case x: SynchronousCrossing  => crossTLSyncIn(x.params)
    case x: AsynchronousCrossing => crossTLAsyncIn(x.depth, x.sync)
    case x: RationalCrossing     => crossTLRationalIn(x.direction)
  }

  def crossTLOut(arg: CoreplexClockCrossing)(implicit p: Parameters): TLNode = arg match {
    case x: SynchronousCrossing  => crossTLSyncOut(x.params)
    case x: AsynchronousCrossing => crossTLAsyncOut(x.depth, x.sync)
    case x: RationalCrossing     => crossTLRationalOut(x.direction)
  }

  // AXI4

  def crossAXI4SyncInOut(out: Boolean)(params: BufferParams = BufferParams.default)(implicit p: Parameters): AXI4Node = {
    val node = this { LazyModule(new AXI4Buffer(params)).node }
    checks = CrossingCheck(out, node, node) :: checks
    node
  }

  def crossAXI4AsyncInOut(out: Boolean)(depth: Int = 8, sync: Int = 3)(implicit p: Parameters): AXI4Node = {
    lazy val axi4asource = LazyModule(new AXI4AsyncCrossingSource(sync))
    lazy val axi4asink = LazyModule(new AXI4AsyncCrossingSink(depth, sync))
    val source = if (out) this { axi4asource } else axi4asource
    val sink = if (out) axi4asink else this { axi4asink }
    sink.node :*=* source.node
    checks = CrossingCheck(out, source.node, sink.node) :: checks
    NodeHandle(source.node, sink.node)
  }

  def crossAXI4SyncIn (params: BufferParams = BufferParams.default)(implicit p: Parameters): AXI4Node = crossAXI4SyncInOut(false)(params)
  def crossAXI4SyncOut(params: BufferParams = BufferParams.default)(implicit p: Parameters): AXI4Node = crossAXI4SyncInOut(true )(params)
  def crossAXI4AsyncIn (depth: Int = 8, sync: Int = 3)(implicit p: Parameters): AXI4Node = crossAXI4AsyncInOut(false)(depth, sync)
  def crossAXI4AsyncOut(depth: Int = 8, sync: Int = 3)(implicit p: Parameters): AXI4Node = crossAXI4AsyncInOut(true )(depth, sync)

  def crossAXI4In(arg: CoreplexClockCrossing)(implicit p: Parameters): AXI4Node = arg match {
    case x: SynchronousCrossing  => crossAXI4SyncIn(x.params)
    case x: AsynchronousCrossing => crossAXI4AsyncIn(x.depth, x.sync)
    case x: RationalCrossing     => throw new IllegalArgumentException("AXI4 Rational crossing unimplemented")
  }

  def crossAXI4Out(arg: CoreplexClockCrossing)(implicit p: Parameters): AXI4Node = arg match {
    case x: SynchronousCrossing  => crossAXI4SyncOut(x.params)
    case x: AsynchronousCrossing => crossAXI4AsyncOut(x.depth, x.sync)
    case x: RationalCrossing     => throw new IllegalArgumentException("AXI4 Rational crossing unimplemented")
  }

  // Interrupts

  def crossIntSyncInOut(out: Boolean)(alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = {
    lazy val intssource = LazyModule(new IntSyncCrossingSource(alreadyRegistered))
    lazy val intssink = LazyModule(new IntSyncCrossingSink(0))
    val source = if (out) this { intssource } else intssource
    val sink = if (out) intssink else this { intssink }
    sink.node :*=* source.node
    checks = CrossingCheck(out, source.node, sink.node) :: checks
    NodeHandle(source.node, sink.node)
  }

  def crossIntAsyncInOut(out: Boolean)(sync: Int = 3, alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = {
    lazy val intasource = LazyModule(new IntSyncCrossingSource(alreadyRegistered))
    lazy val intasink = LazyModule(new IntSyncCrossingSink(sync))
    val source = if (out) this { intasource } else intasource
    val sink = if (out) intasink else this { intasink }
    sink.node :*=* source.node
    checks = CrossingCheck(out, source.node, sink.node) :: checks
    NodeHandle(source.node, sink.node)
  }

  def crossIntRationalInOut(out: Boolean)(alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = {
    lazy val intrsource = LazyModule(new IntSyncCrossingSource(alreadyRegistered))
    lazy val intrsink = LazyModule(new IntSyncCrossingSink(1))
    val source = if (out) this { intrsource } else intrsource
    val sink = if (out) intrsink else this { intrsink }
    sink.node :*=* source.node
    checks = CrossingCheck(out, source.node, sink.node) :: checks
    NodeHandle(source.node, sink.node)
  }

  def crossIntSyncIn (alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossIntSyncInOut(false)(alreadyRegistered)
  def crossIntSyncOut(alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossIntSyncInOut(true )(alreadyRegistered)
  def crossIntAsyncIn (sync: Int = 3, alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossIntAsyncInOut(false)(sync, alreadyRegistered)
  def crossIntAsyncOut(sync: Int = 3, alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossIntAsyncInOut(true )(sync, alreadyRegistered)
  def crossIntRationalIn (alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossIntRationalInOut(false)(alreadyRegistered)
  def crossIntRationalOut(alreadyRegistered: Boolean = false)(implicit p: Parameters): IntNode = crossIntRationalInOut(true )(alreadyRegistered)

  def crossIntIn(arg: CoreplexClockCrossing, alreadyRegistered: Boolean)(implicit p: Parameters): IntNode = arg match {
    case x: SynchronousCrossing  => crossIntSyncIn(alreadyRegistered)
    case x: AsynchronousCrossing => crossIntAsyncIn(x.sync, alreadyRegistered)
    case x: RationalCrossing     => crossIntRationalIn(alreadyRegistered)
  }

  def crossIntOut(arg: CoreplexClockCrossing, alreadyRegistered: Boolean)(implicit p: Parameters): IntNode = arg match {
    case x: SynchronousCrossing  => crossIntSyncOut(alreadyRegistered)
    case x: AsynchronousCrossing => crossIntAsyncOut(x.sync, alreadyRegistered)
    case x: RationalCrossing     => crossIntRationalOut(alreadyRegistered)
  }

  def crossIntIn (arg: CoreplexClockCrossing)(implicit p: Parameters): IntNode = crossIntIn (arg, false)
  def crossIntOut(arg: CoreplexClockCrossing)(implicit p: Parameters): IntNode = crossIntOut(arg, false)
}

trait HasCrossing extends HasCrossingMethods
{
  this: LazyModule =>
  val crossing: CoreplexClockCrossing

  def crossTLIn   (implicit p: Parameters): TLNode  = crossTLIn   (crossing)
  def crossTLOut  (implicit p: Parameters): TLNode  = crossTLOut  (crossing)
  def crossAXI4In (implicit p: Parameters): AXI4Node= crossAXI4In (crossing)
  def crossAXI4Out(implicit p: Parameters): AXI4Node= crossAXI4Out(crossing)
  def crossIntIn  (implicit p: Parameters): IntNode = crossIntIn  (crossing)
  def crossIntOut (implicit p: Parameters): IntNode = crossIntOut (crossing)

  def crossIntIn (alreadyRegistered: Boolean)(implicit p: Parameters): IntNode = crossIntIn (crossing, alreadyRegistered)
  def crossIntOut(alreadyRegistered: Boolean)(implicit p: Parameters): IntNode = crossIntOut(crossing, alreadyRegistered)
}

class CrossingWrapper(val crossing: CoreplexClockCrossing)(implicit p: Parameters) extends SimpleLazyModule with HasCrossing
