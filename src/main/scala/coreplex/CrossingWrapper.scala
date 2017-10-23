// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._

/** Enumerates the three types of clock crossing between tiles and system bus */
sealed trait CoreplexClockCrossing
case class SynchronousCrossing(params: BufferParams = BufferParams.default) extends CoreplexClockCrossing
case class RationalCrossing(direction: RationalDirection = FastToSlow) extends CoreplexClockCrossing
case class AsynchronousCrossing(depth: Int, sync: Int = 3) extends CoreplexClockCrossing

trait HasCrossingHelper extends LazyScope
{
  this: LazyModule =>
  val crossing: CoreplexClockCrossing

  def cross(x: TLCrossableNode): TLOutwardNode = {
    val out = x.node.parentsOut.exists(_ eq this) // is the crossing exiting the wrapper?
    crossing match {
      case SynchronousCrossing(params) => {
        // !!! Why does star resolution fail for tile with no slave devices?
        // this { TLBuffer(params)(x.node) }
        x.node
      }
      case RationalCrossing(direction) => {
        def sourceGen = LazyModule(new TLRationalCrossingSource)
        def sinkGen = LazyModule(new TLRationalCrossingSink(direction))
        val source = if (out) this { sourceGen } else sourceGen
        val sink = if (out) sinkGen else this { sinkGen }
        source.node :=? x.node
        sink.node :=? source.node
        sink.node
      }
      case AsynchronousCrossing(depth, sync) => {
        def sourceGen = LazyModule(new TLAsyncCrossingSource(sync))
        def sinkGen = LazyModule(new TLAsyncCrossingSink(depth, sync))
        val source = if (out) this { sourceGen } else sourceGen
        val sink = if (out) sinkGen else this { sinkGen }
        source.node :=? x.node
        sink.node :=? source.node
        sink.node
      }
    }
  }

  def cross(
      name: Option[String] = None,
      alreadyRegistered: Boolean = false,
      overrideCrossing: Option[CoreplexClockCrossing] = None)
      (x: IntCrossableNode): IntOutwardNode = {
    val out = x.node.parentsOut.exists(_ eq this) // is the crossing exiting the wrapper?
    overrideCrossing.getOrElse(crossing) match {
      case SynchronousCrossing(_) => {
        def sourceGen = LazyModule(new IntSyncCrossingSource(alreadyRegistered))
        def sinkGen = LazyModule(new IntSyncCrossingSink(0))
        val source = if (out) this { sourceGen } else sourceGen
        val sink = if (out) sinkGen else this { sinkGen }
        name.map(_ + "SyncSource").foreach(source.suggestName)
        name.map(_ + "SyncSink").foreach(sink.suggestName)
        source.node :=? x.node
        sink.node :=? source.node
        sink.node
      }
      case RationalCrossing(_) => {
        def sourceGen = LazyModule(new IntSyncCrossingSource(alreadyRegistered))
        def sinkGen = LazyModule(new IntSyncCrossingSink(1))
        val source = if (out) this { sourceGen } else sourceGen
        val sink = if (out) sinkGen else this { sinkGen }
        name.map(_ + "SyncSource").foreach(source.suggestName)
        name.map(_ + "SyncSink").foreach(sink.suggestName)
        source.node :=? x.node
        sink.node :=? source.node
        sink.node
      }
      case AsynchronousCrossing(_, sync) => {
        def sourceGen = LazyModule(new IntSyncCrossingSource(alreadyRegistered))
        def sinkGen = LazyModule(new IntSyncCrossingSink(sync))
        val source = if (out) this { sourceGen } else sourceGen
        val sink = if (out) sinkGen else this { sinkGen }
        name.map(_ + "SyncSource").foreach(source.suggestName)
        name.map(_ + "SyncSink").foreach(sink.suggestName)
        source.node :=? x.node
        sink.node :=? source.node
        sink.node
      }
    }
  }
}

class CrossingWrapper(val crossing: CoreplexClockCrossing)(implicit p: Parameters) extends SimpleLazyModule with HasCrossingHelper
