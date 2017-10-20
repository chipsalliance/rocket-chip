// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

trait HasCrossingHelper extends LazyScope
{
  this: LazyModule =>
  val crossing: CoreplexClockCrossing
  def cross(x: TLCrossableNode, name: String): TLOutwardNode = {
    val out = x.node.parentsOut.exists(_ eq this) // is the crossing exiting the wrapper?
    crossing match {
      case SynchronousCrossing(params) => {
        val buffer = this { LazyModule(new TLBuffer(params)) }
        buffer.suggestName(name + "SynchronousBuffer")
        buffer.node := x.node
        buffer.node
      }
      case RationalCrossing(direction) => {
        def sourceGen = LazyModule(new TLRationalCrossingSource)
        def sinkGen = LazyModule(new TLRationalCrossingSink(direction))
        val source = if (out) this { sourceGen } else sourceGen
        val sink = if (out) sinkGen else this { sinkGen }
        source.suggestName(name + "RationalSource")
        sink.suggestName(name + "RationalSink")
        source.node := x.node
        sink.node := source.node
        sink.node
      }
      case AsynchronousCrossing(depth, sync) => {
        def sourceGen = this { LazyModule(new TLAsyncCrossingSource(sync)) }
        def sinkGen = LazyModule(new TLAsyncCrossingSink(depth, sync))
        val source = if (out) this { sourceGen } else sourceGen
        val sink = if (out) sinkGen else this { sinkGen }
        source.suggestName(name + "AsynchronousSource")
        sink.suggestName(name + "AsynchronousSink")
        source.node := x.node
        sink.node := source.node
        sink.node
      }
    }
  }
  // def cross(x: IntCrossableNode, name: String): IntOutwardNode = { x.node }
}

class CrossingWrapper(val crossing: CoreplexClockCrossing)(implicit p: Parameters) extends SimpleLazyModule with HasCrossingHelper

