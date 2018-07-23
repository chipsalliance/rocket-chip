// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.RationalDirection

case class TLInwardCrossingHelper(name: String, scope: LazyScope, node: TLInwardNode) {
  def apply(xing: ClockCrossingType = NoCrossing)(implicit p: Parameters): TLInwardNode = {
    xing match {
      case AsynchronousCrossing(depth, sync) =>
        node :*=* scope { TLAsyncCrossingSink(depth, sync) :*=* TLAsyncNameNode(name) } :*=* TLAsyncCrossingSource(sync)
      case RationalCrossing(direction) =>
        node :*=* scope { TLRationalCrossingSink(direction.flip) :*=* TLRationalNameNode(name) } :*=* TLRationalCrossingSource()
      case SynchronousCrossing(buffer) =>
        node :*=* scope { TLBuffer(buffer) :*=* TLNameNode(name) }
    }
  }
}

case class TLOutwardCrossingHelper(name: String, scope: LazyScope, node: TLOutwardNode) {
  def apply(xing: ClockCrossingType = NoCrossing)(implicit p: Parameters): TLOutwardNode = {
    xing match {
      case AsynchronousCrossing(depth, sync) =>
        TLAsyncCrossingSink(depth, sync) :*=* scope { TLAsyncNameNode(name) :*=* TLAsyncCrossingSource(sync) } :*=* node
      case RationalCrossing(direction) =>
        TLRationalCrossingSink(direction) :*=* scope { TLRationalNameNode(name) :*=* TLRationalCrossingSource() } :*=* node
      case SynchronousCrossing(buffer) =>
        scope { TLNameNode(name) :*=* TLBuffer(buffer) } :*=* node
    }
  }
}
