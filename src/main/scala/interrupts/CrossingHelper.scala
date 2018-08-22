// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

case class IntInwardCrossingHelper(name: String, scope: LazyScope, node: IntInwardNode) {
  def apply(xing: ClockCrossingType = NoCrossing, alreadyRegistered: Boolean = false)(implicit p: Parameters): IntInwardNode = {
    xing match {
      case x: AsynchronousCrossing =>
        node :*=* scope { IntSyncCrossingSink(x.sinkSync) :*=* IntSyncNameNode(name) } :*=* IntSyncNameNode(name) :*=* IntSyncCrossingSource(alreadyRegistered)
      case RationalCrossing(_) =>
        node :*=* scope { IntSyncCrossingSink(1) :*=* IntSyncNameNode(name) } :*=* IntSyncNameNode(name) :*=* IntSyncCrossingSource(alreadyRegistered)
      case SynchronousCrossing(_) =>
        node :*=* scope { IntSyncCrossingSink(0) :*=* IntSyncNameNode(name) } :*=* IntSyncNameNode(name) :*=* IntSyncCrossingSource(alreadyRegistered)
    }
  }
}

case class IntOutwardCrossingHelper(name: String, scope: LazyScope, node: IntOutwardNode) {
  def apply(xing: ClockCrossingType = NoCrossing, alreadyRegistered: Boolean = false)(implicit p: Parameters): IntOutwardNode = {
    xing match {
      case x: AsynchronousCrossing =>
        IntSyncCrossingSink(x.sinkSync) :*=* IntSyncNameNode(name) :*=* scope { IntSyncNameNode(name) :*=* IntSyncCrossingSource(alreadyRegistered) } :*=* node
      case RationalCrossing(_) =>
        IntSyncCrossingSink(1) :*=* IntSyncNameNode(name) :*=* scope { IntSyncNameNode(name) :*=* IntSyncCrossingSource(alreadyRegistered) } :*=* node
      case SynchronousCrossing(buffer) =>
        IntSyncCrossingSink(0) :*=* IntSyncNameNode(name) :*=* scope { IntSyncNameNode(name) :*=* IntSyncCrossingSource(alreadyRegistered) } :*=* node
    }
  }
}
