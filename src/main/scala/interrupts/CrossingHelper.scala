// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.CreditedDelay

case class IntInwardCrossingHelper(name: String, scope: LazyScope, node: IntInwardNode) {
  def apply(xing: ClockCrossingType = NoCrossing, alreadyRegistered: Boolean = false)(implicit p: Parameters): IntInwardNode = {
    xing match {
      case x: AsynchronousCrossing =>
        node :*=* scope { IntSyncAsyncCrossingSink(x.sinkSync) :*=* IntSyncNameNode(name) } :*=* IntSyncNameNode(name) :*=* IntSyncCrossingSource(alreadyRegistered)
      case RationalCrossing(_) =>
        node :*=* scope { IntSyncRationalCrossingSink() :*=* IntSyncNameNode(name) } :*=* IntSyncNameNode(name) :*=* IntSyncCrossingSource(alreadyRegistered)
      case SynchronousCrossing(_) =>
        node :*=* scope { IntSyncSyncCrossingSink() :*=* IntSyncNameNode(name) } :*=* IntSyncNameNode(name) :*=* IntSyncCrossingSource(alreadyRegistered)
      case CreditedCrossing(CreditedDelay(sourceDebit, _), CreditedDelay(sinkDebit, _)) =>
        node :*=* scope { IntSyncSyncCrossingSink(/*sinkDebit==0*/) :*=* IntSyncNameNode(name) } :*=* IntSyncNameNode(name) :*=* IntSyncCrossingSource(sourceDebit==0)
    }
  }
}

case class IntOutwardCrossingHelper(name: String, scope: LazyScope, node: IntOutwardNode) {
  def apply(xing: ClockCrossingType = NoCrossing, alreadyRegistered: Boolean = false)(implicit p: Parameters): IntOutwardNode = {
    xing match {
      case x: AsynchronousCrossing =>
        IntSyncAsyncCrossingSink(x.sinkSync) :*=* IntSyncNameNode(name) :*=* scope { IntSyncNameNode(name) :*=* IntSyncCrossingSource(alreadyRegistered) } :*=* node
      case RationalCrossing(_) =>
        IntSyncRationalCrossingSink() :*=* IntSyncNameNode(name) :*=* scope { IntSyncNameNode(name) :*=* IntSyncCrossingSource(alreadyRegistered) } :*=* node
      case SynchronousCrossing(buffer) =>
        IntSyncSyncCrossingSink() :*=* IntSyncNameNode(name) :*=* scope { IntSyncNameNode(name) :*=* IntSyncCrossingSource(alreadyRegistered) } :*=* node
      case CreditedCrossing(CreditedDelay(sourceDebit, _), CreditedDelay(sinkDebit, _)) =>
        IntSyncSyncCrossingSink(/*sinkDebit==0*/) :*=* IntSyncNameNode(name) :*=* scope { IntSyncNameNode(name) :*=* IntSyncCrossingSource(sourceDebit==0) } :*=* node
    }
  }
}
