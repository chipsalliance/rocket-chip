// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ResetCrossingType, NoResetCrossing, StretchedResetCrossing}
import freechips.rocketchip.util.CreditedDelay

trait IntOutwardCrossingHelper {
  type HelperCrossingType <: CrossingType
  def apply(xing: HelperCrossingType)(implicit p: Parameters): IntOutwardNode
}

trait IntInwardCrossingHelper {
  type HelperCrossingType <: CrossingType
  def apply(xing: HelperCrossingType)(implicit p: Parameters): IntInwardNode
}

case class IntInwardClockCrossingHelper(name: String, scope: LazyScope, node: IntInwardNode)
  extends IntInwardCrossingHelper
{
  type HelperCrossingType = ClockCrossingType
  def apply(xing: ClockCrossingType)(implicit p: Parameters): IntInwardNode = apply(xing, false)
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

case class IntInwardResetCrossingHelper(name: String, scope: LazyScope, node: IntInwardNode)
  extends IntInwardCrossingHelper
{
  type HelperCrossingType = ResetCrossingType
  def apply(xing: ResetCrossingType)(implicit p: Parameters): IntInwardNode = {
    xing match {
      case _: NoResetCrossing => node
      case s: StretchedResetCrossing =>
        node :*=* scope { IntNameNode(name) } :*=* IntBlockDuringReset(s.cycles)
    }
  }
}

case class IntOutwardClockCrossingHelper(name: String, scope: LazyScope, node: IntOutwardNode)
  extends IntOutwardCrossingHelper
{
  type HelperCrossingType = ClockCrossingType
  def apply(xing: ClockCrossingType)(implicit p: Parameters): IntOutwardNode = apply(xing, false)
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

case class IntOutwardResetCrossingHelper(name: String, scope: LazyScope, node: IntOutwardNode)
  extends IntOutwardCrossingHelper
{
  type HelperCrossingType = ResetCrossingType
  def apply(xing: ResetCrossingType)(implicit p: Parameters): IntOutwardNode = {
    xing match {
      case _: NoResetCrossing => node
      case s: StretchedResetCrossing =>
        IntBlockDuringReset(s.cycles) :*=* scope { IntNameNode(name) } :*=* node
    }
  }
}
