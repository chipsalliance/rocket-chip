// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._

trait TLOutwardCrossingHelper {
  type HelperCrossingType <: CrossingType
  def apply(xing: HelperCrossingType)(implicit p: Parameters): TLOutwardNode
}

trait TLInwardCrossingHelper {
  type HelperCrossingType <: CrossingType
  def apply(xing: HelperCrossingType)(implicit p: Parameters): TLInwardNode
}

case class TLInwardClockCrossingHelper(name: String, scope: LazyScope, node: TLInwardNode)
    extends TLInwardCrossingHelper
{
  type HelperCrossingType = ClockCrossingType
  def apply(xing: ClockCrossingType = NoCrossing)(implicit p: Parameters): TLInwardNode = {
    xing match {
      case x: AsynchronousCrossing =>
        node :*=* scope { TLAsyncCrossingSink(x.asSinkParams) :*=* TLAsyncNameNode(name) } :*=* TLAsyncNameNode(name) :*=* TLAsyncCrossingSource(x.sourceSync)
      case RationalCrossing(direction) =>
        node :*=* scope { TLRationalCrossingSink(direction.flip) :*=* TLRationalNameNode(name) } :*=* TLRationalNameNode(name) :*=* TLRationalCrossingSource()
      case SynchronousCrossing(buffer) =>
        node :*=* scope { TLBuffer(buffer) :*=* TLNameNode(name) } :*=* TLNameNode(name)
      case CreditedCrossing(sourceDelay, sinkDelay) =>
        node :*=* scope { TLCreditedSink(sinkDelay) :*=* TLCreditedNameNode(name) } :*=* TLCreditedNameNode(name) :*=* TLCreditedSource(sourceDelay)
    }
  }
}

case class TLInwardResetCrossingHelper(name: String, scope: LazyScope, node: TLInwardNode)
    extends TLInwardCrossingHelper
{
  type HelperCrossingType = ResetCrossingType
  def apply(xing: ResetCrossingType)(implicit p: Parameters): TLInwardNode = {
    xing match {
      case _: NoResetCrossing => node
      case s: StretchedResetCrossing =>
        node :*=* scope { TLNameNode(name) } :*=* TLBlockDuringReset(s.cycles)
    }
  }
}

case class TLOutwardClockCrossingHelper(name: String, scope: LazyScope, node: TLOutwardNode)
    extends TLOutwardCrossingHelper
{
  type HelperCrossingType = ClockCrossingType
  def apply(xing: ClockCrossingType = NoCrossing)(implicit p: Parameters): TLOutwardNode = {
    xing match {
      case x: AsynchronousCrossing =>
        TLAsyncCrossingSink(x.asSinkParams) :*=* TLAsyncNameNode(name) :*=* scope { TLAsyncNameNode(name) :*=* TLAsyncCrossingSource(x.sourceSync) } :*=* node
      case RationalCrossing(direction) =>
        TLRationalCrossingSink(direction) :*=* TLRationalNameNode(name) :*=* scope { TLRationalNameNode(name) :*=* TLRationalCrossingSource() } :*=* node
      case SynchronousCrossing(buffer) =>
        TLNameNode(name) :*=* scope { TLNameNode(name) :*=* TLBuffer(buffer) } :*=* node
      case CreditedCrossing(sourceDelay, sinkDelay) =>
        TLCreditedSink(sinkDelay) :*=* TLCreditedNameNode(name) :*=* scope { TLCreditedNameNode(name) :*=* TLCreditedSource(sourceDelay) } :*=* node
    }
  }
}

case class TLOutwardResetCrossingHelper(name: String, scope: LazyScope, node: TLOutwardNode)
    extends TLOutwardCrossingHelper
{
  type HelperCrossingType = ResetCrossingType
  def apply(xing: ResetCrossingType)(implicit p: Parameters): TLOutwardNode = {
    xing match {
      case _: NoResetCrossing => node
      case s: StretchedResetCrossing =>
        TLBlockDuringReset(s.cycles) :*=* scope { TLNameNode(name) } :*=* node
    }
  }
}
