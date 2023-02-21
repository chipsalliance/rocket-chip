// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ResetCrossingType, NoResetCrossing, StretchedResetCrossing}

trait AXI4OutwardCrossingHelper {
  type HelperCrossingType <: CrossingType
  def apply(xing: HelperCrossingType)(implicit p: Parameters): AXI4OutwardNode
}

trait AXI4InwardCrossingHelper {
  type HelperCrossingType <: CrossingType
  def apply(xing: HelperCrossingType)(implicit p: Parameters): AXI4InwardNode
}

case class AXI4InwardClockCrossingHelper(name: String, scope: LazyScope, node: AXI4InwardNode)
  extends AXI4InwardCrossingHelper
{
  type HelperCrossingType = ClockCrossingType
  def apply(xing: ClockCrossingType = NoCrossing)(implicit p: Parameters): AXI4InwardNode = {
    xing match {
      case x: AsynchronousCrossing =>
        node :*=* scope { AXI4AsyncCrossingSink(x.asSinkParams) :*=* AXI4AsyncNameNode(name) } :*=* AXI4AsyncNameNode(name) :*=* AXI4AsyncCrossingSource(x.sourceSync)
      case RationalCrossing(direction) =>
        throw new IllegalArgumentException("AXI4 Rational crossing unimplemented")
      case SynchronousCrossing(buffer) =>
        node :*=* scope { AXI4Buffer(buffer) :*=* AXI4NameNode(name) } :*=* AXI4NameNode(name)
      case CreditedCrossing(sourceDelay, sinkDelay) =>
        node :*=* scope { AXI4CreditedSink(sinkDelay) :*=* AXI4CreditedNameNode(name) } :*=* AXI4CreditedNameNode(name) :*=* AXI4CreditedSource(sourceDelay)
    }
  }
}

case class AXI4InwardResetCrossingHelper(name: String, scope: LazyScope, node: AXI4InwardNode)
  extends AXI4InwardCrossingHelper
{
  type HelperCrossingType = ResetCrossingType
  def apply(xing: ResetCrossingType)(implicit p: Parameters): AXI4InwardNode = {
    xing match {
      case _: NoResetCrossing => node
      case _: StretchedResetCrossing => throw new Exception("No ResetStretcher adapter for AXI$")
    }
  }
}

case class AXI4OutwardClockCrossingHelper(name: String, scope: LazyScope, node: AXI4OutwardNode)
  extends AXI4OutwardCrossingHelper
{
  type HelperCrossingType = ClockCrossingType
  def apply(xing: ClockCrossingType = NoCrossing)(implicit p: Parameters): AXI4OutwardNode = {
    xing match {
      case x: AsynchronousCrossing =>
        AXI4AsyncCrossingSink(x.asSinkParams) :*=* AXI4AsyncNameNode(name) :*=* scope { AXI4AsyncNameNode(name) :*=* AXI4AsyncCrossingSource(x.sourceSync) } :*=* node
      case RationalCrossing(direction) =>
        throw new IllegalArgumentException("AXI4 Rational crossing unimplemented")
      case SynchronousCrossing(buffer) =>
        AXI4NameNode(name) :*=* scope { AXI4NameNode(name) :*=* AXI4Buffer(buffer) } :*=* node
      case CreditedCrossing(sourceDelay, sinkDelay) =>
        AXI4CreditedSink(sinkDelay) :*=* AXI4CreditedNameNode(name) :*=* scope { AXI4CreditedNameNode(name) :*=* AXI4CreditedSource(sourceDelay) } :*=* node
    }
  }
}

case class AXI4OutwardResetCrossingHelper(name: String, scope: LazyScope, node: AXI4OutwardNode)
  extends AXI4OutwardCrossingHelper
{
  type HelperCrossingType = ResetCrossingType
  def apply(xing: ResetCrossingType)(implicit p: Parameters): AXI4OutwardNode = {
    xing match {
      case _: NoResetCrossing => node
      case _: StretchedResetCrossing => throw new Exception("No ResetStretcher adapter for AXI$")
    }
  }
}
