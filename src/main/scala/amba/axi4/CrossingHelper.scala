// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

case class AXI4InwardCrossingHelper(name: String, scope: LazyScope, node: AXI4InwardNode) {
  def apply(xing: ClockCrossingType = NoCrossing)(implicit p: Parameters): AXI4InwardNode = {
    xing match {
      case x: AsynchronousCrossing =>
        node :*=* scope { AXI4AsyncCrossingSink(x.asSinkParams) :*=* AXI4AsyncNameNode(name) } :*=* AXI4AsyncNameNode(name) :*=* AXI4AsyncCrossingSource(x.sourceSync)
      case RationalCrossing(direction) =>
        throw new IllegalArgumentException("AXI4 Rational crossing unimplemented")
      case SynchronousCrossing(buffer) =>
        node :*=* scope { AXI4Buffer(buffer) :*=* AXI4NameNode(name) } :*=* AXI4NameNode(name)
    }
  }
}

case class AXI4OutwardCrossingHelper(name: String, scope: LazyScope, node: AXI4OutwardNode) {
  def apply(xing: ClockCrossingType = NoCrossing)(implicit p: Parameters): AXI4OutwardNode = {
    xing match {
      case x: AsynchronousCrossing =>
        AXI4AsyncCrossingSink(x.asSinkParams) :*=* AXI4AsyncNameNode(name) :*=* scope { AXI4AsyncNameNode(name) :*=* AXI4AsyncCrossingSource(x.sourceSync) } :*=* node
      case RationalCrossing(direction) =>
        throw new IllegalArgumentException("AXI4 Rational crossing unimplemented")
      case SynchronousCrossing(buffer) =>
        AXI4NameNode(name) :*=* scope { AXI4NameNode(name) :*=* AXI4Buffer(buffer) } :*=* node
    }
  }
}
