// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.interrupts._

trait HasCrossing extends LazyScope { this: LazyModule =>

  def crossing: ClockCrossingType
  lazy val tl_xing = new TLCrossingHelper(this, crossing)
  lazy val axi4_xing = new AXI4CrossingHelper(this, crossing)
  lazy val int_xing = new IntCrossingHelper(this, crossing)

  @deprecated("Make a CrossingHelper and call a method on it instead", "rocket-chip 1.3")
  def crossTLIn   (implicit p: Parameters): TLNode  = tl_xing.crossTLIn
  @deprecated("Make a CrossingHelper and call a method on it instead", "rocket-chip 1.3")
  def crossTLOut  (implicit p: Parameters): TLNode  = tl_xing.crossTLOut
  @deprecated("Make a CrossingHelper and call a method on it instead", "rocket-chip 1.3")
  def crossAXI4In (implicit p: Parameters): AXI4Node= axi4_xing.crossAXI4In
  @deprecated("Make a CrossingHelper and call a method on it instead", "rocket-chip 1.3")
  def crossAXI4Out(implicit p: Parameters): AXI4Node= axi4_xing.crossAXI4Out
  @deprecated("Make a CrossingHelper and call a method on it instead", "rocket-chip 1.3")
  def crossIntIn  (implicit p: Parameters): IntNode = int_xing.crossIntIn
  @deprecated("Make a CrossingHelper and call a method on it instead", "rocket-chip 1.3")
  def crossIntOut (implicit p: Parameters): IntNode = int_xing.crossIntOut
  @deprecated("Make a CrossingHelper and call a method on it instead", "rocket-chip 1.3")
  def crossIntIn (alreadyRegistered: Boolean)(implicit p: Parameters): IntNode = int_xing.crossIntIn (alreadyRegistered)
  @deprecated("Make a CrossingHelper and call a method on it instead", "rocket-chip 1.3")
  def crossIntOut(alreadyRegistered: Boolean)(implicit p: Parameters): IntNode = int_xing.crossIntOut(alreadyRegistered)
}

class CrossingWrapper(val crossing: ClockCrossingType)(implicit p: Parameters) extends SimpleLazyModule with HasCrossing
