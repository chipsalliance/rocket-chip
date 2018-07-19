// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.interrupts._

@deprecated("HasCrossing has been replaced by protocol-specific CrossingHelpers", "rocket-chip 1.3")
trait HasCrossing extends LazyScope { this: LazyModule =>

  def crossing: ClockCrossingType
  protected lazy val tl_xing = new TLCrossingHelper(this, crossing)
  protected lazy val axi4_xing = new AXI4CrossingHelper(this, crossing)
  protected lazy val int_xing = new IntCrossingHelper(this, crossing)

  def crossTLIn   (implicit p: Parameters): TLNode  = tl_xing.crossTLIn
  def crossTLOut  (implicit p: Parameters): TLNode  = tl_xing.crossTLOut
  def crossAXI4In (implicit p: Parameters): AXI4Node= axi4_xing.crossAXI4In
  def crossAXI4Out(implicit p: Parameters): AXI4Node= axi4_xing.crossAXI4Out
  def crossIntIn  (implicit p: Parameters): IntNode = int_xing.crossIntIn
  def crossIntOut (implicit p: Parameters): IntNode = int_xing.crossIntOut
  def crossIntIn (alreadyRegistered: Boolean)(implicit p: Parameters): IntNode = int_xing.crossIntIn (alreadyRegistered)
  def crossIntOut(alreadyRegistered: Boolean)(implicit p: Parameters): IntNode = int_xing.crossIntOut(alreadyRegistered)
}

@deprecated("CrossingWrapper has been replaced by protocol-specific CrossingHelpers", "rocket-chip 1.3")
class CrossingWrapper(val crossing: ClockCrossingType)(implicit p: Parameters) extends SimpleLazyModule with HasCrossing
