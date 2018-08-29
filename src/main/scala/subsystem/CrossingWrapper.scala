// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.interrupts._


@deprecated("Only use this trait if you are confident you island will only ever be crossed to a single clock", "rocket-chip 1.3")
trait HasCrossing extends CrossesToOnlyOneClockDomain { this: LazyModule => }

/** Given a constant crossing type, define a bunch of helper methods for
  * crossing to all the procotols.
  * Note: only use this if you don't care that all signals of a given protocol
  *       type will have the same name prefixes (e.g. "tl_in_xing_*").
  */
trait CrossesToOnlyOneClockDomain extends HasClockDomainCrossing { this: LazyModule =>

  def crossing: ClockCrossingType

  def crossTLIn(n: TLInwardNode)(implicit p: Parameters): TLInwardNode = {
    val tlInXing = this.crossIn(n)
    tlInXing(crossing)
  }
  
  def crossTLOut(n: TLOutwardNode)(implicit p: Parameters): TLOutwardNode = {
    val tlOutXing = this.crossOut(n)
    tlOutXing(crossing)
  }

  def crossAXI4In(n: AXI4InwardNode)(implicit p: Parameters): AXI4InwardNode = {
    val axi4InXing = this.crossIn(n)
    axi4InXing(crossing)
  }

  def crossAXI4Out(n: AXI4OutwardNode)(implicit p: Parameters): AXI4OutwardNode = {
    val axi4OutXing = this.crossOut(n)
    axi4OutXing(crossing)
  }

  def crossIntIn(n: IntInwardNode)(implicit p: Parameters): IntInwardNode = {
    val intInXing = this.crossIn(n)
    intInXing(crossing)
  }

  def crossIntOut(n: IntOutwardNode)(implicit p: Parameters): IntOutwardNode = {
    val intOutXing = this.crossOut(n)
    intOutXing(crossing)
  }
}

/** A convenient way of creating a LazyScope with a particular uniform clock relationship */
class CrossingWrapper(val crossing: ClockCrossingType)(implicit p: Parameters) extends SimpleLazyModule with CrossesToOnlyOneClockDomain
