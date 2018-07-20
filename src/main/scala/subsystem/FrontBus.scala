// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class FrontBusParams(
  beatBytes: Int,
  blockBytes: Int,
  sbusCrossing: ClockCrossingType = SynchronousCrossing(),
  sbusBuffer: BufferParams = BufferParams.none) extends HasTLBusParams

case object FrontBusKey extends Field[FrontBusParams]

class FrontBus(params: FrontBusParams)(implicit p: Parameters)
    extends TLBusWrapper(params, "front_bus")
    with CanAttachTLMasters
    with HasTLXbarPhy {

  def fromCoherentChip(gen: => TLNode): TLInwardNode = {
    from("coherent_subsystem") { inwardNode :=* gen }
  }

  protected val sbusXing = new TLCrossingHelper(this)
  def crossToSystemBus(gen: (=> TLNode) => TLInwardNode) {
    to("sbus") {
      (gen(sbusXing.crossTLOut(params.sbusCrossing))
        :=* TLBuffer(params.sbusBuffer)
        :=* outwardNode)
    }
  }
}
