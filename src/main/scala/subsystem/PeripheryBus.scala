// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class PeripheryBusParams(
  beatBytes: Int,
  blockBytes: Int,
  arithmeticAtomics: Boolean = true,
  bufferAtomics: BufferParams = BufferParams.default,
  sbusCrossingType: SubsystemClockCrossing = SynchronousCrossing(), // relative to sbus
  frequency: BigInt = BigInt(100000000) // 100 MHz as default bus frequency
) extends HasTLBusParams

case object PeripheryBusKey extends Field[PeripheryBusParams]

class PeripheryBus(params: PeripheryBusParams)(implicit p: Parameters)
    extends TLBusWrapper(params, "periphery_bus")
    with CanAttachTLSlaves
    with HasTLXbarPhy {

  val sbusXing = new CrossingHelper(this, params.sbusCrossingType)

  def fromSystemBus(gen: => TLOutwardNode) {
    from("sbus") {
      (inwardNode
        :*= TLBuffer(params.bufferAtomics)
        :*= TLAtomicAutomata(arithmetic = params.arithmeticAtomics)
        :*= gen)
    }
  }

  def fromOtherMaster[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[D,U,E,B,TLClientPortParameters,TLManagerPortParameters,TLEdgeOut,TLBundle] =
        TLNameNode(name)): InwardNodeHandle[D,U,E,B] = {
    from("master" named name) {
      inwardNode :=* TLBuffer(buffer) :=* gen
    }
  }

  def toTile
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => TLNode): TLOutwardNode = {
    to("tile" named name) { FlipRendering { implicit p =>
      gen :*= TLBuffer(buffer) :*= outwardNode
    }}
  }
}
