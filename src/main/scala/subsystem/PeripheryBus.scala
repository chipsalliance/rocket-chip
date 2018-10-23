// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink.{DevNullParams, TLError}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class BusAtomics(
  arithmetic: Boolean = true,
  buffer: BufferParams = BufferParams.default
)

case class PeripheryBusParams(
  beatBytes: Int,
  blockBytes: Int,
  atomics: Option[BusAtomics] = Some(BusAtomics()),
  busXType: ClockCrossingType = SynchronousCrossing(), // relative to sbus
  frequency: BigInt = BigInt(100000000), // 100 MHz as default bus frequency
  errorDevice: Option[DevNullParams] = None
) extends HasTLBusParams

case object PeripheryBusKey extends Field[PeripheryBusParams]
case object ControlBusKey extends Field[PeripheryBusParams]

class PeripheryBus(params: PeripheryBusParams)(implicit p: Parameters)
    extends TLBusWrapper(params, "periphery_bus")
    with HasClockDomainCrossing
    with CanAttachTLSlaves {

  private val in_xbar = LazyModule(new TLXbar)
  private val out_xbar = LazyModule(new TLXbar)
  private val atomics = params.atomics.map { pa =>
    TLBuffer(pa.buffer) :*= TLAtomicAutomata(arithmetic = pa.arithmetic)
  }.getOrElse(TLNameNode("no_atomics"))

  (out_xbar.node
    :*= TLFIFOFixer(TLFIFOFixer.all)
    :*= atomics
    :*= in_xbar.node)

  def inwardNode: TLInwardNode = in_xbar.node
  def outwardNode: TLOutwardNode = out_xbar.node

  params.errorDevice.foreach { dnp => LazyScope("wrapped_error_device") {
    val error = LazyModule(new TLError(params = dnp, beatBytes = params.beatBytes))
    error.node := outwardNode
  }}

  def crossFromSystemBus(gen: (=> TLInwardNode) => NoHandle) {
    from("sbus") {
      val from_sbus = this.crossIn(inwardNode)
      gen(from_sbus(params.busXType))
    }
  }

  def crossFromControlBus(gen: (=> TLInwardNode) => NoHandle) {
    from("cbus") {
      val from_cbus = this.crossIn(inwardNode)
      gen(from_cbus(params.busXType))
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
      (gen: => TLInwardNode): NoHandle = {
    to("tile" named name) { FlipRendering { implicit p =>
      gen :*= TLWidthWidget(params.beatBytes) :*= TLBuffer(buffer) :*= outwardNode
    }}
  }

  def toSlaveBus(name: String): (=> TLInwardNode) => NoHandle =
    gen => to(s"bus_named_$name") {
      (gen
        :*= TLWidthWidget(params.beatBytes)
        :*= outwardNode)
    }
}
