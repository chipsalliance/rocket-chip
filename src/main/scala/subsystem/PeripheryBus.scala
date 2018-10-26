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
  frequency: BigInt = BigInt(100000000), // 100 MHz as default bus frequency
  errorDevice: Option[DevNullParams] = None
) extends HasTLBusParams

case object PeripheryBusKey extends Field[PeripheryBusParams]
case object ControlBusKey extends Field[PeripheryBusParams]

class PeripheryBus(params: PeripheryBusParams)(implicit p: Parameters)
    extends TLBusWrapper(params, "periphery_bus")
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

  def toTile
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => TLInwardNode): NoHandle = {
    to("tile" named name) { FlipRendering { implicit p =>
      gen :*= TLWidthWidget(params.beatBytes) :*= TLBuffer(buffer) :*= outwardNode
    }}
  }
}
