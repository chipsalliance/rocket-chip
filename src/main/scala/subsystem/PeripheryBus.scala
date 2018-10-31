// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class BusAtomics(
  arithmetic: Boolean = true,
  buffer: BufferParams = BufferParams.default,
  widenBytes: Option[Int] = None
)

case class PeripheryBusParams(
    beatBytes: Int,
    blockBytes: Int,
    atomics: Option[BusAtomics] = Some(BusAtomics()),
    frequency: BigInt = BigInt(100000000), // 100 MHz as default bus frequency
    zeroDevice: Option[AddressSet] = None,
    errorDevice: Option[DevNullParams] = None)
  extends HasTLBusParams with HasBuiltInDeviceParams

class PeripheryBus(params: PeripheryBusParams)(implicit p: Parameters)
    extends TLBusWrapper(params, "periphery_bus")
    with CanHaveBuiltInDevices
    with CanAttachTLSlaves {

  private val node: TLNode = params.atomics.map { pa =>
    val in_xbar = LazyModule(new TLXbar)
    val out_xbar = LazyModule(new TLXbar)
    (out_xbar.node
      :*= TLFIFOFixer(TLFIFOFixer.all)
      :*= TLBuffer(pa.buffer)
      :*= (pa.widenBytes.filter(_ > beatBytes).map { w =>
          TLWidthWidget(w) :*= TLAtomicAutomata(arithmetic = pa.arithmetic) :*= TLWidthWidget(beatBytes)
        } .getOrElse { TLAtomicAutomata(arithmetic = pa.arithmetic) })
      :*= in_xbar.node)
  } .getOrElse { TLXbar() :*= TLFIFOFixer(TLFIFOFixer.all) }

  def inwardNode: TLInwardNode = node
  def outwardNode: TLOutwardNode = node

  attachBuiltInDevices(params)

  def toTile
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => TLInwardNode): NoHandle = {
    to("tile" named name) { FlipRendering { implicit p =>
      gen :*= TLWidthWidget(params.beatBytes) :*= TLBuffer(buffer) :*= outwardNode
    }}
  }
}
