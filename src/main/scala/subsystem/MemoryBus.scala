// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.devices.tilelink.{DevNullParams, TLError, TLZero}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

// TODO: applies to all caches, for now
case object CacheBlockBytes extends Field[Int](64)

/** L2 Broadcast Hub configuration */
case class BroadcastParams(
  nTrackers:  Int     = 4,
  bufferless: Boolean = false)

case object BroadcastKey extends Field(BroadcastParams())

/** L2 memory subsystem configuration */
case class BankedL2Params(
  nBanks: Int = 1,
  coherenceManager: BaseSubsystem => (TLInwardNode, TLOutwardNode, () => Option[Bool]) = { subsystem =>
    implicit val p = subsystem.p
    val BroadcastParams(nTrackers, bufferless) = p(BroadcastKey)
    val bh = LazyModule(new TLBroadcast(subsystem.memBusBlockBytes, nTrackers, bufferless))
    val ww = LazyModule(new TLWidthWidget(subsystem.sbus.beatBytes))
    ww.node :*= bh.node
    (bh.node, ww.node, () => None)
  }) {
}

case object BankedL2Key extends Field(BankedL2Params())

/** Parameterization of the memory-side bus created for each memory channel */
case class MemoryBusParams(
  beatBytes: Int,
  blockBytes: Int,
  zeroDevice: Option[AddressSet] = None,
  errorDevice: Option[DevNullParams] = None) extends HasTLBusParams

case object MemoryBusKey extends Field[MemoryBusParams]

/** Wrapper for creating TL nodes from a bus connected to the back of each mem channel */
class MemoryBus(params: MemoryBusParams)(implicit p: Parameters)
    extends TLBusWrapper(params, "memory_bus")(p)
    with CanAttachTLSlaves {

  private val xbar = LazyModule(new TLXbar).suggestName(busName + "_xbar")
  def inwardNode: TLInwardNode = xbar.node
  def outwardNode: TLOutwardNode = ProbePicker() :*= xbar.node

  params.zeroDevice.foreach { addr => LazyScope("wrapped_zero_device") {
    val zero = LazyModule(new TLZero(
      address = addr,
      beatBytes = params.beatBytes))
    zero.node := TLFragmenter(params.beatBytes, params.blockBytes) := TLBuffer() := outwardNode
  }}

  params.errorDevice.foreach { dnp => LazyScope("wrapped_error_device") {
    val error = LazyModule(new TLError(
      params = dnp,
      beatBytes = params.beatBytes))
    error.node := TLBuffer() := outwardNode
  }}

  def toDRAMController[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[ TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle, D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("memory_controller" named name) { gen := TLBuffer(buffer) := outwardNode }
  }
}
