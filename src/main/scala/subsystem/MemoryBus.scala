// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
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
  coherenceManager: BaseSubsystem => (TLInwardNode, TLOutwardNode, Option[IntOutwardNode]) = { subsystem =>
    implicit val p = subsystem.p
    val BroadcastParams(nTrackers, bufferless) = p(BroadcastKey)
    val bh = LazyModule(new TLBroadcast(subsystem.mbus.blockBytes, nTrackers, bufferless))
    val ww = LazyModule(new TLWidthWidget(subsystem.sbus.beatBytes))
    val ss = TLSourceShrinker(nTrackers)
    ww.node :*= bh.node
    ss :*= ww.node
    (bh.node, ss, None)
  }) {
  require (isPow2(nBanks) || nBanks == 0)
}

/** Parameterization of the memory-side bus created for each memory channel */
case class MemoryBusParams(
  beatBytes: Int,
  blockBytes: Int,
  zeroDevice: Option[AddressSet] = None,
  errorDevice: Option[DevNullParams] = None,
  replicatorMask: BigInt = 0)
  extends HasTLBusParams
  with HasBuiltInDeviceParams
  with HasRegionReplicatorParams

/** Wrapper for creating TL nodes from a bus connected to the back of each mem channel */
class MemoryBus(params: MemoryBusParams)(implicit p: Parameters)
    extends TLBusWrapper(params, "memory_bus")(p)
    with CanHaveBuiltInDevices
    with CanAttachTLSlaves {

  private val xbar = LazyModule(new TLXbar).suggestName(busName + "_xbar")
  def inwardNode: TLInwardNode =
    if (params.replicatorMask == 0) xbar.node else { xbar.node :*= RegionReplicator(params.replicatorMask) }
  def outwardNode: TLOutwardNode = ProbePicker() :*= xbar.node
  def busView: TLEdge = xbar.node.edges.in.head
  attachBuiltInDevices(params)

  def toDRAMController[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[ TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle, D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("memory_controller" named name) { gen := TLWidthWidget(params.beatBytes) := TLBuffer(buffer) := outwardNode }
  }
}
