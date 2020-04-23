// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

/** Parameterization of the memory-side bus created for each memory channel */
case class MemoryBusParams(
  beatBytes: Int,
  blockBytes: Int,
  dtsFrequency: Option[BigInt] = None,
  zeroDevice: Option[AddressSet] = None,
  errorDevice: Option[DevNullParams] = None,
  replicatorMask: BigInt = 0)
  extends HasTLBusParams
  with HasBuiltInDeviceParams
  with HasRegionReplicatorParams
  with TLBusWrapperInstantiationLike
{
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): MemoryBus = {
    val mbus = LazyModule(new MemoryBus(this, loc.name))
    mbus.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> mbus)
    mbus
  }
}

/** Wrapper for creating TL nodes from a bus connected to the back of each mem channel */
class MemoryBus(params: MemoryBusParams, name: String = "memory_bus")(implicit p: Parameters)
    extends TLBusWrapper(params, name)(p)
{
  private val xbar = LazyModule(new TLXbar).suggestName(busName + "_xbar")
  private def replicate(node: TLInwardNode): TLInwardNode =
    if (params.replicatorMask == 0) node else { node :*=* RegionReplicator(params.replicatorMask) }
  val inwardNode: TLInwardNode = replicate(xbar.node)
  val outwardNode: TLOutwardNode = ProbePicker() :*= xbar.node
  def busView: TLEdge = xbar.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(params, outwardNode)
}
