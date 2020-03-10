// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class SystemBusParams(
    beatBytes: Int,
    blockBytes: Int,
    policy: TLArbiter.Policy = TLArbiter.roundRobin,
    dtsFrequency: Option[BigInt] = None,
    zeroDevice: Option[AddressSet] = None,
    errorDevice: Option[DevNullParams] = None)
  extends HasTLBusParams
  with HasBuiltInDeviceParams
  with TLBusWrapperInstantiationLike
{
  def instantiate(context: HasLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): SystemBus = {
    val sbus = LazyModule(new SystemBus(this, loc.name))
    context.tlBusWrapperLocationMap.updateDynamic(loc.name)(sbus)
    sbus
  }
}

class SystemBus(params: SystemBusParams, name: String = "system_bus")(implicit p: Parameters)
    extends TLBusWrapper(params, name)
    with CanHaveBuiltInDevices
    with CanAttachTLSlaves
    with CanAttachTLMasters
{
  private val system_bus_xbar = LazyModule(new TLXbar(policy = params.policy))
  def inwardNode: TLInwardNode = system_bus_xbar.node
  def outwardNode: TLOutwardNode = system_bus_xbar.node
  def busView: TLEdge = system_bus_xbar.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(params, outwardNode)

  def fromTile
      (name: Option[String], buffer: BufferParams = BufferParams.none, cork: Option[Boolean] = None)
      (gen: => TLOutwardNode): NoHandle = {
    from("tile" named name) {
      inwardNode :=* TLBuffer(buffer) :=* TLFIFOFixer(TLFIFOFixer.allVolatile) :=* gen
    }
  }
}
