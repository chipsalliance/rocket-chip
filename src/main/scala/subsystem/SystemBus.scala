// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.devices.tilelink.{
  BuiltInDevices, BuiltInZeroDeviceParams, BuiltInErrorDeviceParams, HasBuiltInDeviceParams
}
import freechips.rocketchip.tilelink.{
  TLArbiter, RegionReplicator, ReplicatedRegion, HasTLBusParams, TLBusWrapper,
  TLBusWrapperInstantiationLike, TLXbar, TLEdge, TLInwardNode, TLOutwardNode,
  TLFIFOFixer, TLTempNode
}
import freechips.rocketchip.util.Location

case class SystemBusParams(
    beatBytes: Int,
    blockBytes: Int,
    policy: TLArbiter.Policy = TLArbiter.roundRobin,
    dtsFrequency: Option[BigInt] = None,
    zeroDevice: Option[BuiltInZeroDeviceParams] = None,
    errorDevice: Option[BuiltInErrorDeviceParams] = None,
    replication: Option[ReplicatedRegion] = None)
  extends HasTLBusParams
  with HasBuiltInDeviceParams
  with TLBusWrapperInstantiationLike
{
  def instantiate(context: HasTileLinkLocations with HasPRCILocations with LazyModule, loc: Location[TLBusWrapper])(implicit p: Parameters): SystemBus = {
    val sbus = LazyModule(new SystemBus(this, loc.name))
    sbus.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> sbus)
    sbus
  }
}

class SystemBus(params: SystemBusParams, name: String = "system_bus")(implicit p: Parameters)
    extends TLBusWrapper(params, name)
{
  private val replicator = params.replication.map(r => LazyModule(new RegionReplicator(r)))
  val prefixNode = replicator.map { r =>
    r.prefix := addressPrefixNexusNode
    addressPrefixNexusNode
  }

  private val system_bus_xbar = LazyModule(new TLXbar(policy = params.policy, nameSuffix = Some(name)))
  val inwardNode: TLInwardNode = system_bus_xbar.node :=* TLFIFOFixer(TLFIFOFixer.allVolatile) :=* replicator.map(_.node).getOrElse(TLTempNode())
  val outwardNode: TLOutwardNode = system_bus_xbar.node
  def busView: TLEdge = system_bus_xbar.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(params, outwardNode)
}
