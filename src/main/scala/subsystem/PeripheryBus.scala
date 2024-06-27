// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.devices.tilelink.{BuiltInZeroDeviceParams, BuiltInErrorDeviceParams, HasBuiltInDeviceParams, BuiltInDevices}
import freechips.rocketchip.diplomacy.BufferParams
import freechips.rocketchip.tilelink.{
  RegionReplicator, ReplicatedRegion, HasTLBusParams, HasRegionReplicatorParams, TLBusWrapper,
  TLBusWrapperInstantiationLike, TLFIFOFixer, TLNode, TLXbar, TLInwardNode, TLOutwardNode,
  TLBuffer, TLWidthWidget, TLAtomicAutomata, TLEdge
}
import freechips.rocketchip.util.Location

case class BusAtomics(
  arithmetic: Boolean = true,
  buffer: BufferParams = BufferParams.default,
  widenBytes: Option[Int] = None
)

case class PeripheryBusParams(
    beatBytes: Int,
    blockBytes: Int,
    atomics: Option[BusAtomics] = Some(BusAtomics()),
    dtsFrequency: Option[BigInt] = None,
    zeroDevice: Option[BuiltInZeroDeviceParams] = None,
    errorDevice: Option[BuiltInErrorDeviceParams] = None,
    replication: Option[ReplicatedRegion] = None)
  extends HasTLBusParams
  with HasBuiltInDeviceParams
  with HasRegionReplicatorParams
  with TLBusWrapperInstantiationLike
{
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): PeripheryBus = {
    val pbus = LazyModule(new PeripheryBus(this, loc.name))
    pbus.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> pbus)
    pbus
  }
}

class PeripheryBus(params: PeripheryBusParams, name: String)(implicit p: Parameters)
    extends TLBusWrapper(params, name)
{
  override lazy val desiredName = s"PeripheryBus_$name"
  private val replicator = params.replication.map(r => LazyModule(new RegionReplicator(r)))
  val prefixNode = replicator.map { r =>
    r.prefix := addressPrefixNexusNode
    addressPrefixNexusNode
  }

  private val fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))
  private val node: TLNode = params.atomics.map { pa =>
    val in_xbar = LazyModule(new TLXbar(nameSuffix = Some(s"${name}_in")))
    val out_xbar = LazyModule(new TLXbar(nameSuffix = Some(s"${name}_out")))
    val fixer_node = replicator.map(fixer.node :*= _.node).getOrElse(fixer.node)
    (out_xbar.node
      :*= fixer_node
      :*= TLBuffer(pa.buffer)
      :*= (pa.widenBytes.filter(_ > beatBytes).map { w =>
          TLWidthWidget(w) :*= TLAtomicAutomata(arithmetic = pa.arithmetic, nameSuffix = Some(name))
        } .getOrElse { TLAtomicAutomata(arithmetic = pa.arithmetic, nameSuffix = Some(name)) })
      :*= in_xbar.node)
  } .getOrElse { TLXbar() :*= fixer.node }

  def inwardNode: TLInwardNode = node
  def outwardNode: TLOutwardNode = node
  def busView: TLEdge = fixer.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(params, outwardNode)
}
