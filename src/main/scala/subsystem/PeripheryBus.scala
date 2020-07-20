// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.prci._
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
    dtsFrequency: Option[BigInt] = None,
    zeroDevice: Option[AddressSet] = None,
    errorDevice: Option[DevNullParams] = None,
    replication: Option[ReplicatedRegion] = None,
    clockSinkWhere: Option[ClockSinkLocation] = None)
  extends HasTLBusParams
  with HasBuiltInDeviceParams
  with HasRegionReplicatorParams
  with TLBusWrapperInstantiationLike
{
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): PeripheryBus = {
    val pbus = LazyModule(new PeripheryBus(this, loc.name))

    pbus.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> pbus)

    clockSinkWhere.map { loc => context.anyLocationMap += (loc -> pbus.clockSinkNode) }
      .getOrElse { context.defaultClockLocationMap += (new ClockSinkLocation(s"${loc.name}_clocksink") -> pbus.clockSinkNode) }

    pbus
  }
}

class PeripheryBus(params: PeripheryBusParams, name: String)(implicit p: Parameters)
    extends TLBusWrapper(params, name)
{
  private val replicator = params.replication.map(r => LazyModule(new RegionReplicator(r)))
  val prefixNode = replicator.map(_.prefix)

  private val fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))
  private val node: TLNode = params.atomics.map { pa =>
    val in_xbar = LazyModule(new TLXbar)
    val out_xbar = LazyModule(new TLXbar)
    val fixer_node = replicator.map(fixer.node :*= _.node).getOrElse(fixer.node)
    (out_xbar.node
      :*= fixer_node
      :*= TLBuffer(pa.buffer)
      :*= (pa.widenBytes.filter(_ > beatBytes).map { w =>
          TLWidthWidget(w) :*= TLAtomicAutomata(arithmetic = pa.arithmetic)
        } .getOrElse { TLAtomicAutomata(arithmetic = pa.arithmetic) })
      :*= in_xbar.node)
  } .getOrElse { TLXbar() :*= fixer.node }

  def inwardNode: TLInwardNode = node
  def outwardNode: TLOutwardNode = node
  def busView: TLEdge = fixer.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(params, outwardNode)
}
