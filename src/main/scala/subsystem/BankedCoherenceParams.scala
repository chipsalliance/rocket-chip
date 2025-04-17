// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.devices.tilelink.BuiltInDevices
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.interrupts.IntOutwardNode
import freechips.rocketchip.tilelink.{
  TLBroadcast, HasTLBusParams, BroadcastFilter, TLBusWrapper, TLBusWrapperInstantiationLike,
  TLJbar, TLEdge, TLOutwardNode, TLTempNode, TLInwardNode, BankBinder, TLBroadcastParams,
  TLBroadcastControlParams, TLBuffer, TLFragmenter, TLNameNode
}
import freechips.rocketchip.util.Location

import CoherenceManagerWrapper._

/** Global cache coherence granularity, which applies to all caches, for now. */
case object CacheBlockBytes extends Field[Int](64)

/** LLC Broadcast Hub configuration */
case object BroadcastKey extends Field(BroadcastParams())

case class BroadcastParams(
  nTrackers:      Int     = 4,
  bufferless:     Boolean = false,
  controlAddress: Option[BigInt] = None,
  filterFactory:  TLBroadcast.ProbeFilterFactory = BroadcastFilter.factory)

/** Coherence manager configuration */
case object SubsystemBankedCoherenceKey extends Field(BankedCoherenceParams())
case class ClusterBankedCoherenceKey(clusterId: Int) extends Field(BankedCoherenceParams(nBanks=0))

case class BankedCoherenceParams(
  nBanks: Int = 1,
  coherenceManager: CoherenceManagerInstantiationFn = broadcastManager
) {
  require (isPow2(nBanks) || nBanks == 0)
}

case class CoherenceManagerWrapperParams(
    blockBytes: Int,
    beatBytes: Int,
    nBanks: Int,
    name: String,
    dtsFrequency: Option[BigInt] = None)
  (val coherenceManager: CoherenceManagerInstantiationFn)
  extends HasTLBusParams 
  with TLBusWrapperInstantiationLike
{
  def instantiate(context: HasTileLinkLocations with HasPRCILocations with LazyModule, loc: Location[TLBusWrapper])(implicit p: Parameters): CoherenceManagerWrapper = {
    val cmWrapper = LazyModule(new CoherenceManagerWrapper(this, context))
    cmWrapper.suggestName(loc.name + "_wrapper")
    cmWrapper.halt.foreach { context.anyLocationMap += loc.halt(_) }
    context.tlBusWrapperLocationMap += (loc -> cmWrapper)
    cmWrapper
  }
}

class CoherenceManagerWrapper(params: CoherenceManagerWrapperParams, context: HasTileLinkLocations with HasPRCILocations with LazyModule)(implicit p: Parameters) extends TLBusWrapper(params, params.name) {
  val (tempIn, tempOut, halt) = params.coherenceManager(context)

  private val coherent_jbar = LazyModule(new TLJbar)
  def busView: TLEdge = coherent_jbar.node.edges.out.head
  val inwardNode = tempIn :*= coherent_jbar.node
  val builtInDevices = BuiltInDevices.none
  val prefixNode = None

  private def banked(node: TLOutwardNode): TLOutwardNode =
    if (params.nBanks == 0) node else { TLTempNode() :=* BankBinder(params.nBanks, params.blockBytes) :*= node }
  val outwardNode = banked(tempOut)
}

object CoherenceManagerWrapper {
  type CoherenceManagerInstantiationFn = HasTileLinkLocations with HasPRCILocations with LazyModule => (TLInwardNode, TLOutwardNode, Option[IntOutwardNode])

  def broadcastManagerFn(
    name: String,
    location: HierarchicalLocation,
    controlPortsSlaveWhere: TLBusWrapperLocation
  ): CoherenceManagerInstantiationFn = { context =>
    implicit val p = context.p
    val cbus = context.locateTLBusWrapper(controlPortsSlaveWhere)

    val BroadcastParams(nTrackers, bufferless, controlAddress, filterFactory) = p(BroadcastKey)
    val bh = LazyModule(new TLBroadcast(TLBroadcastParams(
      lineBytes     = p(CacheBlockBytes),
      numTrackers   = nTrackers,
      bufferless    = bufferless,
      control       = controlAddress.map(x => TLBroadcastControlParams(AddressSet(x, 0xfff), cbus.beatBytes)),
      filterFactory = filterFactory)))
    bh.suggestName(name)

    bh.controlNode.foreach { _ := cbus.coupleTo(s"${name}_ctrl") { TLBuffer(1) := TLFragmenter(cbus) := _ } }
    bh.intNode.foreach { context.ibus.fromSync := _ }

    (bh.node, bh.node, None)
  }

  val broadcastManager = broadcastManagerFn("broadcast", InSystem, CBUS)

  val incoherentManager: CoherenceManagerInstantiationFn = { _ =>
    val node = TLNameNode("no_coherence_manager")
    (node, node, None)
  }
}
