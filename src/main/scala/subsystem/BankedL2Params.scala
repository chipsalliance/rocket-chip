// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import chisel3.util.isPow2
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import CoherenceManagerWrapper._

/** Global cache coherence granularity, which applies to all caches, for now. */
case object CacheBlockBytes extends Field[Int](64)

/** L2 Broadcast Hub configuration */
case object BroadcastKey extends Field(BroadcastParams())

case class BroadcastParams(
  nTrackers:  Int     = 4,
  bufferless: Boolean = false)

/** L2 memory subsystem configuration */
case object BankedL2Key extends Field(BankedL2Params())

case class BankedL2Params(
  nBanks: Int = 1,
  coherenceManager: CoherenceManagerInstantiationFn = broadcastManager
) {
  require (isPow2(nBanks) || nBanks == 0)
}

case class CoherenceManagerWrapperParams(
    blockBytes: Int,
    beatBytes: Int,
    nBanks: Int,
    name: String)
  (val coherenceManager: CoherenceManagerInstantiationFn)
  extends HasTLBusParams 
  with TLBusWrapperInstantiationLike
{
  val dtsFrequency = None
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): CoherenceManagerWrapper = {
    val cmWrapper = LazyModule(new CoherenceManagerWrapper(this, context))
    cmWrapper.suggestName(loc.name + "_wrapper")
    cmWrapper.halt.foreach { context.anyLocationMap += loc.halt(_) }
    context.tlBusWrapperLocationMap += (loc -> cmWrapper)
    cmWrapper
  }
}

class CoherenceManagerWrapper(params: CoherenceManagerWrapperParams, context: HasTileLinkLocations)(implicit p: Parameters) extends TLBusWrapper(params, params.name) {
  val (tempIn, tempOut, halt) = params.coherenceManager(context)

  // TODO could remove temp if we could get access to .edges from InwardNodeHandle
  val viewNode = TLIdentityNode()
  def busView: TLEdge = viewNode.edges.out.head
  val inwardNode = tempIn :*= viewNode

  private def banked(node: TLOutwardNode): TLOutwardNode =
    if (params.nBanks == 0) node else { TLTempNode() :=* BankBinder(params.nBanks, params.blockBytes) :*= node }
  val outwardNode = banked(tempOut)
}

object CoherenceManagerWrapper {
  type CoherenceManagerInstantiationFn = HasTileLinkLocations => (TLInwardNode, TLOutwardNode, Option[IntOutwardNode])

  val broadcastManager: CoherenceManagerInstantiationFn = { context =>
    implicit val p = context.p
    val BroadcastParams(nTrackers, bufferless) = p(BroadcastKey)
    val bh = LazyModule(new TLBroadcast(p(CacheBlockBytes), nTrackers, bufferless))
    val ss = TLSourceShrinker(nTrackers)
    ss :*= bh.node
    (bh.node, ss, None)
  }

  val incoherentManager: CoherenceManagerInstantiationFn = { _ =>
    val node = TLNameNode("no_coherence_manager")
    (node, node, None)
  }
}
