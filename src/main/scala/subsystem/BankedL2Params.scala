// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import chisel3.util.isPow2
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

// TODO: applies to all caches, for now
case object CacheBlockBytes extends Field[Int](64)

case object BroadcastKey extends Field(BroadcastParams())
case object BankedL2Key extends Field(BankedL2Params())

/** L2 Broadcast Hub configuration */
case class BroadcastParams(
  nTrackers:  Int     = 4,
  bufferless: Boolean = false)

/** L2 memory subsystem configuration */
case class BankedL2Params(
  nBanks: Int = 1,
  coherenceManager: CoherenceManagerWrapper.CoherenceManagerInstantiationFn = { subsystem =>
    implicit val p = subsystem.p
    val BroadcastParams(nTrackers, bufferless) = p(BroadcastKey)
    val bh = LazyModule(new TLBroadcast(p(CacheBlockBytes), nTrackers, bufferless))
    val ss = TLSourceShrinker(nTrackers)
    ss :*= bh.node
    (bh.node, ss, None)
  }
) {
  require (isPow2(nBanks) || nBanks == 0)
}


case class CoherenceManagerWrapperParams(blockBytes: Int, beatBytes: Int, name: String)
  (val coherenceManager: CoherenceManagerWrapper.CoherenceManagerInstantiationFn)
  extends HasTLBusParams 
  with TLBusWrapperInstantiationLike
{
  val dtsFrequency = None
  def instantiate(context: HasLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): CoherenceManagerWrapper = {
    val cmWrapper = LazyModule(new CoherenceManagerWrapper(this, context))
    context.updateDynamic(loc.name+"Halt")(cmWrapper.halt)
    context.tlBusWrapperLocationMap.updateDynamic(loc.name)(cmWrapper)
    cmWrapper
  }
}

object CoherenceManagerWrapper {
  type CoherenceManagerInstantiationFn = HasLocations => (TLInwardNode, TLOutwardNode, Option[IntOutwardNode])
}

class CoherenceManagerWrapper(params: CoherenceManagerWrapperParams, context: HasLocations)(implicit p: Parameters) extends TLBusWrapper(params, params.name) {
  val (temp, outwardNode, halt) = params.coherenceManager(context)
  // TODO could remove temp if we could get access to .edges from InwardNodeHandle
  val viewNode = TLIdentityNode()
  val inwardNode = temp :*= viewNode
  def busView: TLEdge = viewNode.edges.out.head
}
