// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

// TODO: applies to all caches, for now
case object CacheBlockBytes extends Field[Int]

/** L2 Broadcast Hub configuration */
case class BroadcastParams(
  nTrackers:  Int     = 4,
  bufferless: Boolean = false)
case object BroadcastParams extends Field[BroadcastParams]

/** L2 memory subsystem configuration */
case class BankedL2Params(
  nMemoryChannels:  Int = 1,
  nBanksPerChannel: Int = 1,
  coherenceManager: (Parameters, HasMemoryBus) => (TLInwardNode, TLOutwardNode) = { case (q, _) =>
    implicit val p = q
    val MemoryBusParams(_, blockBytes, _, _) = p(MemoryBusParams)
    val BroadcastParams(nTrackers, bufferless) = p(BroadcastParams)
    val bh = LazyModule(new TLBroadcast(blockBytes, nTrackers, bufferless))
    (bh.node, bh.node)
  }) {
  val nBanks = nMemoryChannels*nBanksPerChannel
}
case object BankedL2Params extends Field[BankedL2Params]

/** Parameterization of the memory-side bus created for each memory channel */
case class MemoryBusParams(
  beatBytes: Int,
  blockBytes: Int,
  masterBuffering: BufferParams = BufferParams.none,
  slaveBuffering: BufferParams = BufferParams.none
) extends TLBusParams

case object MemoryBusParams extends Field[MemoryBusParams]

/** Wrapper for creating TL nodes from a bus connected to the back of each mem channel */
class MemoryBus(params: MemoryBusParams)(implicit p: Parameters) extends TLBusWrapper(params, "MemoryBus")(p) {
  def fromCoherenceManager: TLInwardNode = inwardBufNode
  def toDRAMController: TLOutwardNode = outwardBufNode
  def toVariableWidthSlave: TLOutwardNode = outwardFragNode
}

trait HasMemoryBus extends HasSystemBus with HasPeripheryBus with HasInterruptBus {
  private val mbusParams = p(MemoryBusParams)
  private val MemoryBusParams(beatBytes, blockBytes, _, _) = mbusParams
  private val l2Params = p(BankedL2Params)
  val BankedL2Params(nMemoryChannels, nBanksPerChannel, coherenceManager) = l2Params
  val nBanks = l2Params.nBanks
  val cacheBlockBytes = blockBytes

  require (isPow2(nMemoryChannels) || nMemoryChannels == 0)
  require (isPow2(nBanksPerChannel))
  require (isPow2(blockBytes))

  private val (in, out) = coherenceManager(p, this)
  private val mask = ~BigInt((nBanks-1) * blockBytes)
  val memBuses = Seq.tabulate(nMemoryChannels) { channel =>
    val mbus = new MemoryBus(mbusParams)
    for (bank <- 0 until nBanksPerChannel) {
      val offset = (bank * nMemoryChannels) + channel
      in := sbus.toMemoryBus
      mbus.fromCoherenceManager := TLFilter(TLFilter.Mmask(AddressSet(offset * blockBytes, mask)))(out)
    }
    mbus
  }
}
