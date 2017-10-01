// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
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
  nMemoryChannels:  Int = 1,
  nBanksPerChannel: Int = 1,
  coherenceManager: HasMemoryBus => (TLInwardNode, TLOutwardNode, () => Option[Bool]) = { coreplex =>
    implicit val p = coreplex.p
    val BroadcastParams(nTrackers, bufferless) = p(BroadcastKey)
    val bh = LazyModule(new TLBroadcast(coreplex.memBusBlockBytes, nTrackers, bufferless))
    val ww = LazyModule(new TLWidthWidget(coreplex.sbusBeatBytes))
    ww.node :*= bh.node
    (bh.node, ww.node, () => None)
  }) {
  val nBanks = nMemoryChannels*nBanksPerChannel
}

case object BankedL2Key extends Field(BankedL2Params())

/** Parameterization of the memory-side bus created for each memory channel */
case class MemoryBusParams(
  beatBytes: Int,
  blockBytes: Int,
  masterBuffering: BufferParams = BufferParams.none,
  slaveBuffering: BufferParams = BufferParams.none
) extends TLBusParams

case object MemoryBusKey extends Field[MemoryBusParams]

/** Wrapper for creating TL nodes from a bus connected to the back of each mem channel */
class MemoryBus(params: MemoryBusParams)(implicit p: Parameters) extends TLBusWrapper(params, "MemoryBus")(p) {
  def fromCoherenceManager: TLInwardNode = inwardBufNode
  def toDRAMController: TLOutwardNode = outwardBufNode
  def toVariableWidthSlave: TLOutwardNode = outwardFragNode
}

trait HasMemoryBus extends HasSystemBus with HasPeripheryBus with HasInterruptBus {
  private val mbusParams = p(MemoryBusKey)
  private val l2Params = p(BankedL2Key)
  val MemoryBusParams(memBusBeatBytes, memBusBlockBytes, _, _) = mbusParams
  val BankedL2Params(nMemoryChannels, nBanksPerChannel, coherenceManager) = l2Params
  val nBanks = l2Params.nBanks
  val cacheBlockBytes = memBusBlockBytes
  private val (in, out, halt) = coherenceManager(this)
  def memBusCanCauseHalt: () => Option[Bool] = halt

  require (isPow2(nMemoryChannels) || nMemoryChannels == 0)
  require (isPow2(nBanksPerChannel))
  require (isPow2(memBusBlockBytes))

  private val mask = ~BigInt((nBanks-1) * memBusBlockBytes)
  val memBuses = Seq.tabulate(nMemoryChannels) { channel =>
    val mbus = LazyModule(new MemoryBus(mbusParams))
    for (bank <- 0 until nBanksPerChannel) {
      val offset = (bank * nMemoryChannels) + channel
      ForceFanout(a = true) { implicit p => in := sbus.toMemoryBus }
      mbus.fromCoherenceManager := TLFilter(TLFilter.Mmask(AddressSet(offset * memBusBlockBytes, mask)))(out)
    }
    mbus
  }
}
