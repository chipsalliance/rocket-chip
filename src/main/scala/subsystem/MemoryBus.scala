// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

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
  coherenceManager: HasMemoryBus => (TLInwardNode, TLOutwardNode, () => Option[Bool]) = { subsystem =>
    implicit val p = subsystem.p
    val BroadcastParams(nTrackers, bufferless) = p(BroadcastKey)
    val bh = LazyModule(new TLBroadcast(subsystem.memBusBlockBytes, nTrackers, bufferless))
    val ww = LazyModule(new TLWidthWidget(subsystem.sbusBeatBytes))
    ww.node :*= bh.node
    (bh.node, ww.node, () => None)
  }) {
  val nBanks = nMemoryChannels*nBanksPerChannel
}

case object BankedL2Key extends Field(BankedL2Params())

/** Parameterization of the memory-side bus created for each memory channel */
case class MemoryBusParams(beatBytes: Int, blockBytes: Int) extends HasTLBusParams

case object MemoryBusKey extends Field[MemoryBusParams]

/** Wrapper for creating TL nodes from a bus connected to the back of each mem channel */
class MemoryBus(params: MemoryBusParams)(implicit p: Parameters) extends TLBusWrapper(params, "MemoryBus")(p)
    with HasTLXbarPhy {

  private def bufferTo(buffer: BufferParams): TLOutwardNode =
    TLBuffer(buffer) :*= delayNode :*= outwardNode

  def fromCoherenceManager(
        name: Option[String] = None,
        buffer: BufferParams = BufferParams.none)
      (gen: => TLNode): TLInwardNode = {
    from(s"CoherenceManager${name.getOrElse("")}") {
      inwardNode :*= TLBuffer(buffer) :*= gen
    }
  }

  def toDRAMController[D,U,E,B <: Data](
        name: Option[String] = None,
        buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B]): OutwardNodeHandle[D,U,E,B] = {
    to(s"DRAMController${name.getOrElse("")}") { gen := bufferTo(buffer) }
  }

  def toVariableWidthSlave(
        name: Option[String] = None,
        buffer: BufferParams = BufferParams.none)
      (gen: => TLNode): TLOutwardNode = {
    to(s"Slave${name.getOrElse("")}") {
      gen :*= TLFragmenter(params.beatBytes, params.blockBytes) :*= bufferTo(buffer)
    }
  }
}

trait HasMemoryBus extends HasSystemBus with HasPeripheryBus with HasInterruptBus {
  private val mbusParams = p(MemoryBusKey)
  private val l2Params = p(BankedL2Key)
  val MemoryBusParams(memBusBeatBytes, memBusBlockBytes) = mbusParams
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
      ForceFanout(a = true) { implicit p => sbus.toMemoryBus { in } }
      mbus.fromCoherenceManager(None) { TLFilter(TLFilter.Mmask(AddressSet(offset * memBusBlockBytes, mask))) } := out
    }
    mbus
  }
}
