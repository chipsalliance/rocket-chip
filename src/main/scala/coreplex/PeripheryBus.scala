// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import freechips.rocketchip.config.Field

case class PeripheryBusParams(
  beatBytes: Int,
  blockBytes: Int,
  masterBuffering: BufferParams = BufferParams.default,
  slaveBuffering: BufferParams = BufferParams.none,
  arithmetic: Boolean = true,
  frequency: BigInt = BigInt(100000000) // 100 MHz as default bus frequency
) extends TLBusParams {
}

case object PeripheryBusKey extends Field[PeripheryBusParams]

class PeripheryBus(params: PeripheryBusParams)(implicit p: Parameters) extends TLBusWrapper(params, "PeripheryBus") {

  def toFixedWidthSingleBeatSlave(widthBytes: Int) = {
    TLFragmenter(widthBytes, params.blockBytes)(outwardWWNode)
  }

  def toLargeBurstSlave(maxXferBytes: Int) = {
    TLFragmenter(params.beatBytes, maxXferBytes)(outwardBufNode)
  }

  val fromSystemBus: TLInwardNode = {
    val atomics = LazyModule(new TLAtomicAutomata(arithmetic = params.arithmetic))
    inwardBufNode := atomics.node
    atomics.node
  }
}

/** Provides buses that serve as attachment points,
  * for use in traits that connect individual devices or external ports.
  */
trait HasPeripheryBus extends HasSystemBus {
  private val pbusParams = p(PeripheryBusKey)
  val pbusBeatBytes = pbusParams.beatBytes

  val pbus = LazyModule(new PeripheryBus(pbusParams))

  // The peripheryBus hangs off of systemBus; here we convert TL-UH -> TL-UL
  pbus.fromSystemBus := sbus.toPeripheryBus()
}
