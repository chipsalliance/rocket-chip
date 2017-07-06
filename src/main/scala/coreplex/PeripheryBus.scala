// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

case class PeripheryBusParams(
  beatBytes: Int,
  blockBytes: Int,
  masterBuffering: BufferParams = BufferParams.default,
  slaveBuffering: BufferParams = BufferParams.none,
  masterFIFOPolicy: TLFIFOFixer.Policy = TLFIFOFixer.all,
  arithmetic: Boolean = true
) extends TLBusParams

case object PeripheryBusParams extends Field[PeripheryBusParams]

class PeripheryBus(params: PeripheryBusParams)(implicit p: Parameters) extends TLBusWrapper(params)

/** Provides buses that serve as attachment points,
  * for use in traits that connect individual devices or external ports.
  */
trait HasPeripheryBus extends HasSystemBus {
  private val pbusParams = p(PeripheryBusParams)
  val pbus = new PeripheryBus(pbusParams)

  // The peripheryBus hangs off of systemBus;
  // here we convert TL-UH -> TL-UL
  pbus.inwardBufNode := TLAtomicAutomata(arithmetic = pbusParams.arithmetic)(sbus.outwardWWNode)
}
