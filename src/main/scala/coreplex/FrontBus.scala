// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class FrontBusParams(
  beatBytes: Int,
  blockBytes: Int,
  masterBuffering: BufferParams = BufferParams.default,
  slaveBuffering: BufferParams = BufferParams.default
) extends TLBusParams

case object FrontBusKey extends Field[FrontBusParams]

class FrontBus(params: FrontBusParams)(implicit p: Parameters) extends TLBusWrapper(params, "FrontBus") {

  private val master_buffer = LazyModule(new TLBuffer(params.masterBuffering))
  private val master_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))

  master_buffer.suggestName(s"${busName}_master_TLBuffer")
  master_fixer.suggestName(s"${busName}_master_TLFIFOFixer")

  master_fixer.node :=* master_buffer.node
  inwardNode :=* master_fixer.node

  def fromSyncPorts(addBuffers: Int = 0, name: Option[String] = None): TLInwardNode = {
    val (in, out) = bufferChain(addBuffers, name)
    master_buffer.node :=* out
    in
  }

  def fromSyncMasters(addBuffers: Int = 0, name: Option[String] = None): TLInwardNode = {
    val (in, out) = bufferChain(addBuffers, name)
    master_buffer.node :=* out
    in
  }

  def fromCoherentChip: TLInwardNode = inwardNode

  def toSystemBus : TLOutwardNode = outwardBufNode

}

/** Provides buses that serve as attachment points,
  * for use in traits that connect individual devices or external ports.
  */
trait HasFrontBus extends HasSystemBus {
  private val frontbusParams = p(FrontBusKey)
  val frontbusBeatBytes = frontbusParams.beatBytes

  val fbus = LazyModule(new FrontBus(frontbusParams))

  FlipRendering { implicit p => sbus.fromFrontBus := fbus.toSystemBus }
}
