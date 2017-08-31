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
  slaveBuffering: BufferParams = BufferParams.none // TODO should be BufferParams.none on BCE
) extends TLBusParams

case object FrontBusParams extends Field[FrontBusParams]

class FrontBus(params: FrontBusParams)(implicit p: Parameters) extends TLBusWrapper(params, "FrontBus") {

  def fromSyncMasters(params: BufferParams = BufferParams.default, buffers: Int = 1, name: Option[String] = None): TLInwardNode =
    fromSyncPorts(params, buffers, name)

  def fromSyncPorts(params: BufferParams =  BufferParams.default, buffers: Int = 1, name: Option[String] = None): TLInwardNode = {
    val buf = List.fill(buffers)(LazyModule(new TLBuffer(params)))
    name.foreach { n => buf.zipWithIndex foreach {case (b, i) => b.suggestName(s"${busName}_${n}_${i}_TLBuffer")}}
    for(i<-1 until buffers) {
      buf(i).node :=* buf(i-1).node
    }
    inwardNode :=* buf(buffers-1).node
    if(buffers>0) buf(0).node else inwardNode
  }

  def fromSyncFIFOMaster(params: BufferParams =  BufferParams.default, buffers: Int = 1, name: Option[String] = None): TLInwardNode =
    fromSyncPorts(params, buffers, name)

  def toSystemBus : TLOutwardNode = outwardBufNode

}

/** Provides buses that serve as attachment points,
  * for use in traits that connect individual devices or external ports.
  */
trait HasFrontBus extends HasSystemBus {
  private val frontbusParams = p(FrontBusParams)
  val frontbusBeatBytes = frontbusParams.beatBytes

  val frontbus = new FrontBus(frontbusParams)

  sbus.fromSyncPorts(name = Some("FrontBus")) := frontbus.toSystemBus

}
