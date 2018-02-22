// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class PeripheryBusParams(
  beatBytes: Int,
  blockBytes: Int,
  frequency: BigInt = BigInt(100000000) // 100 MHz as default bus frequency
) extends HasTLBusParams

case object PeripheryBusKey extends Field[PeripheryBusParams]

class PeripheryBus(params: PeripheryBusParams, val crossing: SubsystemClockCrossing = SynchronousCrossing())
                  (implicit p: Parameters) extends TLBusWrapper(params, "periphery_bus")
    with HasTLXbarPhy
    with HasCrossing {

  private def bufferTo(buffer: BufferParams): TLOutwardNode =
    TLBuffer(buffer) :*= delayNode :*= outwardNode

  private def bufferTo(buffers: Int): TLOutwardNode =
    TLBuffer.chain(buffers).foldRight(delayNode)(_ :*= _) :*= outwardNode

  private def fragmentTo(minSize: Int, maxSize: Int, buffer: BufferParams): TLOutwardNode =
    TLFragmenter(minSize, maxSize) :*= bufferTo(buffer)

  private def fixedWidthTo(buffer: BufferParams): TLOutwardNode =
    TLWidthWidget(params.beatBytes) :*= bufferTo(buffer)

  def toSlave[D,U,E,B <: Data](
        name: Option[String] = None,
        buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[
                TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,
                D,U,E,B] = TLIdentity.gen): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :*= bufferTo(buffer) }
  }

  def toVariableWidthSlave[D,U,E,B <: Data](
        name: Option[String] = None,
        buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[
                TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,
                D,U,E,B] = TLIdentity.gen): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) {
      gen :*= fragmentTo(params.beatBytes, params.blockBytes, buffer)
    }
  }

  def toFixedWidthSlave[D,U,E,B <: Data](
        name: Option[String] = None,
        buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[
                TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,
                D,U,E,B] = TLIdentity.gen): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :*= fixedWidthTo(buffer) }
  }

  def toFixedWidthSingleBeatSlave[D,U,E,B <: Data](
        widthBytes: Int,
        name: Option[String] = None,
        buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[
                TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,
                D,U,E,B] = TLIdentity.gen): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) {
      gen :*= TLFragmenter(widthBytes, params.blockBytes) :*= fixedWidthTo(buffer)
    }
  }

  def toLargeBurstSlave(
        maxXferBytes: Int,
        name: Option[String] = None,
        buffer: BufferParams = BufferParams.none)
      (gen: => TLNode): TLOutwardNode = {
    to("slave" named name) {
      gen :*= fragmentTo(params.beatBytes, maxXferBytes, buffer)
    }
  }

  def toFixedWidthPort[D,U,E,B <: Data](
        name: Option[String] = None,
        buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B]): OutwardNodeHandle[D,U,E,B] = {
    to("port" named name) { gen := fixedWidthTo(buffer) }
  }


  def fromSystemBus(
        arithmetic: Boolean = true,
        buffer: BufferParams = BufferParams.default)
      (gen: => TLOutwardNode) {
    from("sbus") {
      (inwardNode
        :*= TLBuffer(buffer)
        :*= TLAtomicAutomata(arithmetic = arithmetic)
        :*= gen)
    }
  }

  def fromOtherMaster(
        name: Option[String] = None,
        buffer: BufferParams = BufferParams.none)
      (gen: => TLNode): TLInwardNode = {
    from("master" named name) { inwardNode :*= TLBuffer(buffer) :*= gen }
  }


  def toTile(
        name: Option[String] = None,
        buffers: Int = 0)
      (gen: => TLNode): TLOutwardNode = {
    to("tile" named name) {
      FlipRendering { implicit p =>
        gen :*= bufferTo(buffers)
      }
    }
  }
}
