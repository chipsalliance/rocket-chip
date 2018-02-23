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
  arithmeticAtomics: Boolean = true,
  bufferAtomics: BufferParams = BufferParams.default,
  sbusCrossingType: SubsystemClockCrossing = SynchronousCrossing(), // relative to sbus
  frequency: BigInt = BigInt(100000000) // 100 MHz as default bus frequency
) extends HasTLBusParams

case object PeripheryBusKey extends Field[PeripheryBusParams]

class PeripheryBus(params: PeripheryBusParams)
                  (implicit p: Parameters) extends TLBusWrapper(params, "periphery_bus")
    with HasTLXbarPhy
    with HasCrossing {
  val crossing = params.sbusCrossingType

  def toSlave[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLIdentity.gen): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :*= bufferTo(buffer) }
  }

  def toVariableWidthSlaveNode(name: Option[String] = None, buffer: BufferParams = BufferParams.none)(node: TLInwardNode) { toVariableWidthSlaveNodeOption(name, buffer)(Some(node)) }

  def toVariableWidthSlaveNodeOption(name: Option[String] = None, buffer: BufferParams = BufferParams.none)(node: Option[TLInwardNode]) {
    node foreach { n => to("slave" named name) { n :*= fragmentTo(buffer) } }
  }

  def toVariableWidthSlave[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLIdentity.gen): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :*= fragmentTo(buffer) }
  }

  def toFixedWidthSlaveNode(name: Option[String] = None, buffer: BufferParams = BufferParams.none)(gen: TLInwardNode) {
    to("slave" named name) { gen :*= fixedWidthTo(buffer) }
  }

  def toFixedWidthSlave[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLIdentity.gen): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :*= fixedWidthTo(buffer) }
  }

  def toFixedWidthSingleBeatSlaveNode
      (widthBytes: Int, name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: TLInwardNode) {
    to("slave" named name) {
      gen :*= TLFragmenter(widthBytes, params.blockBytes) :*= fixedWidthTo(buffer)
    }
  }

  def toFixedWidthSingleBeatSlave[D,U,E,B <: Data]
      (widthBytes: Int, name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLIdentity.gen): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) {
      gen :*= TLFragmenter(widthBytes, params.blockBytes) :*= fixedWidthTo(buffer)
    }
  }

  def toLargeBurstSlave[D,U,E,B <: Data]
      (maxXferBytes: Int, name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLIdentity.gen): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) {
      gen :*= fragmentTo(params.beatBytes, maxXferBytes, buffer)
    }
  }

  def toFixedWidthPort[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLIdentity.gen): OutwardNodeHandle[D,U,E,B] = {
    to("port" named name) { gen := fixedWidthTo(buffer) }
  }


  def fromSystemBus(gen: => TLOutwardNode) {
    from("sbus") {
      (inwardNode
        :*= TLBuffer(params.bufferAtomics)
        :*= TLAtomicAutomata(arithmetic = params.arithmeticAtomics)
        :*= gen)
    }
  }

  def fromOtherMaster[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[D,U,E,B,TLClientPortParameters,TLManagerPortParameters,TLEdgeOut,TLBundle] =
        TLIdentity.gen): InwardNodeHandle[D,U,E,B] = {
    from("master" named name) { bufferFrom(buffer) :=* gen }
  }


  def toTile
      (name: Option[String] = None, buffers: Int = 0)
      (gen: => TLNode): TLOutwardNode = {
    to("tile" named name) { FlipRendering { implicit p =>
      gen :*= bufferTo(buffers)
    }}
  }
}
