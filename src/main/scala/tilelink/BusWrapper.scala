// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

/** Specifies widths of various attachement points in the SoC */
trait HasTLBusParams {
  val beatBytes: Int
  val blockBytes: Int

  def beatBits: Int = beatBytes * 8
  def blockBits: Int = blockBytes * 8
  def blockBeats: Int = blockBytes / beatBytes
  def blockOffset: Int = log2Up(blockBytes)
}

abstract class TLBusWrapper(params: HasTLBusParams, val busName: String)(implicit p: Parameters)
    extends SimpleLazyModule
    with LazyScope
    with HasTLBusParams {

  val beatBytes = params.beatBytes
  val blockBytes = params.blockBytes
  require(blockBytes % beatBytes == 0)

  def inwardNode: TLInwardNode
  def outwardNode: TLOutwardNode

  protected def bufferFrom(buffer: BufferParams): TLInwardNode =
    inwardNode :=* TLBuffer(buffer)

  protected def fixFrom(policy: TLFIFOFixer.Policy, buffer: BufferParams): TLInwardNode =
    inwardNode :=* TLBuffer(buffer) :=* TLFIFOFixer(policy)

  protected def bufferTo(buffer: BufferParams): TLOutwardNode =
    TLBuffer(buffer) :*= outwardNode

  protected def fixedWidthTo(buffer: BufferParams): TLOutwardNode =
    TLWidthWidget(beatBytes) :*= bufferTo(buffer)

  protected def fragmentTo(buffer: BufferParams): TLOutwardNode =
    TLFragmenter(beatBytes, blockBytes) :*= bufferTo(buffer)

  protected def fragmentTo(minSize: Int, maxSize: Int, buffer: BufferParams): TLOutwardNode =
    TLFragmenter(minSize, maxSize) :*= bufferTo(buffer)

  def to[T](name: String)(body: => T): T = {
    this { LazyScope(s"coupler_to_${name}") { body } }
  }

  def from[T](name: String)(body: => T): T = {
    this { LazyScope(s"coupler_from_${name}") { body } }
  }
}

trait CanAttachTLSlaves { this: TLBusWrapper =>
  def toSlave[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :*= bufferTo(buffer) }
  }

  def toVariableWidthSlaveNode(name: Option[String] = None, buffer: BufferParams = BufferParams.none)(node: TLInwardNode) {
    toVariableWidthSlaveNodeOption(name, buffer)(Some(node))
  }

  def toVariableWidthSlaveNodeOption(name: Option[String] = None, buffer: BufferParams = BufferParams.none)(node: Option[TLInwardNode]) {
    node foreach { n => to("slave" named name) { n :*= fragmentTo(buffer) } }
  }

  def toVariableWidthSlave[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :*= fragmentTo(buffer) }
  }

  def toFixedWidthSlaveNode(name: Option[String] = None, buffer: BufferParams = BufferParams.none)(gen: TLInwardNode) {
    to("slave" named name) { gen :*= fixedWidthTo(buffer) }
  }

  def toFixedWidthSlave[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :*= fixedWidthTo(buffer) }
  }

  def toFixedWidthSingleBeatSlaveNode
      (widthBytes: Int, name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: TLInwardNode) {
    to("slave" named name) {
      gen :*= TLFragmenter(widthBytes, blockBytes) :*= fixedWidthTo(buffer)
    }
  }

  def toFixedWidthSingleBeatSlave[D,U,E,B <: Data]
      (widthBytes: Int, name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) {
      gen :*= TLFragmenter(widthBytes, blockBytes) :*= fixedWidthTo(buffer)
    }
  }

  def toLargeBurstSlave[D,U,E,B <: Data]
      (maxXferBytes: Int, name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) {
      gen :*= fragmentTo(beatBytes, maxXferBytes, buffer)
    }
  }

  def toFixedWidthPort[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("port" named name) { gen := fixedWidthTo(buffer) }
  }
}

trait CanAttachTLMasters { this: TLBusWrapper =>
  def fromMasterNode
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: TLOutwardNode) {
    from("master" named name) { fixFrom(TLFIFOFixer.all, buffer) :=* gen }
  }

  def fromMaster[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[D,U,E,B,TLClientPortParameters,TLManagerPortParameters,TLEdgeOut,TLBundle] =
        TLNameNode(name)): InwardNodeHandle[D,U,E,B] = {
    from("master" named name) { fixFrom(TLFIFOFixer.all, buffer) :=* gen }
  }

  def fromPort[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[D,U,E,B,TLClientPortParameters,TLManagerPortParameters,TLEdgeOut,TLBundle] =
        TLNameNode(name)): InwardNodeHandle[D,U,E,B] = {
    from("port" named name) { fixFrom(TLFIFOFixer.all, buffer) :=* gen }
  }

  def fromCoherentMaster[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[D,U,E,B,TLClientPortParameters,TLManagerPortParameters,TLEdgeOut,TLBundle] =
        TLNameNode(name)): InwardNodeHandle[D,U,E,B] = {
    from("coherent_master" named name) { fixFrom(TLFIFOFixer.all, buffer) :=* gen }
  }
}

trait HasTLXbarPhy { this: TLBusWrapper =>
  private val xbar = LazyModule(new TLXbar).suggestName(busName + "_xbar")

  def inwardNode: TLInwardNode = xbar.node
  def outwardNode: TLOutwardNode = xbar.node
}
