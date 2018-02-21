// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class SystemBusParams(beatBytes: Int, blockBytes: Int) extends HasTLBusParams

case object SystemBusKey extends Field[SystemBusParams]

class SystemBus(params: SystemBusParams)(implicit p: Parameters) extends TLBusWrapper(params, "system_bus")
    with HasTLXbarPhy {

  private val master_splitter = LazyModule(new TLSplitter)
  inwardNode :=* master_splitter.node

  def busView = master_splitter.node.edges.in.head

  private def bufferTo(buffer: BufferParams): TLOutwardNode =
    TLBuffer(buffer) :*= delayNode :*= outwardNode

  private def fixedWidthTo(buffer: BufferParams): TLOutwardNode =
    TLWidthWidget(params.beatBytes) :*= bufferTo(buffer)

  def toPeripheryBus(buffer: BufferParams = BufferParams.none)
                    (gen: => TLNode): TLOutwardNode = {
    to("pbus") {
      (gen
        := TLFIFOFixer(TLFIFOFixer.all)
        := TLWidthWidget(params.beatBytes)
        := bufferTo(buffer))
    }
  }

  def toMemoryBus(gen: => TLInwardNode) {
    to("mbus") { gen :*= delayNode :*= outwardNode }
  }

  def toSlave(name: Option[String] = None, buffer: BufferParams = BufferParams.default)
             (gen: => TLNode): TLOutwardNode = {
    to("slave" named name) { gen :*= bufferTo(buffer) }
  }
 
  def toSplitSlave(name: Option[String] = None)
                  (gen: => TLNode): TLOutwardNode = {
    to("slave" named name) { gen :*= master_splitter.node }
  }

  def toFixedWidthSlave(
        name: Option[String] = None,
        buffer: BufferParams = BufferParams.none)
      (gen: => TLNode): TLOutwardNode = {
    to("slave" named name) { gen :*= fixedWidthTo(buffer) }
  }

  def toVariableWidthSlave(
        name: Option[String] = None,
        buffer: BufferParams = BufferParams.default)
      (gen: => TLNode): TLOutwardNode = {
    to("slave" named name) {
      gen :*= TLFragmenter(params.beatBytes, params.blockBytes) :*= bufferTo(buffer)
    }
  }

  def fromCoherentChip(gen: => TLNode): TLInwardNode = {
    from("CoherentChip") { inwardNode :=* gen }
  }

  def fromFrontBus(gen: => TLNode): TLInwardNode = {
    from("FrontBus") { master_splitter.node :=* gen }
  }

  def fromTile(
        name: Option[String],
        buffers: Int = 0,
        cork: Option[Boolean] = None)
      (gen: => TLNode): TLInwardNode = {
    from("tile" named name) {
      (List(master_splitter.node, TLFIFOFixer(TLFIFOFixer.allUncacheable)) ++ TLBuffer.chain(buffers))
        .reduce(_ :=* _) :=* gen
    }
  }

  def toFixedWidthPort[D,U,E,B <: Data](
        name: Option[String] = None,
        buffer: BufferParams = BufferParams.default)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B]): OutwardNodeHandle[D,U,E,B] = {
    to("port" named name) { gen := fixedWidthTo(buffer) }
  }

  def fromPort[D,U,E,B <: Data](
        name: Option[String] = None,
        buffers: Int = 0)
      (gen: => NodeHandle[D,U,E,B,TLClientPortParameters,TLManagerPortParameters,TLEdgeOut,TLBundle]): InwardNodeHandle[D,U,E,B] = {
    from("port" named name) {
      (List(
        master_splitter.node,
        TLFIFOFixer(TLFIFOFixer.all)) ++
        TLBuffer.chain(buffers)).reduce(_ :=* _) :=* gen
    }
  }

  def fromMaster(
        name: Option[String] = None,
        buffers: Int = 0)
      (gen: => TLNode): TLInwardNode = {
    from("master" named name) {
      (List(
        master_splitter.node,
        TLFIFOFixer(TLFIFOFixer.all)) ++
        TLBuffer.chain(buffers)).reduce(_ :=* _) :=* gen
    }
  }
}
