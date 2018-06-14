// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class SystemBusParams(
  beatBytes: Int,
  blockBytes: Int,
  pbusBuffer: BufferParams = BufferParams.none,
  arithmeticAtomics: Boolean = true,
  bufferAtomics: BufferParams = BufferParams.default) extends HasTLBusParams

case object SystemBusKey extends Field[SystemBusParams]

class SystemBus(params: SystemBusParams)(implicit p: Parameters) extends TLBusWrapper(params, "system_bus")
    with HasTLXbarPhy {

  private val master_splitter = LazyModule(new TLSplitter)
  inwardNode :=* master_splitter.node

  val cbus_params = new PeripheryBusParams(
    p(PeripheryBusKey).beatBytes,
    params.blockBytes,
    params.arithmeticAtomics,
    params.bufferAtomics,
    SynchronousCrossing())
  val control_bus = LazyModule(new PeripheryBus(cbus_params))
  control_bus.fromSystemBus {
    TLFIFOFixer(TLFIFOFixer.all) :*= TLWidthWidget(params.beatBytes) :*= bufferTo(params.pbusBuffer)
  }

  protected def fixFromThenSplit(policy: TLFIFOFixer.Policy, buffer: BufferParams): TLInwardNode =
    master_splitter.node :=* TLBuffer(buffer) :=* TLFIFOFixer(policy)

  def busView = master_splitter.node.edges.in.head

  def toPeripheryBus(gen: => TLNode): TLOutwardNode = {
    to("pbus") {
      (gen
        :*= TLFIFOFixer(TLFIFOFixer.all)
        :*= TLWidthWidget(params.beatBytes)
        :*= bufferTo(params.pbusBuffer))
    }
  }

  def toMemoryBus(gen: => TLInwardNode) {
    to("mbus") { gen := delayNode := outwardNode }
  }

  def toSlave[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.default)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLIdentity.gen): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :*= bufferTo(buffer) }
  }
 
  def toSplitSlave[D,U,E,B <: Data]
      (name: Option[String] = None)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLIdentity.gen): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :=* master_splitter.node }
  }

  def toFixedWidthSlave[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.default)
      (gen: =>  NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLIdentity.gen): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :*= fixedWidthTo(buffer) }
  }

  def toVariableWidthSlave[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.default)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLIdentity.gen): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :*= fragmentTo(buffer) }
  }

  def fromFrontBus(gen: => TLNode): TLInwardNode = {
    from("front_bus") { master_splitter.node :=* gen }
  }

  def fromTile
      (name: Option[String], buffer: BufferParams = BufferParams.none, cork: Option[Boolean] = None)
      (gen: => TLNode): TLInwardNode = {
    from("tile" named name) {
      fixFromThenSplit(TLFIFOFixer.allUncacheable, buffer) :=* gen
    }
  }

  def toFixedWidthPort[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.default)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLIdentity.gen): OutwardNodeHandle[D,U,E,B] = {
    to("port" named name) { gen := fixedWidthTo(buffer) }
  }

  def fromPort[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[D,U,E,B,TLClientPortParameters,TLManagerPortParameters,TLEdgeOut,TLBundle] =
        TLIdentity.gen): InwardNodeHandle[D,U,E,B] = {
    from("port" named name) { fixFromThenSplit(TLFIFOFixer.all, buffer) :=* gen }
  }

  def fromCoherentMaster[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[D,U,E,B,TLClientPortParameters,TLManagerPortParameters,TLEdgeOut,TLBundle] =
        TLIdentity.gen): InwardNodeHandle[D,U,E,B] = {
    from("coherent_master" named name) { fixFrom(TLFIFOFixer.all, buffer) :=* gen }
  }

  def fromMaster[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[D,U,E,B,TLClientPortParameters,TLManagerPortParameters,TLEdgeOut,TLBundle] =
        TLIdentity.gen): InwardNodeHandle[D,U,E,B] = {
    from("master" named name) { fixFromThenSplit(TLFIFOFixer.all, buffer) :=* gen }
  }

}
