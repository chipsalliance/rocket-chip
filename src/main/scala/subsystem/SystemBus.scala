// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink.{DevNullParams, TLError, TLZero}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class SystemBusParams(
  beatBytes: Int,
  blockBytes: Int,
  policy: TLArbiter.Policy = TLArbiter.roundRobin,
  errorDevice: Option[DevNullParams] = None) extends HasTLBusParams

case object SystemBusKey extends Field[SystemBusParams]

class SystemBus(params: SystemBusParams)(implicit p: Parameters)
    extends TLBusWrapper(params, "system_bus")
    with CanAttachTLSlaves
    with CanAttachTLMasters
    with HasTLXbarPhy {

  private val master_splitter = LazyModule(new TLSplitter)
  inwardNode :=* master_splitter.node

  params.errorDevice.foreach { dnp => LazyScope("wrapped_error_device") {
    val error = LazyModule(new TLError(params = dnp, beatBytes = params.beatBytes))
    error.node := TLBuffer() := outwardNode
  }}
  def busView = master_splitter.node.edges.in.head

  def toSlaveBus(name: String): (=> TLInwardNode) => NoHandle =
    gen => to(s"bus_named_$name") {
      (gen
        :*= TLWidthWidget(params.beatBytes)
        :*= outwardNode)
    }

  def fromMasterBus(name: String): (=> TLOutwardNode) => NoHandle =
    gen => from(s"bus_named_$name") { master_splitter.node :=* gen }

  def toSplitSlave[D,U,E,B <: Data]
      (name: Option[String] = None)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :=* master_splitter.node }
  }

  def fromTile
      (name: Option[String], buffer: BufferParams = BufferParams.none, cork: Option[Boolean] = None)
      (gen: => TLOutwardNode): NoHandle = {
    from("tile" named name) {
      master_splitter.node :=* TLBuffer(buffer) :=* TLFIFOFixer(TLFIFOFixer.allUncacheable) :=* gen
    }
  }
}
