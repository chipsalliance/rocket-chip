// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class SystemBusParams(
  beatBytes: Int,
  blockBytes: Int,
  masterBuffering: BufferParams = BufferParams.default,
  slaveBuffering: BufferParams = BufferParams.default
) extends TLBusParams

case object SystemBusKey extends Field[SystemBusParams]

class SystemBus(params: SystemBusParams)(implicit p: Parameters) extends TLBusWrapper(params, "SystemBus") {

  private val master_splitter = LazyModule(new TLSplitter)  // Allows cycle-free connection to external networks
  master_splitter.suggestName(s"${busName}_master_TLSplitter")
  inwardNode :=* master_splitter.node
  def busView = master_splitter.node.edges.in.head

  protected def inwardSplitNode: TLInwardNode = master_splitter.node
  protected def outwardSplitNode: TLOutwardNode = master_splitter.node


  private val port_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))
  port_fixer.suggestName(s"${busName}_port_TLFIFOFixer")
  master_splitter.node :=* port_fixer.node

  private val pbus_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))
  pbus_fixer.suggestName(s"${busName}_pbus_TLFIFOFixer")
  pbus_fixer.node :*= outwardWWNode

  def toSplitSlaves: TLOutwardNode = outwardSplitNode

  def toPeripheryBus(addBuffers: Int = 0): TLOutwardNode = {
    TLBuffer.chain(addBuffers).foldRight(pbus_fixer.node:TLOutwardNode)(_ :*= _)
  }

  val toMemoryBus: TLOutwardNode = outwardNode

  val toSlave: TLOutwardNode = outwardBufNode

  def fromCoherentChip: TLInwardNode = inwardNode

  def fromFrontBus: TLInwardNode = master_splitter.node

  def fromTile(name: Option[String])(gen: Parameters => TLOutwardNode) {
    this {
      LazyScope(s"${busName}FromTile${name.getOrElse("")}") {
        master_splitter.node :=* gen(p)
      }
    }
  }

  def fromSyncPorts(params: BufferParams =  BufferParams.default, name: Option[String] = None): TLInwardNode = {
    val buffer = LazyModule(new TLBuffer(params))
    name.foreach { n => buffer.suggestName(s"${busName}_${n}_TLBuffer") }
    port_fixer.node :=* buffer.node
    buffer.node
  }

  def fromSyncFIFOMaster(params: BufferParams =  BufferParams.default, name: Option[String] = None): TLInwardNode = {
    fromSyncPorts(params, name)
  }
}

/** Provides buses that serve as attachment points,
  * for use in traits that connect individual devices or external ports.
  */
trait HasSystemBus extends HasInterruptBus {
  private val sbusParams = p(SystemBusKey)
  val sbusBeatBytes = sbusParams.beatBytes

  val sbus = LazyModule(new SystemBus(sbusParams))

  def sharedMemoryTLEdge: TLEdge = sbus.busView
}
