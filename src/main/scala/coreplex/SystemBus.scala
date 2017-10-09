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

  private val tile_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.allUncacheable))
  tile_fixer.suggestName(s"${busName}_tile_TLFIFOFixer")
  master_splitter.node :=* tile_fixer.node

  private val port_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))
  port_fixer.suggestName(s"${busName}_port_TLFIFOFixer")
  master_splitter.node :=* port_fixer.node

  private val pbus_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))
  pbus_fixer.suggestName(s"${busName}_pbus_TLFIFOFixer")
  pbus_fixer.node :*= outwardWWNode

  def toSplitSlaves: TLOutwardNode = outwardSplitNode

  def toPeripheryBus(addBuffers: Int = 0): TLOutwardNode = {
    val (in, out) = bufferChain(addBuffers, name = Some("pbus"))
    in := pbus_fixer.node
    out
  }

  val toMemoryBus: TLOutwardNode = outwardNode

  val toSlave: TLOutwardNode = outwardBufNode

  def fromCoherentChip: TLInwardNode = inwardNode

  def fromFrontBus: TLInwardNode = master_splitter.node

  def fromSyncTiles(params: BufferParams, addBuffers: Int = 0, name: Option[String] = None): TLInwardNode = {
    val tile_buf = LazyModule(new TLBuffer(params))
    name.foreach { n => tile_buf.suggestName(s"${busName}_${n}_TLBuffer") }
    val (in, out) = bufferChain(addBuffers, name = name)

    tile_fixer.node :=* out
    in :=* tile_buf.node
    tile_buf.node
  }

  def fromRationalTiles(dir: RationalDirection, addBuffers: Int = 0, name: Option[String] = None): TLRationalInwardNode = {
    val tile_sink = LazyModule(new TLRationalCrossingSink(direction = dir))
    name.foreach { n => tile_sink.suggestName(s"${busName}_${n}_TLRationalCrossingSink") }
    val (in, out) = bufferChain(addBuffers, name = name)

    tile_fixer.node :=* out
    in :=* tile_sink.node
    tile_sink.node
  }

  def fromAsyncTiles(depth: Int, sync: Int, addBuffers: Int = 0, name: Option[String] = None): TLAsyncInwardNode = {
    val tile_sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
    name.foreach { n => tile_sink.suggestName(s"${busName}_${n}_TLAsyncCrossingSink") }
    val (in, out) = bufferChain(addBuffers, name = name)

    tile_fixer.node :=* out
    in :=* tile_sink.node
    tile_sink.node
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

  def fromAsyncPorts(depth: Int = 8, sync: Int = 3, name : Option[String] = None): TLAsyncInwardNode = {
    val sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
    name.foreach { n => sink.suggestName(s"${busName}_${n}_TLAsyncCrossingSink") }
    port_fixer.node :=* sink.node
    sink.node
  }

  def fromAsyncFIFOMaster(depth: Int = 8, sync: Int = 3, name: Option[String] = None): TLAsyncInwardNode = fromAsyncPorts(depth, sync, name)

  def fromRationalPorts(dir: RationalDirection, name: Option[String] = None): TLRationalInwardNode = {
    val sink = LazyModule(new TLRationalCrossingSink(dir))
    name.foreach{ n => sink.suggestName(s"${busName}_${n}_TLRationalCrossingSink") }
    port_fixer.node :=* sink.node
    sink.node
  }

  def fromRationalFIFOMaster(dir: RationalDirection, name: Option[String] = None): TLRationalInwardNode = fromRationalPorts(dir, name)
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
