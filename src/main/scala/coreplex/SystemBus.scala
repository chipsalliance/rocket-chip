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
  slaveBuffering: BufferParams = BufferParams.flow // TODO should be BufferParams.none on BCE
) extends TLBusParams

case object SystemBusParams extends Field[SystemBusParams]

class SystemBus(params: SystemBusParams)(implicit p: Parameters) extends TLBusWrapper(params) {
  xbar.suggestName("SystemBus")

  private val master_splitter = LazyModule(new TLSplitter)  // Allows cycle-free connection to external networks
  inwardNode :=* master_splitter.node
  def busView = master_splitter.node.edgesIn.head

  protected def inwardSplitNode: TLInwardNode = master_splitter.node
  protected def outwardSplitNode: TLOutwardNode = master_splitter.node

  private val tile_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.allUncacheable))
  private val port_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))
  master_splitter.node :=* tile_fixer.node
  master_splitter.node :=* port_fixer.node

  def toSplitSlaves: TLOutwardNode = outwardSplitNode

  val toPeripheryBus: TLOutwardNode = outwardWWNode

  val toMemoryBus: TLOutwardNode = outwardNode

  val toSlave: TLOutwardNode = outwardBufNode

  def fromCoherentChip: TLInwardNode = inwardNode

  def fromSyncTiles(params: BufferParams): TLInwardNode = {
    val buf = LazyModule(new TLBuffer(params))
    tile_fixer.node :=* buf.node
    buf.node
  }

  def fromRationalTiles(dir: RationalDirection): TLRationalInwardNode = {
    val sink = LazyModule(new TLRationalCrossingSink(direction = dir))
    tile_fixer.node :=* sink.node
    sink.node
  }

  def fromAsyncTiles(depth: Int, sync: Int): TLAsyncInwardNode = {
    val sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
    tile_fixer.node :=* sink.node
    sink.node
  }

  def fromSyncPorts(params: BufferParams =  BufferParams.default, name: Option[String] = None): TLInwardNode = {
    val buffer = LazyModule(new TLBuffer(params))
    name.foreach{ n => buffer.suggestName(s"${n}_TLBuffer") }
    port_fixer.node :=* buffer.node
    buffer.node
  }

  def fromSyncFIFOMaster(params: BufferParams =  BufferParams.default, name: Option[String] = None): TLInwardNode = {
    fromSyncPorts(params, name)
  }

  def fromAsyncPorts(depth: Int = 8, sync: Int = 3): TLAsyncInwardNode = {
    val sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
    port_fixer.node :=* sink.node
    sink.node
  }

  def fromAsyncFIFOMaster(depth: Int = 8, sync: Int = 3): TLAsyncInwardNode = fromAsyncPorts(depth, sync)

  def fromRationalPorts(dir: RationalDirection): TLRationalInwardNode = {
    val sink = LazyModule(new TLRationalCrossingSink(dir))
    port_fixer.node :=* sink.node
    sink.node
  }

  def fromRationalFIFOMaster(dir: RationalDirection): TLRationalInwardNode = fromRationalPorts(dir)
}

/** Provides buses that serve as attachment points,
  * for use in traits that connect individual devices or external ports.
  */
trait HasSystemBus extends HasInterruptBus {
  private val sbusParams = p(SystemBusParams)
  val sbusBeatBytes = sbusParams.beatBytes

  val sbus = new SystemBus(sbusParams)

  def sharedMemoryTLEdge: TLEdge = sbus.busView
}
