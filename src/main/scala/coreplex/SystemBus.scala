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
  slaveBuffering: BufferParams = BufferParams.default, // TODO should be BufferParams.none on BCE
  splitSlavesBuffering : BufferParams = BufferParams.default
) extends TLBusParams

case object SystemBusParams extends Field[SystemBusParams]

class SystemBus(params: SystemBusParams)(implicit p: Parameters) extends TLBusWrapper(params) {
  xbar.suggestName("SystemBus")

  private val master_splitter = LazyModule(new TLSplitter)  // Allows cycle-free connection to external networks
  inwardNode :=* master_splitter.node
  def busView = master_splitter.node.edgesIn.head

  private val split_slaves_buffer = LazyModule(new TLBuffer(params.splitSlavesBuffering))
  split_slaves_buffer.node :=* master_splitter.node

  protected def inwardSplitNode: TLInwardNode = master_splitter.node
  protected def outwardSplitNode: TLOutwardNode = split_slaves_buffer.node

  private val tile_fixer = LazyModule(new TLBuffer(BufferParams.none)) //TLFIFOFixer(TLFIFOFixer.allUncacheable))
  private val port_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))
  private val master_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))

  private val port_buffer =  LazyModule(new TLBuffer(BufferParams.default))
  private val master_buffer =  LazyModule(new TLBuffer(BufferParams.default))
  port_buffer.node :=* port_fixer.node
  master_buffer.node :=* master_fixer.node

  master_splitter.node :=* tile_fixer.node
  master_splitter.node :=* port_buffer.node
  inwardNode :=* master_buffer.node

  def toSplitSlaves: TLOutwardNode = outwardSplitNode

  private val pbusBuffer0 = LazyModule(new TLBuffer(BufferParams.default))
  private val pbusBuffer1 = LazyModule(new TLBuffer(BufferParams.default))
  private val pbusBuffer2 = LazyModule(new TLBuffer(BufferParams.default))
  private val pbusBuffer3 = LazyModule(new TLBuffer(BufferParams.default))
  pbusBuffer0.node :*= outwardWWNode
  pbusBuffer1.node :*= pbusBuffer0.node
  pbusBuffer2.node :*= pbusBuffer1.node
  pbusBuffer3.node :*= pbusBuffer2.node
  val toPeripheryBus: TLOutwardNode = pbusBuffer3.node

  val toMemoryBus: TLOutwardNode = outwardNode

  val toSlave: TLOutwardNode = outwardBufNode

  def fromAsyncMasters(depth: Int = 8, sync: Int = 3): TLAsyncInwardNode = {
    val sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
    master_fixer.node :=* sink.node
    sink.node
  }

  def fromSyncMasters(): TLInwardNode = {
    master_fixer.node
  }

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

  def fromSyncPorts(): TLInwardNode = {
    port_fixer.node
  }

  def fromSyncFIFOMaster(): TLInwardNode = fromSyncPorts()

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
