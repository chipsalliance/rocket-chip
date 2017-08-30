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

case object SystemBusParams extends Field[SystemBusParams]

class SystemBus(params: SystemBusParams)(implicit p: Parameters) extends TLBusWrapper(params, "SystemBus") {

  private val master_splitter = LazyModule(new TLSplitter)  // Allows cycle-free connection to external networks
  master_splitter.suggestName(s"${busName}_master_TLSplitter"}
  inwardNode :=* master_splitter.node
  def busView = master_splitter.node.edgesIn.head

  protected def inwardSplitNode: TLInwardNode = master_splitter.node
  protected def outwardSplitNode: TLOutwardNode = master_splitter.node

  private val tile_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.allUncacheable))
  tile_fixer.suggestName(s"${busName}_tile_TLFIFOFixer")
  private val port_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))
  port_fixer.suggestName(s"${busName}_port_TLFIFOFixer")
  private val pbus_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))
  pbus_fixer.suggestName(s"${busName}_pbus_TLFIFOFixer")
  
  master_splitter.node :=* tile_fixer.node
  master_splitter.node :=* port_fixer.node
  pbus_fixer.node :*= outwardWWNode

  def toSplitSlaves: TLOutwardNode = outwardSplitNode

  private val pbusBuffer0 = LazyModule(new TLBuffer(BufferParams.default))
  private val pbusBuffer1 = LazyModule(new TLBuffer(BufferParams.default))
  private val pbusBuffer2 = LazyModule(new TLBuffer(BufferParams.default))
  private val pbusBuffer3 = LazyModule(new TLBuffer(BufferParams.default))
  pbusBuffer0.node :*= pbus_fixer.node
  pbusBuffer1.node :*= pbusBuffer0.node
  pbusBuffer2.node :*= pbusBuffer1.node
  pbusBuffer3.node :*= pbusBuffer2.node
  val toPeripheryBus: TLOutwardNode = pbusBuffer3.node

  val toMemoryBus: TLOutwardNode = outwardNode

  val toSlave: TLOutwardNode = outwardBufNode

  def fromCoherentChip: TLInwardNode = inwardNode

  def fromSyncTiles(params: BufferParams, name: Option[String] = None): TLInwardNode = {
    val buf = LazyModule(new TLBuffer(params))
    name.foreach{n => buf.suggestName(s"${busName}_${n}_TLBuffer")}
    tile_fixer.node :=* buf.node
    buf.node
  }

  def fromRationalTiles(dir: RationalDirection, name: Option[String] = None): TLRationalInwardNode = {
    val sink = LazyModule(new TLRationalCrossingSink(direction = dir))
    name.foreach{n => sink.suggestName(s"${busName}_${n}_TLRationalCrossingSink")}
    tile_fixer.node :=* sink.node
    sink.node
  }

  def fromAsyncTiles(depth: Int, sync: Int, name: Option[String] = None): TLAsyncInwardNode = {
    val sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
    name.foreach{n => sink.suggestName(s"${busName}_${n}_TLAsyncCrossignSink")}
    tile_fixer.node :=* sink.node
    sink.node
  }

  def fromSyncPorts(params: BufferParams =  BufferParams.default, name: Option[String] = None): TLInwardNode = {
    val buffer = LazyModule(new TLBuffer(params))
    name.foreach{ n => buffer.suggestName(s"${busName}_${n}_TLBuffer") }
    port_fixer.node :=* buffer.node
    buffer.node
  }

  def fromSyncFIFOMaster(params: BufferParams =  BufferParams.default, name: Option[String] = None): TLInwardNode = {
    fromSyncPorts(params, name)
  }

  def fromAsyncPorts(depth: Int = 8, sync: Int = 3, name : Option[String] = None): TLAsyncInwardNode = {
    val sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
    name.foreach{ n => sink.suggestName(s"${busName}_${n}_TLAsyncCrossingSink") }
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
  private val sbusParams = p(SystemBusParams)
  val sbusBeatBytes = sbusParams.beatBytes

  val sbus = new SystemBus(sbusParams)

  def sharedMemoryTLEdge: TLEdge = sbus.busView
}
