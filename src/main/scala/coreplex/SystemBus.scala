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
  private val tile_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.allUncacheable))
  inwardSplitNode :=* tile_fixer.node

  def toSplitSlaves: TLOutwardNode = outwardSplitNode

  val toPeripheryBus: TLOutwardNode = outwardWWNode

  val toMemoryBus: TLOutwardNode = outwardWWNode // TODO: don't buffer here by default

  def fromAsyncMasters(depth: Int = 8, sync: Int = 3): TLAsyncInwardNode = {
    val sink = LazyModule(new TLAsyncCrossingSink(depth, sync))
    inwardNode :=* sink.node
    sink.node
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

  def fromSyncPorts(policy: TLFIFOFixer.Policy): TLInwardNode = {
    val buffer = LazyModule(new TLBuffer())
    val ff =  LazyModule(new TLFIFOFixer(policy))
    ff.node :=* buffer.node
    inwardSplitNode :=* ff.node
    buffer.node 
  }
}

/** Provides buses that serve as attachment points,
  * for use in traits that connect individual devices or external ports.
  */
trait HasSystemBus extends HasInterruptBus {
  private val sbusParams = p(SystemBusParams)
  val sbusBeatBytes = sbusParams.beatBytes

  val sbus = new SystemBus(sbusParams)

  def sharedMemoryTLEdge: TLEdge = sbus.edgesIn.head
}
