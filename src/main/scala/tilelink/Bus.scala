// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

/** Specifies widths of various attachement points in the SoC */
trait TLBusParams {
  val beatBytes: Int
  val blockBytes: Int
  val masterBuffering: BufferParams
  val masterFIFOPolicy: TLFIFOFixer.Policy
  val slaveBuffering: BufferParams

  def beatBits: Int = beatBytes * 8
  def blockBits: Int = blockBytes * 8
  def blockBeats: Int = blockBytes / beatBytes
  def blockOffset: Int = log2Up(blockBytes)
}

abstract class TLBusWrapper(params: TLBusParams)(implicit p: Parameters) extends TLBusParams {
  val beatBytes = params.beatBytes
  val blockBytes = params.blockBytes
  val masterBuffering = params.masterBuffering
  val slaveBuffering = params.slaveBuffering
  val masterFIFOPolicy = params.masterFIFOPolicy
  require(blockBytes % beatBytes == 0)

  private val xbar = LazyModule(new TLXbar)
  private val masterBuffer = LazyModule(new TLBuffer(masterBuffering))
  private val masterFIFO = LazyModule(new TLFIFOFixer(masterFIFOPolicy))
  private val masterSplitter = LazyModule(new TLSplitter)  // Allows cycle-free connection to external networks
  private val slaveBuffer = LazyModule(new TLBuffer(slaveBuffering))
  private val slaveFrag = LazyModule(new TLFragmenter(beatBytes, blockBytes))
  private val slaveWW = LazyModule(new TLWidthWidget(beatBytes))

  masterFIFO.node :=* masterBuffer.node
  masterSplitter.node :=* masterFIFO.node
  xbar.node :=* masterSplitter.node

  slaveBuffer.node :*= xbar.node
  slaveFrag.node :*= slaveBuffer.node
  slaveWW.node :*= slaveBuffer.node

  def edgesIn = xbar.node.edgesIn
  def outwardNode: TLOutwardNode = xbar.node
  def outwardBufNode: TLOutwardNode = slaveBuffer.node
  def outwardFragNode: TLOutwardNode = slaveFrag.node
  def outwardFragNode(maxXfer: Int): TLOutwardNode = {
    TLFragmenter(params.beatBytes, maxXfer)(slaveBuffer.node)
  }
  def outwardWWNode: TLOutwardNode = slaveWW.node
  def outwardWWNode(buf: BufferParams): TLOutwardNode = {
    val buffer = LazyModule(new TLBuffer(buf))
    val ww = LazyModule(new TLWidthWidget(beatBytes))
    buffer.node :*= xbar.node
    ww.node :*= buffer.node
    ww.node
  }
  def inwardNode: TLInwardNode = xbar.node
  def inwardBufNode: TLInwardNode = masterBuffer.node
  def inwardFIFONode: TLInwardNode = masterFIFO.node
  def inwardFIFONode(policy: TLFIFOFixer.Policy): TLInwardNode = {
    val buffer = LazyModule(new TLBuffer())
    val ff =  LazyModule(new TLFIFOFixer(policy))
    ff.node :=* buffer.node
    masterSplitter.node :=* ff.node
    buffer.node 
  }
  def inwardSplitNode: TLInwardNode = masterSplitter.node
  def outwardSplitNode: TLOutwardNode = masterSplitter.node
}
