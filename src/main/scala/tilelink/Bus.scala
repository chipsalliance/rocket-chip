// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._

case object TLBusDelayProbability extends Field[Double]

/** Specifies widths of various attachement points in the SoC */
trait TLBusParams {
  val beatBytes: Int
  val blockBytes: Int
  val masterBuffering: BufferParams
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
  require(blockBytes % beatBytes == 0)
  private val delayProb = p(TLBusDelayProbability)

  private val delayer = if (delayProb > 0.0) Some(LazyModule(new TLDelayer(delayProb))) else None
  protected val xbar = LazyModule(new TLXbar)
  private val master_buffer = LazyModule(new TLBuffer(masterBuffering))
  private val slave_buffer = LazyModule(new TLBuffer(slaveBuffering))
  private val slave_frag = LazyModule(new TLFragmenter(beatBytes, blockBytes))
  private val slave_ww = LazyModule(new TLWidthWidget(beatBytes))

  xbar.node :=* master_buffer.node
  slave_buffer.node :*= delayer.map { d =>
    d.node :*= xbar.node
    d.node
  } .getOrElse { xbar.node }
  slave_frag.node :*= slave_buffer.node
  slave_ww.node :*= slave_buffer.node

  protected def outwardNode: TLOutwardNode = delayer.map(_.node).getOrElse(xbar.node)
  protected def outwardBufNode: TLOutwardNode = slave_buffer.node
  protected def outwardFragNode: TLOutwardNode = slave_frag.node
  protected def outwardWWNode: TLOutwardNode = slave_ww.node
  protected def inwardNode: TLInwardNode = xbar.node
  protected def inwardBufNode: TLInwardNode = master_buffer.node

  def bufferFromMasters: TLInwardNode = inwardBufNode

  def bufferToSlaves: TLOutwardNode = outwardBufNode 

  def toAsyncSlaves(sync: Int = 3): TLAsyncOutwardNode = {
    val source = LazyModule(new TLAsyncCrossingSource(sync))
    source.node :*= outwardNode
    source.node
  }

  def toRationalSlaves: TLRationalOutwardNode = {
    val source = LazyModule(new TLRationalCrossingSource())
    source.node :*= outwardNode
    source.node
  }

  def toVariableWidthSlaves: TLOutwardNode = outwardFragNode

  def toAsyncVariableWidthSlaves(sync: Int = 3): TLAsyncOutwardNode = {
    val source = LazyModule(new TLAsyncCrossingSource(sync))
    source.node :*= outwardFragNode
    source.node
  }

  def toRationalVariableWidthSlaves: TLRationalOutwardNode = {
    val source = LazyModule(new TLRationalCrossingSource())
    source.node :*= outwardFragNode
    source.node
  }

  def toFixedWidthSlaves: TLOutwardNode = outwardWWNode

  def toAsyncFixedWidthSlaves(sync: Int = 3): TLAsyncOutwardNode = {
    val source = LazyModule(new TLAsyncCrossingSource(sync))
    source.node := outwardWWNode
    source.node
  }

  def toRationalFixedWidthSlaves: TLRationalOutwardNode = {
    val source = LazyModule(new TLRationalCrossingSource())
    source.node :*= outwardWWNode
    source.node
  }

  def toFixedWidthPorts: TLOutwardNode = outwardWWNode // TODO, do/don't buffer here; knowing we will after the necessary port conversions

}
