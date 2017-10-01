// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._

case object TLBusDelayProbability extends Field[Double](0.0)

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

abstract class TLBusWrapper(params: TLBusParams, val busName: String)(implicit p: Parameters)
    extends SimpleLazyModule with LazyScope with TLBusParams {

  val beatBytes = params.beatBytes
  val blockBytes = params.blockBytes
  val masterBuffering = params.masterBuffering
  val slaveBuffering = params.slaveBuffering
  require(blockBytes % beatBytes == 0)
  private val delayProb = p(TLBusDelayProbability)

  protected val xbar = LazyModule(new TLXbar)
  xbar.suggestName(busName)

  private val master_buffer = LazyModule(new TLBuffer(masterBuffering))
  master_buffer.suggestName(s"${busName}_master_TLBuffer")
  private val slave_buffer = LazyModule(new TLBuffer(slaveBuffering))
  slave_buffer.suggestName(s"${busName}_slave_TLBuffer")
  private val slave_frag = LazyModule(new TLFragmenter(beatBytes, blockBytes))
  slave_frag.suggestName(s"${busName}_slave_TLFragmenter")
  
  private val slave_ww = LazyModule(new TLWidthWidget(beatBytes))
  slave_ww.suggestName(s"${busName}_slave_TLWidthWidget")

  private val delayedNode = if (delayProb > 0.0) {
    val firstDelay = LazyModule(new TLDelayer(delayProb))
    val flowDelay = LazyModule(new TLBuffer(BufferParams.flow))
    val secondDelay = LazyModule(new TLDelayer(delayProb))
    firstDelay.node :*= xbar.node
    flowDelay.node :*= firstDelay.node
    secondDelay.node :*= flowDelay.node
    secondDelay.node
  } else {
    xbar.node
  }

  xbar.node :=* master_buffer.node
  slave_buffer.node :*= delayedNode
  slave_frag.node :*= slave_buffer.node
  slave_ww.node :*= slave_buffer.node

  protected def outwardNode: TLOutwardNode = delayedNode
  protected def outwardBufNode: TLOutwardNode = slave_buffer.node
  protected def outwardFragNode: TLOutwardNode = slave_frag.node
  protected def outwardWWNode: TLOutwardNode = slave_ww.node
  protected def inwardNode: TLInwardNode = xbar.node
  protected def inwardBufNode: TLInwardNode = master_buffer.node

  protected def bufferChain(depth: Int, name: Option[String] = None): (TLInwardNode, TLOutwardNode) = {
    SourceCardinality { implicit p =>
      val chain = LazyModule(new TLBufferChain(depth))
      name.foreach { n => chain.suggestName(s"${busName}_${n}_TLBufferChain")}
      (chain.node, chain.node)
    }
  }

  def bufferFromMasters: TLInwardNode = inwardBufNode

  def bufferToSlaves: TLOutwardNode = outwardBufNode 

  def toSyncSlaves(name: Option[String] = None, addBuffers: Int = 0): TLOutwardNode = SinkCardinality { implicit p =>
    TLBufferChain(addBuffers)(outwardBufNode)
  }

  def toAsyncSlaves(sync: Int = 3, name: Option[String] = None, addBuffers: Int = 0): TLAsyncOutwardNode = SinkCardinality { implicit p =>
    val source = LazyModule(new TLAsyncCrossingSource(sync))
    name.foreach{ n => source.suggestName(s"${busName}_${n}_TLAsyncCrossingSource")}
    source.node :=? TLBufferChain(addBuffers)(outwardNode)
    source.node
  }

  def toRationalSlaves(name: Option[String] = None, addBuffers: Int = 0): TLRationalOutwardNode = SinkCardinality { implicit p =>
    val source = LazyModule(new TLRationalCrossingSource())
    name.foreach{ n => source.suggestName(s"${busName}_${n}_TLRationalCrossingSource")}
    source.node :=? TLBufferChain(addBuffers)(outwardNode)
    source.node
  }

  def toVariableWidthSlaves: TLOutwardNode = outwardFragNode

  def toAsyncVariableWidthSlaves(sync: Int = 3, name: Option[String] = None): TLAsyncOutwardNode = {
    val source = LazyModule(new TLAsyncCrossingSource(sync))
    name.foreach {n =>  source.suggestName(s"${busName}_${name}_TLAsyncCrossingSource")}
    source.node :*= outwardFragNode
    source.node
  }

  def toRationalVariableWidthSlaves(name: Option[String] = None): TLRationalOutwardNode = {
    val source = LazyModule(new TLRationalCrossingSource())
    name.foreach {n =>  source.suggestName(s"${busName}_${name}_TLRationalCrossingSource")}
    source.node :*= outwardFragNode
    source.node
  }

  def toFixedWidthSlaves: TLOutwardNode = outwardWWNode

  def toAsyncFixedWidthSlaves(sync: Int = 3, name: Option[String] = None): TLAsyncOutwardNode = {
    val source = LazyModule(new TLAsyncCrossingSource(sync))
    name.foreach { n => source.suggestName(s"${busName}_${name}_TLAsyncCrossingSource")}
    source.node := outwardWWNode
    source.node
  }

  def toRationalFixedWidthSlaves(name: Option[String] = None): TLRationalOutwardNode = {
    val source = LazyModule(new TLRationalCrossingSource())
    name.foreach {n => source.suggestName(s"${busName}_${name}_TLRationalCrossingSource")}
    source.node :*= outwardWWNode
    source.node
  }

  def toFixedWidthPorts: TLOutwardNode = outwardWWNode // TODO, do/don't buffer here; knowing we will after the necessary port conversions

}
