// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._

case object TLBusDelayProbability extends Field[Double](0.0)

/** Specifies widths of various attachement points in the SoC */
trait HasTLBusParams {
  val beatBytes: Int
  val blockBytes: Int

  def beatBits: Int = beatBytes * 8
  def blockBits: Int = blockBytes * 8
  def blockBeats: Int = blockBytes / beatBytes
  def blockOffset: Int = log2Up(blockBytes)
}

abstract class TLBusWrapper(params: HasTLBusParams, val busName: String)(implicit p: Parameters)
    extends SimpleLazyModule with LazyScope with HasTLBusParams {

  val beatBytes = params.beatBytes
  val blockBytes = params.blockBytes
  require(blockBytes % beatBytes == 0)

  protected def inwardNode: TLInwardNode
  protected def outwardNode: TLOutwardNode

  protected def bufferFrom(buffer: BufferParams): TLInwardNode =
    inwardNode :=* TLBuffer(buffer)

  protected def bufferFrom(buffers: Int): TLInwardNode =
    TLBuffer.chain(buffers).foldLeft(inwardNode)(_ :=* _)

  protected def fixFrom(policy: TLFIFOFixer.Policy, buffers: Int): TLInwardNode =
    inwardNode :=* TLBuffer.chain(buffers).foldLeft(TLFIFOFixer(policy))(_ :=* _)

  protected def bufferTo(buffer: BufferParams): TLOutwardNode =
    TLBuffer(buffer) :*= delayNode :*= outwardNode

  protected def bufferTo(buffers: Int): TLOutwardNode =
    TLBuffer.chain(buffers).foldRight(delayNode)(_ :*= _) :*= outwardNode

  protected def fixedWidthTo(buffer: BufferParams): TLOutwardNode =
    TLWidthWidget(beatBytes) :*= bufferTo(buffer)

  protected def fragmentTo(buffer: BufferParams): TLOutwardNode =
    TLFragmenter(beatBytes, blockBytes) :*= bufferTo(buffer)

  protected def fragmentTo(minSize: Int, maxSize: Int, buffer: BufferParams): TLOutwardNode =
    TLFragmenter(minSize, maxSize) :*= bufferTo(buffer)

  protected def delayNode(implicit p: Parameters): TLNode = {
    val delayProb = p(TLBusDelayProbability)
    if (delayProb > 0.0) {
      TLDelayer(delayProb) :*=* TLBuffer(BufferParams.flow) :*=* TLDelayer(delayProb)
    } else {
      val nodelay = TLIdentityNode()
      nodelay
    }
  }

  protected def to[T](name: String)(body: => T): T = {
    this { LazyScope(s"coupler_to_${name}") { body } }
  }

  protected def from[T](name: String)(body: => T): T = {
    this { LazyScope(s"coupler_from_${name}") { body } }
  }
}

trait HasTLXbarPhy { this: TLBusWrapper =>
  private val xbar = LazyModule(new TLXbar).suggestName(busName + "_xbar")

  protected def inwardNode: TLInwardNode = xbar.node
  protected def outwardNode: TLOutwardNode = xbar.node
}

object TLIdentity {
  def gen: TLNode = {
    val passthru = TLIdentityNode()
    passthru
  }
}
