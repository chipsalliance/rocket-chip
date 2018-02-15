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
    this { LazyScope(s"${busName}To${name}") { body } }
  }

  protected def from[T](name: String)(body: => T): T = {
    this { LazyScope(s"${busName}From${name}") { body } }
  }
}

trait HasTLXbarPhy { this: TLBusWrapper =>
  private val xbar = LazyModule(new TLXbar)
  xbar.suggestName(busName)

  protected def inwardNode: TLInwardNode = xbar.node
  protected def outwardNode: TLOutwardNode = xbar.node
}
