// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

trait Pattern {
  def address: BigInt
  def size: Int
  def bits(edge: TLEdgeOut): (Bool, TLBundleA)
  def dataIn: Option[BigInt] = None
  require ((address & ((BigInt(1) << size) - 1)) == 0)
}

case class WritePattern(address: BigInt, size: Int, data: BigInt) extends Pattern
{
  require (log2Floor(data) < (BigInt(8) << size))
  def bits(edge: TLEdgeOut) = edge.Put(UInt(0), UInt(address), UInt(size), UInt(data))
}

case class ReadPattern(address: BigInt, size: Int) extends Pattern
{
  def bits(edge: TLEdgeOut) = edge.Get(UInt(0), UInt(address), UInt(size))
}

case class ReadExpectPattern(address: BigInt, size: Int, data: BigInt) extends Pattern
{
  def bits(edge: TLEdgeOut) = edge.Get(UInt(0), UInt(address), UInt(size))
  override def dataIn = Some(data)
}

class TLPatternPusher(name: String, pattern: Seq[Pattern])(implicit p: Parameters) extends LazyModule
{
  val node = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters(name = name)))))

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val tl_out = node.bundleOut
      val run = Bool(INPUT)
      val done = Bool(OUTPUT)
    }

    val edgeOut = node.edgesOut(0)
    pattern.foreach { p =>
      require (p.size <= log2Ceil(edgeOut.manager.beatBytes), "Patterns must fit in a single beat")
    }

    val step   = RegInit(UInt(0, width = log2Ceil(pattern.size+1)))
    val flight = RegInit(Bool(false))
    val ready  = RegNext(Bool(true), Bool(false))

    val end = step === UInt(pattern.size)
    io.done := end && !flight

    val a = io.tl_out(0).a
    val d = io.tl_out(0).d

    // Expected response?
    val check  = Vec(pattern.map(p => Bool(p.dataIn.isDefined)))(step) holdUnless a.fire()
    val expect = Vec(pattern.map(p => UInt(p.dataIn.getOrElse(BigInt(0)))))(step) holdUnless a.fire()
    assert (!check || !d.fire() || expect === d.bits.data)

    when (a.fire()) {
      flight := Bool(true)
      step := step + UInt(1)
    }
    when (d.fire()) {
      flight := Bool(false)
    }

    val (plegal, pbits) = pattern.map(_.bits(edgeOut)).unzip
    assert (end || Vec(plegal)(step), s"Pattern pusher ${name} tried to push an illegal request")

    a.valid := io.run && ready && !end && !flight
    a.bits  := Vec(pbits)(step)
    d.ready := Bool(true)

    // Tie off unused channels
    io.tl_out(0).b.ready := Bool(true)
    io.tl_out(0).c.valid := Bool(false)
    io.tl_out(0).e.valid := Bool(false)
  }
}
