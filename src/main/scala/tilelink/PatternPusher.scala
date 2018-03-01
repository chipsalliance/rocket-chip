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
  require (0 <= data && data < (BigInt(1) << (8 << size)))
  def bits(edge: TLEdgeOut) = edge.Put(UInt(0), UInt(address), UInt(size), UInt(data << (8*(address % edge.manager.beatBytes).toInt)))
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
    val io = IO(new Bundle {
      val run = Bool(INPUT)
      val done = Bool(OUTPUT)
    })

    val (tl_out, edgeOut) = node.out(0)
    pattern.foreach { p =>
      require (p.size <= log2Ceil(edgeOut.manager.beatBytes), "Patterns must fit in a single beat")
    }

    val step   = RegInit(UInt(0, width = log2Ceil(pattern.size+1)))
    val flight = RegInit(Bool(false))
    val ready  = RegNext(Bool(true), Bool(false))

    val end = step === UInt(pattern.size)
    io.done := end && !flight

    val a = tl_out.a
    val d = tl_out.d

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
    tl_out.b.ready := Bool(true)
    tl_out.c.valid := Bool(false)
    tl_out.e.valid := Bool(false)
  }
}

object TLPatternPusher
{
  def apply(name: String, pattern: Seq[Pattern])(implicit p: Parameters): TLOutwardNode =
  {
    val pusher = LazyModule(new TLPatternPusher(name, pattern))
    pusher.node
  }
}
