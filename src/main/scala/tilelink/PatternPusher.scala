// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
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
  def bits(edge: TLEdgeOut) = edge.Put(0.U, address.U, size.U, (data << (8*(address % edge.manager.beatBytes).toInt)).U)
}

case class ReadPattern(address: BigInt, size: Int) extends Pattern
{
  def bits(edge: TLEdgeOut) = edge.Get(0.U, address.U, size.U)
}

case class ReadExpectPattern(address: BigInt, size: Int, data: BigInt) extends Pattern
{
  def bits(edge: TLEdgeOut) = edge.Get(0.U, address.U, size.U)
  override def dataIn = Some(data)
}

class TLPatternPusher(name: String, pattern: Seq[Pattern])(implicit p: Parameters) extends LazyModule
{
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = name)))))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val run = Input(Bool())
      val done = Output(Bool())
    })

    val (tl_out, edgeOut) = node.out(0)
    pattern.foreach { p =>
      require (p.size <= log2Ceil(edgeOut.manager.beatBytes), "Patterns must fit in a single beat")
    }

    val step   = RegInit(0.U(log2Ceil(pattern.size+1).W))
    val flight = RegInit(false.B)
    val ready  = RegNext(true.B, false.B)

    val end = step === pattern.size.U
    io.done := end && !flight

    val a = tl_out.a
    val d = tl_out.d

    // Expected response?
    val check  = VecInit(pattern.map(p => p.dataIn.isDefined.B))(step) holdUnless a.fire
    val expect = VecInit(pattern.map(p => p.dataIn.getOrElse(BigInt(0)).U))(step) holdUnless a.fire
    assert (!check || !d.fire || expect === d.bits.data)

    when (a.fire) {
      flight := true.B
      step := step + 1.U
    }
    when (d.fire) {
      flight := false.B
    }

    val (plegal, pbits) = pattern.map(_.bits(edgeOut)).unzip
    assert (end || VecInit(plegal)(step), s"Pattern pusher ${name} tried to push an illegal request")

    a.valid := io.run && ready && !end && !flight
    a.bits  := VecInit(pbits)(step)
    d.ready := true.B

    // Tie off unused channels
    tl_out.b.ready := true.B
    tl_out.c.valid := false.B
    tl_out.e.valid := false.B
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
