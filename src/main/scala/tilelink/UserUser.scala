// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.reflect.ClassTag

class TLUserUser[T <: UserBits : ClassTag](meta: T, f: (TLBundleA, TLClientParameters) => UInt)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode(
    clientFn  = { cp => cp.addUser(meta) },
    managerFn = { mp => mp })

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in

      out.a.bits.user.foreach {
        val mux = edgeOut.putUser(in.a.bits.user.getOrElse(0.U), Seq(x => f(in.a.bits, x)))
        _ := mux(out.a.bits.source)
      }
    }
  }
}

object TLUserUser {
  def apply[T <: UserBits : ClassTag](meta: T, f: (TLBundleA, TLClientParameters) => UInt)(implicit p: Parameters): TLNode = {
    val user = LazyModule(new TLUserUser(meta, f))
    user.node
  }
}

/** Synthesizeable unit tests */
import freechips.rocketchip.unittest._

case class FunkyBits0(name: String) extends UserBits {
  val width = 2
}

case class FunkyBits1(width: Int, hax: Double) extends UserBits

class TLLazyUserTest(txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz  = LazyModule(new TLFuzzer(txns))
  val fuzz2 = LazyModule(new TLFuzzer(txns, noModify=true))
  val model = LazyModule(new TLRAMModel("Xbar"))
  val xbar  = LazyModule(new TLXbar)
  val ram   = LazyModule(new TLRAM(AddressSet(0, 0x3ff)))
  val tap   = TLIdentityNode()

  (ram.node
    := TLFragmenter(4, 256)
    := tap
    := TLWidthWidget(2)
    := TLBuffer()
    := TLAtomicAutomata()
    := TLUserUser(FunkyBits0("hax"), { case x => 2.U })
    := xbar.node
    := TLFIFOFixer()
    := TLUserUser(FunkyBits1(3, 3.1415), { case x => 5.U })
    := model.node
    := fuzz.node)

  xbar.node := fuzz2.node // funky bits 1 not on this master

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    val (x, xEdge) = tap.in(0)
    val fb0      = xEdge.getUserHead[FunkyBits0](x.a.bits)
    val Seq(fb1) = xEdge.getUserOrElse[FunkyBits1](x.a.bits, 7.U)
    val Seq(fbv) = xEdge.getUserSeq[FunkyBits1](x.a.bits)
    when (x.a.fire()) { printf("USER: %x %x %x %x\n", fb0, fb1, fbv.valid, x.a.bits.user.get) }

    io.finished := fuzz.module.io.finished && fuzz2.module.io.finished
  }
}

class TLUserTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new TLLazyUserTest(txns)).module)
  io.finished := dut.io.finished
  dut.io.start := true.B
}
