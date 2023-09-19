// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util.Decoupled
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.CrossingWrapper
import freechips.rocketchip.util._

class TLCreditedBuffer(delay: TLCreditedDelay)(implicit p: Parameters) extends LazyModule
{
  val node = TLCreditedAdapterNode(
    clientFn  = p => p.copy(delay = delay + p.delay),
    managerFn = p => p.copy(delay = delay + p.delay))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.a :<>= in.a.pipeline(delay.a)
      in.b :<>= out.b.pipeline(delay.b)
      out.c :<>= in.c.pipeline(delay.c)
      in.d :<>= out.d.pipeline(delay.d)
      out.e :<>= in.e.pipeline(delay.e)
    }
  }
}

object TLCreditedBuffer {
  def apply(delay: TLCreditedDelay)(implicit p: Parameters): TLCreditedAdapterNode = {
    val buffer = LazyModule(new TLCreditedBuffer(delay))
    buffer.node
  }
  def apply(delay: CreditedDelay)(implicit p: Parameters): TLCreditedAdapterNode = apply(TLCreditedDelay(delay))
  def apply()(implicit p: Parameters): TLCreditedAdapterNode = apply(CreditedDelay(1, 1))
}

class TLCreditedSource(delay: TLCreditedDelay)(implicit p: Parameters) extends LazyModule
{
  val node = TLCreditedSourceNode(delay)
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val tld = edgeOut.delay
      out.a :<>= CreditedIO.fromSender(in.a, tld.a.total).pipeline(delay.a)
      in.b :<>= Decoupled(out.b.pipeline(delay.b).toReceiver(tld.b.total))
      out.c :<>= CreditedIO.fromSender(in.c, tld.c.total).pipeline(delay.c)
      in.d :<>= Decoupled(out.d.pipeline(delay.d).toReceiver(tld.d.total))
      out.e :<>= CreditedIO.fromSender(in.e, tld.e.total).pipeline(delay.e)
    }
  }
}

object TLCreditedSource {
  def apply(delay: TLCreditedDelay)(implicit p: Parameters): TLCreditedSourceNode = {
    val source = LazyModule(new TLCreditedSource(delay))
    source.node
  }
  def apply(delay: CreditedDelay)(implicit p: Parameters): TLCreditedSourceNode = apply(TLCreditedDelay(delay))
  def apply()(implicit p: Parameters): TLCreditedSourceNode = apply(CreditedDelay(1, 1))
}

class TLCreditedSink(delay: TLCreditedDelay)(implicit p: Parameters) extends LazyModule
{
  val node = TLCreditedSinkNode(delay)
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val tld = edgeIn.delay
      out.a :<>= Decoupled(in.a.pipeline(delay.a).toReceiver(tld.a.total))
      in.b :<>= CreditedIO.fromSender(out.b, tld.b.total).pipeline(delay.b)
      out.c :<>= Decoupled(in.c.pipeline(delay.c).toReceiver(tld.c.total))
      in.d :<>= CreditedIO.fromSender(out.d, tld.d.total).pipeline(delay.d)
      out.e :<>= Decoupled(in.e.pipeline(delay.e).toReceiver(tld.e.total))
    }
  }
}

object TLCreditedSink {
  def apply(delay: TLCreditedDelay)(implicit p: Parameters): TLCreditedSinkNode = {
    val sink = LazyModule(new TLCreditedSink(delay))
    sink.node
  }
  def apply(delay: CreditedDelay)(implicit p: Parameters): TLCreditedSinkNode = apply(TLCreditedDelay(delay))
  def apply()(implicit p: Parameters): TLCreditedSinkNode = apply(CreditedDelay(1, 1))
}

// Synthesizable unit tests
import freechips.rocketchip.unittest._

class TLRAMCreditedCrossing(txns: Int, params: CreditedCrossing)(implicit p: Parameters) extends LazyModule {
  val model = LazyModule(new TLRAMModel("CreditedCrossing"))
  val fuzz = LazyModule(new TLFuzzer(txns))
  val island = LazyModule(new CrossingWrapper(params))
  val ram  = island { LazyModule(new TLRAM(AddressSet(0x0, 0x3ff))) }

  island.crossTLIn(ram.node) := TLFragmenter(4, 256) := TLDelayer(0.1) := model.node := fuzz.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMCreditedCrossingTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut_1000 = Module(LazyModule(new TLRAMCreditedCrossing(txns, CreditedCrossing(CreditedDelay(1, 0), CreditedDelay(0, 0)))).module)
  val dut_0100 = Module(LazyModule(new TLRAMCreditedCrossing(txns, CreditedCrossing(CreditedDelay(0, 1), CreditedDelay(0, 0)))).module)
  val dut_0010 = Module(LazyModule(new TLRAMCreditedCrossing(txns, CreditedCrossing(CreditedDelay(0, 0), CreditedDelay(1, 0)))).module)
  val dut_0001 = Module(LazyModule(new TLRAMCreditedCrossing(txns, CreditedCrossing(CreditedDelay(0, 0), CreditedDelay(0, 1)))).module)
  val dut_1111 = Module(LazyModule(new TLRAMCreditedCrossing(txns, CreditedCrossing(CreditedDelay(1, 1), CreditedDelay(1, 1)))).module)

  val duts = Seq(dut_1000, dut_0100, dut_0010, dut_0001, dut_1111)
  duts.foreach { _.io.start := true.B }
  io.finished := duts.map(_.io.finished).reduce(_ && _)
}
