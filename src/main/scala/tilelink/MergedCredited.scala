package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.diplomacy.{AddressSet}
import freechips.rocketchip.prci.{ClockCrossingType, CreditedCrossing}
import freechips.rocketchip.subsystem.CrossingWrapper
import freechips.rocketchip.util.{CreditedDelay, MultiCreditedIO}

case object UseTLMergedCreditedCrossing extends Field[Boolean](false)

class TLMergedCreditedSource(delay: TLMergedCreditedDelay)(implicit p: Parameters) extends LazyModule
{
  val node = TLMergedCreditedSourceNode(delay)
  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val tld = edgeOut.delay

      val to_credited = Wire(Vec(3, Decoupled(new TLBundleACE(edgeIn.bundle))))
      val from_credited = Wire(Vec(2, Decoupled(new TLBundleBD(edgeIn.bundle))))

      out.eca <> MultiCreditedIO.fromSenders(to_credited, tld.ace.total).pipeline(delay.ace)
      from_credited <> VecInit(out.db.pipeline(delay.bd).toReceivers(tld.bd.total))

      Seq(in.e, in.c, in.a).zipWithIndex.foreach { case (channel, i) =>
        val out = to_credited(i)
        channel.ready := out.ready
        out.valid := channel.valid
        out.bits := DontCare
        (out.bits: Data).waiveAll :<>= (channel.bits: Data).waiveAll
      }

      Seq(in.d, in.b).zipWithIndex.foreach { case (channel, i) =>
        val out = from_credited(i)
        out.ready := channel.ready
        channel.valid := out.valid
        (channel.bits: Data).waiveAll :<>= (out.bits: Data).waiveAll
      }
    }
  }
}

object TLMergedCreditedSource {
  def apply(delay: TLMergedCreditedDelay)(implicit p: Parameters): TLMergedCreditedSourceNode = {
    val source = LazyModule(new TLMergedCreditedSource(delay))
    source.node
  }
  def apply(delay: CreditedDelay)(implicit p: Parameters): TLMergedCreditedSourceNode = apply(TLMergedCreditedDelay(delay))
  def apply()(implicit p: Parameters): TLMergedCreditedSourceNode = apply(CreditedDelay(1, 1))
}


class TLMergedCreditedSink(delay: TLMergedCreditedDelay)(implicit p: Parameters) extends LazyModule
{
  val node = TLMergedCreditedSinkNode(delay)
  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val tld = edgeIn.delay

      val to_credited = Wire(Vec(2, Decoupled(new TLBundleBD(edgeOut.bundle))))
      val from_credited = Wire(Vec(3, Decoupled(new TLBundleACE(edgeOut.bundle))))

      in.db <> MultiCreditedIO.fromSenders(to_credited, tld.bd.total).pipeline(delay.bd)
      from_credited <> in.eca.pipeline(delay.ace).toReceivers(tld.ace.total)

      Seq(out.d, out.b).zipWithIndex.foreach { case (channel, i) =>
        val in = to_credited(i)
        channel.ready := in.ready
        in.valid := channel.valid
        in.bits := DontCare
        (in.bits: Data).waiveAll :<>= (channel.bits: Data).waiveAll
      }

      Seq(out.e, out.c, out.a).zipWithIndex.foreach { case (channel, i) =>
        val in = from_credited(i)
        in.ready := channel.ready
        channel.valid := in.valid
        (channel.bits: Data).waiveAll :<>= (in.bits: Data).waiveAll
      }
    }
  }
}

object TLMergedCreditedSink {
  def apply(delay: TLMergedCreditedDelay)(implicit p: Parameters): TLMergedCreditedSinkNode = {
    val sink = LazyModule(new TLMergedCreditedSink(delay))
    sink.node
  }
  def apply(delay: CreditedDelay)(implicit p: Parameters): TLMergedCreditedSinkNode = apply(TLMergedCreditedDelay(delay))
  def apply()(implicit p: Parameters): TLMergedCreditedSinkNode = apply(CreditedDelay(1, 1))
}

// Synthesizable unit tests
import freechips.rocketchip.unittest._

class TLRAMMergedCreditedCrossing(txns: Int, params: CreditedCrossing)(implicit p: Parameters) extends LazyModule {
  val model = LazyModule(new TLRAMModel("MergedCreditedCrossing"))
  val fuzz = LazyModule(new TLFuzzer(txns))
  val island = LazyModule(new CrossingWrapper(params))
  val ram  = island { LazyModule(new TLRAM(AddressSet(0x0, 0x3ff))) }

  island.crossTLIn(ram.node) := TLFragmenter(4, 256) := TLDelayer(0.1) := model.node := fuzz.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMMergedCreditedCrossingTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val u = p.alterPartial { case UseTLMergedCreditedCrossing => true }

  val dut_1000 = Module(LazyModule(new TLRAMMergedCreditedCrossing(txns, CreditedCrossing(CreditedDelay(1, 0), CreditedDelay(0, 0)))(u)).module)
  val dut_0100 = Module(LazyModule(new TLRAMMergedCreditedCrossing(txns, CreditedCrossing(CreditedDelay(0, 1), CreditedDelay(0, 0)))(u)).module)
  val dut_0010 = Module(LazyModule(new TLRAMMergedCreditedCrossing(txns, CreditedCrossing(CreditedDelay(0, 0), CreditedDelay(1, 0)))(u)).module)
  val dut_0001 = Module(LazyModule(new TLRAMMergedCreditedCrossing(txns, CreditedCrossing(CreditedDelay(0, 0), CreditedDelay(0, 1)))(u)).module)
  val dut_1111 = Module(LazyModule(new TLRAMMergedCreditedCrossing(txns, CreditedCrossing(CreditedDelay(1, 1), CreditedDelay(1, 1)))(u)).module)

  val duts = Seq(dut_1000, dut_0100, dut_0010, dut_0001, dut_1111)
  duts.foreach { _.io.start := true.B }
  io.finished := duts.map(_.io.finished).reduce(_ && _)
}
