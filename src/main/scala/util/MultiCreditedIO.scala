package freechips.rocketchip.util

import chisel3._
import chisel3.util._

/** MultiCreditedIO enables serializing multiple independent channels
  * onto a shared credited link, with independent crediting per-channel
  * This can be used to avoid introducing cross-channel resource dependencies
  * to avoid deadlock in a multi-channel communication protocol
  * The sender may only transmit Data when it has non-zero credits.
  * Receivers provide 0 or 1 credits to senders using the credit field.
  * Senders consume 0 or 1 credits by setting the debit field.
  * The bits Data field is DontCare when debit=0.
  * credit MAY depend combinationally on debit.
  * debit MAY depend combinationally on credit.
  * WARNING: The user must ensure the round trip time is > 0.
  * Failure to comply will result in a combinational loop!
  */
final class MultiCreditedIO[T <: Data](gen: T, nChannels: Int) extends Bundle {
  def genType: T = gen

  val channelBits = log2Ceil(nChannels)

  val credits = Input (Valid(UInt(channelBits.W)))
  val debits  = Output(Valid(UInt(channelBits.W)))
  val bits    = Output(genType)

  /** Provide a DecoupledIO interface for sending MultiCreditedIO[Data].
    * Convert an IrrevocableIO input to DecoupledIO via Decoupled().
    * depth controls the maximum number of Data beats inflight.
    * Sender powers on with credits=depth, so sender and receiver must agree on depth.
    * pipe=false increases the receiver=>sender trip time by one cycle.
    * pipe=true causes debit to depend on credit.
    */
  def toSenders(depths: Seq[Int], pipe: Boolean): Seq[DecoupledIO[T]] = {
    require (depths.min >= 1 && depths.size == nChannels)
    val senders = Wire(Vec(nChannels, Decoupled(genType)))
    val arb = Module(new Arbiter(genType, nChannels))
    arb.io.out.ready := true.B
    debits.valid := arb.io.out.valid
    debits.bits := arb.io.chosen
    bits := arb.io.out.bits
    senders.zipWithIndex.map { case (sender, i) =>
      val counter = new CreditedIOCounter(depths(i), depths(i))
      val credit  = credits.valid && credits.bits === i.U
      val debit   = debits.valid && debits.bits === i.U
      counter.update(credit, debit)
      val credit_available = !counter.empty || (pipe.B && credit)

      arb.io.in(i).valid := sender.valid && credit_available
      arb.io.in(i).bits  := sender.bits
      sender.ready       := arb.io.in(i).ready && credit_available
    }
    senders
  }

  def toSenders(depth: Int, pipe: Boolean = true): Seq[DecoupledIO[T]] =
    toSenders(Seq.fill(nChannels)(depth), pipe)

  /** Provide an DecoupledIO interface for receiving MultiCreditedIO[Data].
    * depth controls the Queue depth and thus maximum number of elements inflight.
    * flow=false increases the sender=>receiver trip time by one cycle.
    * flow=true causes credit to depend on debit.
    */
  def toReceivers(depths: Seq[Int], flow: Boolean): Seq[DecoupledIO[T]] = {
    require (depths.min >= 1 && depths.size == nChannels)
    val credit_arb = Module(new Arbiter(UInt(channelBits.W), nChannels))
    credit_arb.io.out.ready := true.B
    credits.valid := credit_arb.io.out.valid
    credits.bits := credit_arb.io.out.bits
    (0 until nChannels).map { i =>
      val enq = Wire(Decoupled(genType))
      enq.valid := debits.valid && debits.bits === i.U
      enq.bits := bits
      assert(!enq.valid || enq.ready)
      val res = Queue(enq, depths(i), pipe=true, flow=flow)
      val out = Wire(Decoupled(genType))
      out.bits := res.bits
      credit_arb.io.in(i).bits := i.U

      credit_arb.io.in(i).valid := res.valid && out.ready
      out.valid := res.valid && credit_arb.io.in(i).ready
      res.ready := out.ready && credit_arb.io.in(i).ready
      out
    }
  }

  def toReceivers(depth: Int, flow: Boolean = true): Seq[DecoupledIO[T]] =
    toReceivers(Seq.fill(nChannels)(depth), flow)

  /** Add register stages to the sender and receiver paths.
    * Apply this method to the producer/sender-facing bundle.
    * The round-trip-time (RTT) is increased by sender+receiver.
    */
  def pipeline(debitDelay: Int, creditDelay: Int): MultiCreditedIO[T] = {
    val res = Wire(MultiCreditedIO(genType, nChannels))
    if (debitDelay <= 0) {
      credits.valid := ShiftRegister(res.credits.valid, creditDelay, false.B, true.B)
      credits.bits  := ShiftRegister(res.credits.bits , creditDelay, true.B)
      res.debits := debits
      res.bits   := bits
    } else {
      // We can't use ShiftRegister, because we want debit-gated enables
      val out = pipeline(debitDelay-1, creditDelay)
      out.credits       := res.credits
      res.debits.valid  := RegNext(out.debits.valid, false.B)
      res.debits.bits   := RegEnable(out.debits.bits, out.debits.valid)
      res.bits          := RegEnable(out.bits       , out.debits.valid)
    }
    res
  }

  def pipeline(delay: CreditedDelay): MultiCreditedIO[T] =
    pipeline(delay.debit, delay.credit)
}

object MultiCreditedIO
{
  def apply[T <: Data](genType: T, nChannels: Int) = new MultiCreditedIO(genType, nChannels)

  def fromSenders[T <: Data](x: Seq[ReadyValidIO[T]], depth: Int, pipe: Boolean = true): MultiCreditedIO[T] = {
    val res = Wire(MultiCreditedIO(chiselTypeOf(x.head.bits), x.size))
    val dec = res.toSenders(depth, pipe)
    x.zip(dec).foreach(t => t._2 <> t._1)
    res
  }

  def fromReceivers[T <: Data](x: Seq[ReadyValidIO[T]], depth: Int, flow: Boolean = true): MultiCreditedIO[T] = {
    val res = Wire(MultiCreditedIO(chiselTypeOf(x.head.bits), x.size))
    val dec = res.toReceivers(depth, flow)
    x.zip(dec).foreach(t => t._1 <> t._2)
    res
  }
}
