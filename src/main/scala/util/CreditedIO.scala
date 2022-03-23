// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

/** Transmission delay in credit-debit systems.
  * debit delay is the number of cycles it takes debit+'bits' to transit the link.
  * credit delay is the number of cycles it takes credits to be returned.
  * round trip / total delay is the sum of debit and credit delay.
  * The system must have a positive total delay, otherwise you have a combinational loop.
  */
case class CreditedDelay(debit: Int, credit: Int)
{
  val total = debit + credit
  require (debit >= 0)
  require (credit >= 0)

  def flip: CreditedDelay = CreditedDelay(credit, debit)

  def +(that: CreditedDelay): CreditedDelay =
    CreditedDelay(debit + that.debit, credit + that.credit)

  override def toString = s"${debit}:${credit}"
}

/** CreditedIO provides credit-debit-based flow control for arbitrary Data.
  * The sender may only transmit Data when it has non-zero credits.
  * Receivers provide 0 or 1 credits to senders using the credit field.
  * Senders consume 0 or 1 credits by setting the debit field.
  * The bits Data field is DontCare when debit=0.
  * credit MAY depend combinationally on debit.
  * debit MAY depend combinationally on credit.
  * WARNING: The user must ensure the round trip time is > 0.
  * Failure to comply will result in a combinational loop!
  */
final class CreditedIO[T <: Data](gen: T) extends Bundle
{
  def genType: T = gen

  val credit = Input (Bool()) // 1: a credit is given to the sender by the receiver
  val debit  = Output(Bool()) // 1: a credit is consumed by the sender to transfer 'bits'
  val bits   = Output(genType)

  /** Provide a DecoupledIO interface for sending CreditedIO[Data].
    * Convert an IrrevocableIO input to DecoupledIO via Decoupled().
    * depth controls the maximum number of Data beats inflight.
    * Sender powers on with credits=depth, so sender and receiver must agree on depth.
    * pipe=false increases the receiver=>sender trip time by one cycle.
    * pipe=true causes debit to depend on credit.
    */
  def toSender(depth: Int, pipe: Boolean = true): DecoupledIO[T] = {
    require (depth >= 1)
    val res = Wire(DecoupledIO(genType))
    val counter = new CreditedIOCounter(depth, depth)
    counter.update(this)
    res.ready := !counter.empty || (pipe.B && credit)
    debit := res.fire()
    bits  := res.bits
    res
  }

  /** Provide an IrrevocableIO interface for receiving CreditedIO[Data].
    * Conversion to DecoupledIO is done via application of Decoupled().
    * depth controls the Queue depth and thus maximum number of elements inflight.
    * flow=false increases the sender=>receiver trip time by one cycle.
    * flow=true causes credit to depend on debit.
    */
  def toReceiver(depth: Int, flow: Boolean = true): IrrevocableIO[T] = {
    require (depth >= 1)
    val enq = Wire(DecoupledIO(genType))
    enq.valid := debit
    enq.bits := bits
    assert (!enq.valid || enq.ready)
    val res = Queue.irrevocable(enq, depth, pipe=true, flow=flow)
    credit := res.fire()
    res
  }

  /** Add register stages to the sender and receiver paths.
    * Apply this method to the producer/sender-facing bundle.
    * The round-trip-time (RTT) is increased by sender+receiver.
    */
  def pipeline(debitDelay: Int, creditDelay: Int): CreditedIO[T] = {
    val res = Wire(CreditedIO(genType))
    if (debitDelay <= 0) {
      credit := ShiftRegister(res.credit, creditDelay, false.B, true.B)
      res.debit := debit
      res.bits  := bits
    } else {
      // We can't use ShiftRegister, because we want debit-gated enables
      val out = pipeline(debitDelay-1, creditDelay)
      out.credit := res.credit
      res.debit := RegNext(out.debit, false.B)
      res.bits  := RegEnable(out.bits, out.debit)
    }
    res
  }

  def pipeline(delay: CreditedDelay): CreditedIO[T] =
    pipeline(delay.debit, delay.credit)
}

object CreditedIO
{
  def apply[T <: Data](genType: T) = new CreditedIO(genType)

  def fromSender[T <: Data](x: ReadyValidIO[T], depth: Int, pipe: Boolean = true): CreditedIO[T] = {
    val res = Wire(CreditedIO(chiselTypeOf(x.bits)))
    val dec = res.toSender(depth, pipe)
    dec.valid := x.valid
    dec.bits := x.bits
    x.ready := dec.ready
    res
  }

  def fromReceiver[T <: Data](x: ReadyValidIO[T], depth: Int, flow: Boolean = true): CreditedIO[T] = {
    val res = Wire(CreditedIO(chiselTypeOf(x.bits)))
    val irr = res.toReceiver(depth, flow)
    x.valid := irr.valid
    x.bits := irr.bits
    irr.ready := x.ready
    res
  }
}

class CreditedIOCounter(val init: Int, val depth: Int) {
  require (0 <= init)
  require (init <= depth)

  private val v = RegInit(init.U(log2Ceil(depth+1).W))
  private val nextV = WireInit(v)

  val value = v + 0.U
  val nextValue = nextV + 0.U

  def full: Bool = v === depth.U
  def empty: Bool = v === 0.U

  def update(credit: Bool, debit: Bool): Unit = {
    assert((!(credit && full) || debit) && (!(debit && empty) || credit))
    val next = Mux(credit, v + 1.U, v - 1.U)
    when (credit =/= debit) {
      nextV := next
      v := next
    }
  }

  def update(c: CreditedIO[_]): Unit = { update(c.credit, c.debit) }
}
