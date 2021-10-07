// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

/** A [[Bundle]] that adds `earlyValid` and `lateCancel` bits to some data.
  * This indicates that the user expects a "ValidCancel" interface between a producer and a consumer.
  * Here, the producer asserts the `earlyValid` bit when data on the `bits` line might contain valid data.
  * The producer also asserts the `lateCancel` bit low to confirm the valid, or high to squash the valid.
  * `lateCancel` is a DontCare when `earlyValid` is low.
  * This differs from [[ReadyValidCancel]] as there is no `ready` line that the consumer can use
  * to put back pressure on the producer.
  * @param gen the type of data to be wrapped in Valid/Cancel
  */
class ValidCancel[+T <: Data](gen: T) extends Bundle {
  val earlyValid = Output(Bool())
  val lateCancel = Output(Bool())
  val bits       = Output(gen)
  def validQual(): Bool = earlyValid && !lateCancel

  /** Down-converts a ValidCancel output to a Valid bundle, dropping early/late timing split. */
  def andNotCancel(): Valid[T] = {
    val out = Wire(new Valid(gen))
    out.valid := validQual()
    out.bits  := bits
    out
  }
}

object ValidCancel {
  /** Wraps some Data with a ValidCancel interface. */
  def apply[T <: Data](gen: T): ValidCancel[T] = new ValidCancel(gen)
}


/** A [[Bundle]] containing 'earlyValid', 'lateCancel', and 'ready' signals that handshake
  * the transfer of data stored in the 'bits' subfield.
  * The base protocol implied by the directionality is that
  * the producer uses the interface as-is (outputs bits)
  * while the consumer uses the flipped interface (inputs bits).
  * @param gen the type of data to be wrapped in Ready/Valid/Cancel
  */
class ReadyValidCancel[+T <: Data](gen: T) extends ValidCancel(gen)
{
  val ready = Input(Bool())
  def mightFire(): Bool = ready && earlyValid
  def fire():      Bool = ready && validQual()

  /** Down-converts a ReadyValidCancel output to a DecoupledIO bundle, dropping early/late timing split. */
  def asDecoupled(): DecoupledIO[T] = {
    val out = Wire(new DecoupledIO(gen))
    out.valid := validQual()
    out.bits  := bits
    ready := out.ready
    out
  }
}

object ReadyValidCancel {

  /** Wraps some Data with a ReadyValidCancel interface. */
  def apply[T <: Data](gen: T): ReadyValidCancel[T] = new ReadyValidCancel(gen)

  /** Up-converts a ReadyValid to a ReadyValidCancel, assuming conservative timing. */
  def apply[T <: Data](in: ReadyValidIO[T]): ReadyValidCancel[T] = {
    val out = Wire(new ReadyValidCancel(chiselTypeOf(in.bits)))
    out.earlyValid := in.valid
    out.lateCancel := false.B
    out.bits := in.bits
    in.ready := out.ready
    out
  }

}


/** Based on Chisel3 Arbiter.
  * Sequence n ReadyValidCancel producers into 1 consumer.
  * If round-robin parameter is false, then priority is given to lower producer.
  * If round-robin parameter is true, then rotate priority when output "mightFire" (earlyValid && ready).
  *
  * @param gen data type
  * @param n number of inputs producers
  * @param rr round-robin
  *
  * @example {{{
  * val arb = Module(new ReadyValidCancelArbiter(UInt(), 2))
  * arb.io.in(0) <> producer0.io.out
  * arb.io.in(1) <> producer1.io.out
  * consumer.io.in <> arb.io.out
  * }}}
  */
class ReadyValidCancelRRArbiter[T <: Data](gen: T, n: Int, rr: Boolean) extends Module {
  val io = IO(new Bundle{
    val in  = Flipped(Vec(n, ReadyValidCancel(gen)))
    val out = ReadyValidCancel(gen)
  })

  val inputEarlyValidVec = Wire(Vec(n, Bool()))
  val grantVec           = Wire(Vec(n, Bool()))
  val selectDcd          = Wire(Vec(n, Bool()))

  inputEarlyValidVec := io.in.map(_.earlyValid)

  private val nextSelectEnc = Wire(UInt(log2Ceil(n).W))
  private val selectEnc_in  = Wire(UInt(log2Ceil(n).W))
  private val selectEnc_en  = Wire(Bool())
  val selectEnc_q = RegEnable(selectEnc_in, 0.U, selectEnc_en)
  selectEnc_en := inputEarlyValidVec.orR || selectEnc_q.orR
  selectEnc_in := Mux(io.out.earlyValid,
                      Mux(io.out.ready,
                          nextSelectEnc,
                          selectEnc_q),
                      0.U)

  private val grantVecVec      = Wire(Vec(n, Vec(n, Bool())))
  private val nextSelectEncVec = Wire(Vec(n, UInt(log2Ceil(n).W)))
  for (i <- 0 until n) {
    grantVecVec(i)(i) := true.B
    for (j <- 1 until n) {
      val k = (i + j) % n
      grantVecVec(i)(k) := !(inputEarlyValidVec.rotate(i).take(j).orR)
    }
    if (isPow2(n)) {
      nextSelectEncVec(i) := i.U +& PriorityEncoder(inputEarlyValidVec.rotate(i) & ~1.U(n.W).asBools)
    } else {
      nextSelectEncVec(i) := (i.U +& PriorityEncoder(inputEarlyValidVec.rotate(i) & ~1.U(n.W).asBools)) % n.U
    }
  }

  if (rr) {
    grantVec          := grantVecVec(selectEnc_q)
    io.out.lateCancel := Mux1H(selectDcd, io.in.map(_.lateCancel))
    io.out.bits       := Mux1H(selectDcd, io.in.map(_.bits))
    nextSelectEnc     := nextSelectEncVec(selectEnc_q)
  } else {
    grantVec          := grantVecVec.head
    io.out.lateCancel := PriorityMux(grantVec.reverse, io.in.reverse.map(_.lateCancel))
    io.out.bits       := PriorityMux(grantVec.reverse, io.in.reverse.map(_.bits))
    nextSelectEnc     := 0.U
  }

  selectDcd := inputEarlyValidVec & grantVec

  io.out.earlyValid := inputEarlyValidVec.orR
  for (i <- 0 until n) {
    io.in(i).ready := io.out.ready && grantVec(i)
  }

  // assertions
  when (io.out.earlyValid) {
    assert(PopCount(selectDcd) === 1.U, "arbiter select should be one-hot when earlyValid")
  }
  if (rr) {
    when (io.out.earlyValid) {
      assert(selectEnc_q < n.U, "arbiter round-robin select out of range")
    }
    when (io.in(selectEnc_q).mightFire && io.in.map(i => i.earlyValid && !i.ready).orR) {
      assert(selectEnc_in =/= selectEnc_q, "arbiter round-robin select did not advance")
    }
  }
}

class ReadyValidCancelArbiter[T <: Data](gen: T, n: Int)
  extends ReadyValidCancelRRArbiter(gen, n, false)
