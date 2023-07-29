// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._

class Atomics(params: TLBundleParameters) extends Module
{
  val io = IO(new Bundle {
    val write    = Flipped(Bool()) // ignore opcode
    val a        = Flipped(new TLBundleA(params))
    val data_in  = Flipped(UInt(params.dataBits.W))
    val data_out = UInt(params.dataBits.W)
  })

  // Arithmetic, what to do
  val adder    = io.a.param(2)
  val unsigned = io.a.param(1)
  val take_max = io.a.param(0)

  val signBit = io.a.mask & Cat(1.U, ~io.a.mask >> 1)
  val inv_d = Mux(adder, io.data_in, ~io.data_in)
  val sum = (FillInterleaved(8, io.a.mask) & io.a.data) + inv_d
  def sign(x: UInt): Bool = (Cat(x.asBools.grouped(8).map(_.last).toList.reverse) & signBit).orR
  val sign_a = sign(io.a.data)
  val sign_d = sign(io.data_in)
  val sign_s = sign(sum)
  val a_bigger_uneq = unsigned === sign_a // result if high bits are unequal
  val a_bigger = Mux(sign_a === sign_d, !sign_s, a_bigger_uneq)
  val pick_a = take_max === a_bigger

  // Logical, what to do
  val lut = VecInit(Seq(
    (0x6).U,   // XOR
    (0xe).U,   // OR
    (0x8).U,   // AND
    (0xc).U))( // SWAP
    io.a.param(1,0))
  val logical = Cat((io.a.data.asBools zip io.data_in.asBools).map { case (a, d) =>
    lut(Cat(a, d))
  }.reverse)

  // Operation, what to do? (0=d, 1=a, 2=sum, 3=logical)
  val select = Mux(io.write, 1.U, VecInit(Seq(
    1.U,   // PutFullData
    1.U,   // PutPartialData
    Mux(adder, 2.U, Mux(pick_a, 1.U, 0.U)), // ArithmeticData
    3.U,   // LogicalData
    0.U,   // Get
    0.U,   // Hint
    0.U,   // AcquireBlock
    0.U))( // AcquirePerm
    io.a.opcode))

  // Only the masked bytes can be modified
  val selects = io.a.mask.asBools.map(b => Mux(b, select, 0.U))
  io.data_out := Cat(selects.zipWithIndex.map { case (s, i) =>
    VecInit(Seq(io.data_in, io.a.data, sum, logical).map(_((i + 1) * 8 - 1, i * 8)))(s)
  }.reverse)
}
