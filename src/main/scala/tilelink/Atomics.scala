// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._

import TLMessages._
import TLPermissions._

class Atomics(params: TLBundleParameters) extends Module
{
  val io = new Bundle {
    val write    = Bool().flip // ignore opcode
    val a        = new TLBundleA(params).flip
    val data_in  = UInt(width = params.dataBits).flip
    val data_out = UInt(width = params.dataBits)
  }

  // Arithmetic, what to do
  val adder    = io.a.param(2)
  val unsigned = io.a.param(1)
  val take_max = io.a.param(0)

  val signBit = io.a.mask & Cat(UInt(1), ~io.a.mask >> 1)
  val inv_d = Mux(adder, io.data_in, ~io.data_in)
  val sum = (FillInterleaved(8, io.a.mask) & io.a.data) + inv_d
  def sign(x: UInt): Bool = (Cat(x.asBools.grouped(8).map(_.last).toList.reverse) & signBit).orR()
  val sign_a = sign(io.a.data)
  val sign_d = sign(io.data_in)
  val sign_s = sign(sum)
  val a_bigger_uneq = unsigned === sign_a // result if high bits are unequal
  val a_bigger = Mux(sign_a === sign_d, !sign_s, a_bigger_uneq)
  val pick_a = take_max === a_bigger

  // Logical, what to do
  val lut = Vec(Seq(
    UInt(0x6),   // XOR
    UInt(0xe),   // OR
    UInt(0x8),   // AND
    UInt(0xc)))( // SWAP
    io.a.param(1,0))
  val logical = Cat((io.a.data.asBools zip io.data_in.asBools).map { case (a, d) =>
    lut(Cat(a, d))
  }.reverse)

  // Operation, what to do? (0=d, 1=a, 2=sum, 3=logical)
  val select = Mux(io.write, UInt(1), Vec(Seq(
    UInt(1),   // PutFullData
    UInt(1),   // PutPartialData
    Mux(adder, UInt(2), Mux(pick_a, UInt(1), UInt(0))), // ArithmeticData
    UInt(3),   // LogicalData
    UInt(0),   // Get
    UInt(0),   // Hint
    UInt(0),   // AcquireBlock
    UInt(0)))( // AcquirePerm
    io.a.opcode))

  // Only the masked bytes can be modified
  val selects = io.a.mask.asBools.map(b => Mux(b, select, UInt(0)))
  io.data_out := Cat(selects.zipWithIndex.map { case (s, i) =>
    Vec(Seq(io.data_in, io.a.data, sum, logical).map(_((i + 1) * 8 - 1, i * 8)))(s)
  }.reverse)
}
