// SPDX-License-Identifier: Apache-2.0

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._

object ZKS {
  val FN_Len      = 2
  def FN_SM4ED    = 0.U(FN_Len.W)
  def FN_SM4KS    = 1.U(FN_Len.W)
  def FN_SM3P0    = 2.U(FN_Len.W)
  def FN_SM3P1    = 3.U(FN_Len.W)
}

class CryptoSMInterface(xLen: Int) extends Bundle {
  val zks_fn = Input(UInt(ZKS.FN_Len.W))
  val valid  = Input(Bool())
  val bs     = Input(UInt(2.W))
  val rs1    = Input(UInt(xLen.W))
  val rs2    = Input(UInt(xLen.W))
  val rd     = Output(UInt(xLen.W))
}

class CryptoSM(xLen:Int) extends Module {
  val io = IO(new CryptoSMInterface(xLen))

  // helper
  def sext(in: UInt): UInt = if (xLen == 32) in
    else {
      require(xLen == 64)
      val in_hi_32 = Fill(32, in(31))
      Cat(in_hi_32, in)
    }
  def asBytes(in: UInt): Vec[UInt] = VecInit(in.asBools.grouped(8).map(VecInit(_).asUInt).toSeq)

  // sm4
  // dynamic selection should be merged into aes rv32 logic!
  val si = asBytes(io.rs2(31,0))(io.bs)
  // this can also be merged into AESSbox for rv32
  val so = SBoxSM4Out(SBoxMid(SBoxSM4In(si)))
  val x = Cat(0.U(24.W), so)
  val y = Mux(io.zks_fn === ZKS.FN_SM4ED,
    x ^ (x << 8) ^ (x << 2) ^ (x << 18) ^ ((x & 0x3F.U) << 26) ^ ((x & 0xC0.U) << 10),
    x ^ ((x & 0x7.U) << 29) ^ ((x & 0xFE.U) << 7) ^ ((x & 0x1.U) << 23) ^ ((x & 0xF8.U) << 13))(31,0)
  // dynamic rotate should be merged into aes rv32 logic!
  // Vec rightRotate = UInt rotateLeft as Vec is big endian while UInt is little endian
  val z = barrel.rightRotate(asBytes(y), io.bs).asUInt
  val sm4 = sext(z ^ io.rs1(31,0))

  // sm3
  val r1 = io.rs1(31,0)
  val sm3 = sext(Mux(io.zks_fn === ZKS.FN_SM3P0,
    r1 ^ r1.rotateLeft(9) ^ r1.rotateLeft(17),
    r1 ^ r1.rotateLeft(15) ^ r1.rotateLeft(23)))

  // according to FN_xxx above
  io.rd := VecInit(Seq(
    sm4, sm4,
    sm3, sm3))(io.zks_fn)
}
