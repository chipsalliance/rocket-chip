// See LICENSE.SiFive for license details.

package freechips.rocketchip.zbk

import chisel3._
import chisel3.util._

object ZBK {
  val opcode = BitPat("b?????????????????????????0?01011")
  val ROR    = BitPat("b0110000??????????101?????0110011")
  val ROL    = BitPat("b0110000??????????001?????0110011")
  val RORI   = BitPat("b011000???????????101?????0010011")
  val ANDN   = BitPat("b0100000??????????111?????0110011")
  val ORN    = BitPat("b0100000??????????110?????0110011")
  val XNOR   = BitPat("b0100000??????????100?????0110011")
  val PACK   = BitPat("b0000100??????????100?????0110011")
  val PACKH  = BitPat("b0000100??????????111?????0110011")
  val ROLW   = BitPat("b0110000??????????001?????0111011")
  val RORW   = BitPat("b0110000??????????101?????0111011")
  val RORIW  = BitPat("b0110000??????????101?????0011011")
  val PACKW  = BitPat("b0000100??????????100?????0111011")
  val BREV8  = BitPat("b011010000111?????101?????0010011")
  val REV8   = BitPat("b011010?11000?????101?????0010011")
  val ZIP    = BitPat("b000010001111?????001?????0010011")
  val UNZIP  = BitPat("b000010001111?????101?????0010011")
  val CLMUL  = BitPat("b0000101??????????001?????0110011")
  val CLMULH = BitPat("b0000101??????????011?????0110011")
  val XPERM8 = BitPat("b0010100??????????100?????0110011")
  val XPERM4 = BitPat("b0010100??????????010?????0110011")
  val FN_Len = 4

  def FN_ROR   =  0.U(FN_Len.W)
  def FN_ROL   =  1.U(FN_Len.W)
  def FN_RORI  =  2.U(FN_Len.W)
  def FN_ANDN  =  3.U(FN_Len.W)
  def FN_ORN   =  4.U(FN_Len.W)
  def FN_XNOR  =  5.U(FN_Len.W)
  def FN_PACK  =  6.U(FN_Len.W)
  def FN_PACKH =  7.U(FN_Len.W)
  def FN_BREV8 =  8.U(FN_Len.W)
  def FN_REV8  =  9.U(FN_Len.W)
  def FN_ZIP   = 10.U(FN_Len.W)
  def FN_UNZIP = 11.U(FN_Len.W)
  def FN_CLMUL = 12.U(FN_Len.W)
  def FN_CLMULH= 13.U(FN_Len.W)
  def FN_XPERM8= 14.U(FN_Len.W)
  def FN_XPERM4= 15.U(FN_Len.W)
}

class ZBKInterface(xLen: Int) extends Bundle {
  val zbk_fn = Input(UInt(ZBK.FN_Len.W))
  val dw     = Input(Bool())
  val valid  = Input(Bool())
  val rs1    = Input(UInt(xLen.W))
  val rs2    = Input(UInt(xLen.W))
  val rd     = Output(UInt(xLen.W))
}

class ZBKImp(xLen: Int) extends Module {
  val io = IO(new ZBKInterface(xLen))

  // rotate
  val (shamt, shin_r) =
    if (xLen == 32) (io.rs2(4,0), io.rs1)
    else {
      require(xLen == 64)
      val shin_hi = Mux(io.dw, io.rs1(63,32), io.rs1(31,0))
      val shamt = Cat(io.rs2(5) & io.dw, io.rs2(4,0))
      (shamt, Cat(shin_hi, io.rs1(31,0)))
    }
  val shin = Mux(io.zbk_fn === ZBK.FN_ROR  || io.zbk_fn === ZBK.FN_RORI, shin_r, Reverse(shin_r))
  val shout_r = shin.rotateRight(shamt)(xLen-1,0)
  val shout_l = Reverse(shout_r)
  val shout_raw = Mux(io.zbk_fn === ZBK.FN_ROR || io.zbk_fn === ZBK.FN_RORI, shout_r, 0.U) |
                  Mux(io.zbk_fn === ZBK.FN_ROL,                              shout_l, 0.U)
  val shout =
    if (xLen == 32) shout_raw
    else {
      require(xLen == 64)
      def sext(in: UInt): UInt = {
        val in_hi_32 = Fill(32, in(31))
        Cat(in_hi_32, in)
      }
      Mux(io.dw, shout_raw, sext(shout_raw(31,0)))
    }

  // bool
  val rs2n = ~io.rs2
  val andn = io.rs1 & rs2n
  val orn = io.rs1 | rs2n
  val xnor = io.rs1 ^ rs2n

  // pack
  val pack =
    if (xLen == 32) Cat(io.rs2(xLen/2-1,0), io.rs1(xLen/2-1,0))
    else {
      require(xLen == 64)
      Mux(io.dw,
        Cat(io.rs2(xLen/2-1,0), io.rs1(xLen/2-1,0)),
        Cat(0.U((xLen/2).W), io.rs2(xLen/4-1,0), io.rs1(xLen/4-1,0)))
    }
  val packh = Cat(0.U((xLen-16).W), io.rs2(7,0), io.rs1(7,0))

  // rev
  val brev8 = VecInit(io.rs1.asBools.grouped(8).map(x => Reverse(VecInit(x).asUInt)).toSeq).asUInt
  val rev8 = VecInit(io.rs1.asBools.grouped(8).map(VecInit(_).asUInt).toSeq.reverse).asUInt

  // ziass ZBKImp(xLen: Int) extends Module {
  val unzip = if (xLen == 32) {
    val bits = io.rs1.asBools.zipWithIndex
    val lo = VecInit(bits filter { case (_, i) => i % 2 == 0 } map { case (b, _) => b }).asUInt
    val hi = VecInit(bits filter { case (_, i) => i % 2 != 0 } map { case (b, _) => b }).asUInt
    Cat(hi, lo)
  } else 0.U
  val zip = if (xLen == 32) {
    val lo = io.rs1(15,0).asBools
    val hi = io.rs1(31,16).asBools
    VecInit(lo.zip(hi).map { case (l, h) => VecInit(Seq(l, h)).asUInt }).asUInt
  } else 0.U

  // xperm
  val xperm8_rs1 = VecInit(io.rs1.asBools.grouped(8).map(VecInit(_).asUInt).toSeq)
  val xperm8 = VecInit(io.rs2.asBools.grouped(8).map(
    x => xperm8_rs1(VecInit(x).asUInt) // FIXME overflow should return 0!
  ).toSeq).asUInt
  val xperm4_rs1 = VecInit(io.rs1.asBools.grouped(4).map(VecInit(_).asUInt).toSeq)
  val xperm4 = VecInit(io.rs2.asBools.grouped(4).map(
    x => xperm4_rs1(VecInit(x).asUInt) // FIXME overflow should return 0!
  ).toSeq).asUInt

  // clmul
  val clmul_rs1 = Mux(io.zbk_fn === ZBK.FN_CLMUL, io.rs1, Reverse(io.rs1))
  val clmul_rs2 = Mux(io.zbk_fn === ZBK.FN_CLMUL, io.rs2, Reverse(io.rs2))
  val clmul_raw = clmul_rs2.asBools.zipWithIndex.map({
    case (b, i) => Mux(b, clmul_rs1 << i, 0.U)
  }).reduce(_ ^ _)(xLen-1,0)
  val clmul = Mux(io.zbk_fn === ZBK.FN_CLMUL, clmul_raw, Cat(0.U(1.W), Reverse(clmul_raw)(xLen-1,1))) // including clmulh

  // according to FN_xxx above
  io.rd := VecInit(Seq(
    shout, shout, shout,
    andn, orn, xnor,
    pack, packh,
    brev8, rev8,
    zip, unzip,
    clmul, clmul,
    xperm8, xperm4))(io.zbk_fn)
}
