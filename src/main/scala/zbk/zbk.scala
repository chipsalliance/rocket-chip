// See LICENSE.SiFive for license details.

package freechips.rocketchip.zbk

import chisel3._
import chisel3.util.BitPat

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
  val REV8   = BitPat("b011010011000?????101?????0010011")
  val ZIP    = BitPat("b000010001111?????001?????0010011")
  val UNZIP  = BitPat("b000010001111?????101?????0010011")
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
  def FN_XPERM8= 12.U(FN_Len.W)
  def FN_XPERM4= 13.U(FN_Len.W)
}

class ZBKInterface(xLen: Int) extends Bundle {
  val zbk_fn = Input(UInt(ZBK.FN_Len.W))
  val dw     = Input(UInt(SZ_DW.W))
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
      val shin_hi = Mux(io.dw === DW_64, io.rs1(63,32), io.rs1(31,0))
      val shamt = Cat(io.rs2(5) & (io.dw === DW_64), io.rs2(4,0))
      (shamt, Cat(shin_hi, io.rs1(31,0)))
    }
  val shin = Mux(io.fn === ZKB.FN_ROR  || io.fn === ZKB.FN_RORI, shin_r, Reverse(shin_r))
  val shout_r = shin.rotateRight(shamt)(xLen-1,0)
  val shout_l = Reverse(shout_r)
  val shout_raw = Mux(io.fn === FN_ROR || io.fn === FN_ROR, shout_r, UInt(0)) |
                  Mux(io.fn === FN_SL,                      shout_l, UInt(0))
  val shout =
    if (xLen == 32) shout_raw
    else {
      require(xLen == 64)
      def sext(in: UInt(32.W)) = {
        val in_hi_32 = Fill(32, in(31))
        Cat(in_hi_32, in)
      }
      Mux(io.dw == DW_64, shout_raw, sext(shout_raw(31,0)))
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
      Mux(io.dw == DW_64,
        Cat(io.rs2(xLen/2-1,0), io.rs1(xLen/2-1,0)),
        Cat(Seq(0.U((xLen/2).W), io.rs2(xLen/4-1,0), io.rs1(xLen/4-1,0))))
    }
  val packh = Cat(0.U((xLen-16).W), Seq(io.rs2(7,0), io.rs1(7,0))

  // rev
  val brev8 = VecInit(io.rs1.asBools.grouped(8).map(Reverse(VecInit(_).asUInt))).asUInt
  val rev8 = VecInit(io.rs1.asBools.grouped(8).map(VecInit(_).asUInt).reverse).asUInt
}
