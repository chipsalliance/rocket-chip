// See LICENSE.SiFive for license details.

package freechips.rocketchip.zk

import chisel3._
import chisel3.util._

object ZBK {
  val PACK   = BitPat("b0000100??????????100?????0110011")
  val PACKH  = BitPat("b0000100??????????111?????0110011")
  val PACKW  = BitPat("b0000100??????????100?????0111011")
  val BREV8  = BitPat("b011010000111?????101?????0010011")
  val REV8   = BitPat("b011010?11000?????101?????0010011")
  val ZIP    = BitPat("b000010001111?????001?????0010011")
  val UNZIP  = BitPat("b000010001111?????101?????0010011")
  val CLMUL  = BitPat("b0000101??????????001?????0110011")
  val CLMULH = BitPat("b0000101??????????011?????0110011")
  val XPERM8 = BitPat("b0010100??????????100?????0110011")
  val XPERM4 = BitPat("b0010100??????????010?????0110011")

  val FN_Len   = 4
  def FN_PACK  =  1.U(FN_Len.W)
  def FN_PACKH =  2.U(FN_Len.W)
  def FN_BREV8 =  3.U(FN_Len.W)
  def FN_REV8  =  4.U(FN_Len.W)
  def FN_ZIP   =  5.U(FN_Len.W)
  def FN_UNZIP =  6.U(FN_Len.W)
  def FN_CLMUL =  7.U(FN_Len.W)
  def FN_CLMULH=  8.U(FN_Len.W)
  def FN_XPERM8=  9.U(FN_Len.W)
  def FN_XPERM4= 10.U(FN_Len.W)
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

  // helper
  def asBytes(in: UInt): Vec[UInt] = VecInit(in.asBools.grouped(8).map(VecInit(_).asUInt).toSeq)
  def asNibbles(in: UInt): Vec[UInt] = VecInit(in.asBools.grouped(4).map(VecInit(_).asUInt).toSeq)
  def sext(in: UInt): UInt = {
    val in_hi_32 = Fill(32, in(31))
    Cat(in_hi_32, in)
  }

  // pack
  val pack =
    if (xLen == 32) Cat(io.rs2(xLen/2-1,0), io.rs1(xLen/2-1,0))
    else {
      require(xLen == 64)
      Mux(io.dw,
        Cat(io.rs2(xLen/2-1,0), io.rs1(xLen/2-1,0)),
        sext(Cat(io.rs2(xLen/4-1,0), io.rs1(xLen/4-1,0))))
    }
  val packh = Cat(0.U((xLen-16).W), io.rs2(7,0), io.rs1(7,0))

  // rev
  val rs1_bytes = asBytes(io.rs1)
  val brev8 = VecInit(rs1_bytes.map(Reverse(_)).toSeq).asUInt
  val rev8 = VecInit(rs1_bytes.reverse.toSeq).asUInt

  // zip
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
  // rs1_bytes defined above
  val rs2_bytes = asBytes(io.rs2)
  val rs1_nibbles = asNibbles(io.rs1)
  val rs2_nibbles = asNibbles(io.rs2)
  val xperm8 = VecInit(rs2_bytes.map(
    x => Mux(x(7,log2Ceil(xLen/8)).orR, 0.U(8.W), rs1_bytes(x)) // return 0 when x overflow
  ).toSeq).asUInt
  val xperm4 = VecInit(rs2_nibbles.map(
    x => if (xLen == 32) Mux(x(3,log2Ceil(xLen/4)).orR, 0.U(4.W), rs1_nibbles(x)) // return 0 when x overflow
    else {
      require(xLen == 64)
      rs1_nibbles(x)
    }
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
    pack, packh,
    brev8, rev8,
    zip, unzip,
    clmul, clmul,
    xperm8, xperm4))(io.zbk_fn)
}
