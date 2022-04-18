// See LICENSE.SiFive for license details.

package freechips.rocketchip.zk

import chisel3._
import chisel3.util._

object ZBK {
  val FN_Len = 3
  def FN_CLMUL  =  0.U(FN_Len.W)
  def FN_CLMULR =  1.U(FN_Len.W)
  def FN_CLMULH =  2.U(FN_Len.W)
  def FN_XPERM8 =  3.U(FN_Len.W)
  def FN_XPERM4 =  4.U(FN_Len.W)
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

  // xperm
  val rs1_bytes = asBytes(io.rs1)
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
  val clmul = clmul_rs2.asBools.zipWithIndex.map({
    case (b, i) => Mux(b, clmul_rs1 << i, 0.U)
  }).reduce(_ ^ _)(xLen-1,0)
  // clmul_raw also for clmulr
  val clmulr = Reverse(clmul)
  val clmulh = Cat(0.U(1.W), clmulr(xLen-1,1))

  // according to FN_xxx above
  io.rd := VecInit(Seq(
    clmul, clmulr, clmulh,
    xperm8, xperm4))(io.zbk_fn)
}
