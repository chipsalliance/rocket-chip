// See LICENSE.SiFive for license details.

package freechips.rocketchip.zbk

//import Chisel._
import chisel3._
import chisel3.util.{BitPat, HasBlackBoxInline}
import chisel3.experimental.fromIntToIntParam

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
  val dw     = Input(Bool())
  val valid  = Input(Bool())
  val rs1    = Input(UInt(xLen.W))
  val rs2    = Input(UInt(xLen.W))
  val rd     = Output(UInt(xLen.W))
}

class ZBKImpv(xLen:Int) extends BlackBox {
  val io = IO(new ZBKInterface(xLen))
}

class ZBKImp(xLen: Int) extends BlackBox(Map("XLEN" -> xLen)) with HasBlackBoxInline {
  val io = IO(new ZBKInterface(xLen))

  setInline("ZBKImp.v",
    s"""
       #module ZBKImp #(parameter XLEN = 32) (
       #    input  [${ZBK.FN_Len - 1}:0] zbk_fn,
       #    input             dw,
       #    input             valid,
       #    input  [XLEN-1:0] rs1,
       #    input  [XLEN-1:0] rs2,
       #    output [XLEN-1:0] rd
       #    );
       #`define ror32(x,  imm,  l)  ({32{imm[4]}} & {l8``l[15:0], l8``l[31:16]}) | ({32{!imm[4]}} & l8``l[31:0]); \\
       #   wire [31:0] l8``l = ({32{imm[3]}} & {l4``l[ 7:0], l4``l[31: 8]}) | ({32{!imm[3]}} & l4``l[31:0]); \\
       #   wire [31:0] l4``l = ({32{imm[2]}} & {l2``l[ 3:0], l2``l[31: 4]}) | ({32{!imm[2]}} & l2``l[31:0]); \\
       #   wire [31:0] l2``l = ({32{imm[1]}} & {l1``l[ 1:0], l1``l[31: 2]}) | ({32{!imm[1]}} & l1``l[31:0]); \\
       #   wire [31:0] l1``l = ({32{imm[0]}} & {l0``l[   0], l0``l[31: 1]}) | ({32{!imm[0]}} & l0``l[31:0]); \\
       #   wire [31:0] l0``l = x
       #`define rol32(x,  imm,  l)  ({32{imm[4]}} & {l8``l[15:0], l8``l[31:16]}) | ({32{!imm[4]}} & l8``l[31:0]); \\
       #   wire [31:0] l8``l = ({32{imm[3]}} & {l4``l[23:0], l4``l[31:24]}) | ({32{!imm[3]}} & l4``l[31:0]);\\
       #   wire [31:0] l4``l = ({32{imm[2]}} & {l2``l[27:0], l2``l[31:28]}) | ({32{!imm[2]}} & l2``l[31:0]);\\
       #   wire [31:0] l2``l = ({32{imm[1]}} & {l1``l[29:0], l1``l[31:30]}) | ({32{!imm[1]}} & l1``l[31:0]);\\
       #   wire [31:0] l1``l = ({32{imm[0]}} & {l0``l[30:0], l0``l[31   ]}) | ({32{!imm[0]}} & l0``l[31:0]);\\
       #   wire [31:0] l0``l = x
       #`define rev(x, d, l)  rb``l;\\
       #   wire [7:0]  rb``l;\\
       #   for (genvar i = 0;  i < d; i = i + 1) begin: rev``l\\
       #       assign rb``l[i] = x[d-i-1]; \\
       #    end
       #
       #`define zip32(x, l)  {zh``l, zl``l}; \\
       #    wire [15:0]   zh``l, zl``l; \\
       #    for (genvar i = 0;  i < 16; i = i + 1) begin: zip32``l\\
       #        assign zh``l[i] = x[2*i + 1]; \\
       #        assign zl``l[i] = x[2*i    ];\\
       #    end
       #
       #`define unzip32(x, l) uz``l;\\
       #    wire [31:0]   uz``l;\\
       #    for (genvar i = 0;  i < 16; i = i + 1) begin: unzip32``l\\
       #        assign uz``l[2*i  ] = x[i];\\
       #        assign uz``l[2*i+1] = x[i+16];\\
       #    end
       #
       #`define lut4(x, i, l) res``l;\\
       #    wire [3:0]    res``l = (i == 0) ? x[ 3: 0] :\\
       #                           (i == 1) ? x[ 7: 4] :\\
       #                           (i == 2) ? x[11: 8] :\\
       #                           (i == 3) ? x[15:12] :\\
       #                           (i == 4) ? x[19:16] :\\
       #                           (i == 5) ? x[23:20] :\\
       #                           (i == 6) ? x[27:24] :\\
       #                           (i == 7) ? x[31:28] :\\
       #                         /*(i == 0)*/ 0
       #
       #`define lut8(x, i, l) res``l;\\
       #    wire [7:0]    res``l = (i == 0) ? x[ 7: 0] :\\
       #                           (i == 1) ? x[15: 8] :\\
       #                           (i == 2) ? x[23:16] :\\
       #                           (i == 3) ? x[31:24] :\\
       #                         /*(i == 0)*/ 0
       #
       #localparam [3:0] FN_ROR   = 4'd00;
       #localparam [3:0] FN_ROL   = 4'd01;
       #localparam [3:0] FN_RORI  = 4'd02;
       #localparam [3:0] FN_ANDN  = 4'd03;
       #localparam [3:0] FN_ORN   = 4'd04;
       #localparam [3:0] FN_XNOR  = 4'd05;
       #localparam [3:0] FN_PACK  = 4'd06;
       #localparam [3:0] FN_PACKH = 4'd07;
       #localparam [3:0] FN_BREV8 = 4'd08;
       #localparam [3:0] FN_REV8  = 4'd09;
       #localparam [3:0] FN_ZIP   = 4'd10;
       #localparam [3:0] FN_UNZIP = 4'd11;
       #localparam [3:0] FN_XPERM8= 4'd12;
       #localparam [3:0] FN_XPERM4= 4'd13;
       #wire    ror_sel = valid && (zbk_fn == FN_ROR);
       #wire    rol_sel = valid && (zbk_fn == FN_ROL);
       #wire   rori_sel = valid && (zbk_fn == FN_RORI);
       #wire   andn_sel = valid && (zbk_fn == FN_ANDN);
       #wire    orn_sel = valid && (zbk_fn == FN_ORN);
       #wire   xnor_sel = valid && (zbk_fn == FN_XNOR);
       #wire   pack_sel = valid && (zbk_fn == FN_PACK);
       #wire  packh_sel = valid && (zbk_fn == FN_PACKH);
       #wire  brev8_sel = valid && (zbk_fn == FN_BREV8);
       #wire   rev8_sel = valid && (zbk_fn == FN_REV8);
       #wire    zip_sel = valid && (zbk_fn == FN_ZIP);
       #wire  unzip_sel = valid && (zbk_fn == FN_UNZIP);
       #wire xperm8_sel = valid && (zbk_fn == FN_XPERM8);
       #wire xperm4_sel = valid && (zbk_fn == FN_XPERM4);
       #wire [ 4:0] shamt  = rs2[4:0];
       #wire [31:0] wror   = `ror32(rs1, shamt, iror32);
       #wire [31:0] wrol   = `rol32(rs1, shamt, irol32);
       #wire [31:0] wandn  = rs1 & (~rs2);
       #wire [31:0] worn   = rs1 | (~rs2);
       #wire [31:0] wxnor  = rs1 ^ (~rs2);
       #wire [31:0] wpack  = {       rs2[15:0], rs1[15:0]};
       #wire [31:0] wpackh = {16'd0, rs2[ 7:0], rs1[ 7:0]};
       #
       #wire [ 7:0] brev8_0 = `rev(rs1[ 7: 0], 8, irev8_0)
       #wire [ 7:0] brev8_1 = `rev(rs1[15: 8], 8, irev8_1)
       #wire [ 7:0] brev8_2 = `rev(rs1[23:16], 8, irev8_2)
       #wire [ 7:0] brev8_3 = `rev(rs1[31:24], 8, irev8_3)
       #wire [31:0] wbrev8  = {brev8_3, brev8_2, brev8_1, brev8_0};
       #
       #wire [31:0] wrev8 = {rs1[ 7: 0], rs1[15: 8], rs1[23:16], rs1[31:24]};
       #
       #wire [31:0] wzip   = `zip32(  rs1, izip32)
       #wire [31:0] wunzip = `unzip32(rs1, iunzip32)
       #
       #wire [ 7:0] wlut8_0 = `lut8(rs1, rs2[ 7: 0], ilut8_0);
       #wire [ 7:0] wlut8_1 = `lut8(rs1, rs2[15: 8], ilut8_1);
       #wire [ 7:0] wlut8_2 = `lut8(rs1, rs2[23:16], ilut8_2);
       #wire [ 7:0] wlut8_3 = `lut8(rs1, rs2[31:24], ilut8_3);
       #wire [31:0] wxperm8 = {wlut8_3, wlut8_2, wlut8_1, wlut8_0};
       #
       #wire [ 3:0] wlut4_0 = `lut4(rs1, rs2[ 3: 0], ilut4_0);
       #wire [ 3:0] wlut4_1 = `lut4(rs1, rs2[ 7: 4], ilut4_1);
       #wire [ 3:0] wlut4_2 = `lut4(rs1, rs2[11: 8], ilut4_2);
       #wire [ 3:0] wlut4_3 = `lut4(rs1, rs2[15:12], ilut4_3);
       #wire [ 3:0] wlut4_4 = `lut4(rs1, rs2[19:16], ilut4_4);
       #wire [ 3:0] wlut4_5 = `lut4(rs1, rs2[23:20], ilut4_5);
       #wire [ 3:0] wlut4_6 = `lut4(rs1, rs2[27:24], ilut4_6);
       #wire [ 3:0] wlut4_7 = `lut4(rs1, rs2[31:28], ilut4_7);
       #wire [31:0] wxperm4 = {wlut4_7, wlut4_6, wlut4_5, wlut4_4, wlut4_3, wlut4_2, wlut4_1, wlut4_0};
       #
       #assign rd  =       {32{   ror_sel}} & wror   |
       #                   {32{   rol_sel}} & wrol   |
       #                   {32{  rori_sel}} & wror   |
       #                   {32{  andn_sel}} & wandn  |
       #                   {32{   orn_sel}} & worn   |
       #                   {32{  xnor_sel}} & wxnor  |
       #                   {32{  pack_sel}} & wpack  |
       #                   {32{ packh_sel}} & wpackh |
       #                   {32{ brev8_sel}} & wbrev8 |
       #                   {32{  rev8_sel}} & wrev8  |
       #                   {32{   zip_sel}} & wzip   |
       #                   {32{ unzip_sel}} & wunzip |
       #                   {32{xperm8_sel}} & wxperm8|
       #                   {32{xperm4_sel}} & wxperm4;
       #
       #endmodule
     """.stripMargin('#'))
}