// See LICENSE.SiFive for license details.

package freechips.rocketchip.zk

import chisel3._
import chisel3.util.{BitPat, HasBlackBoxInline}

object ZBK {
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
  val REV8_32= BitPat("b011010011000?????101?????0010011")
  val REV8_64= BitPat("b011010111000?????101?????0010011")
  val ZIP    = BitPat("b000010001111?????001?????0010011")
  val UNZIP  = BitPat("b000010001111?????101?????0010011")
  val CLMUL  = BitPat("b0000101??????????001?????0110011")
  val CLMULH = BitPat("b0000101??????????011?????0110011")
  val XPERM8 = BitPat("b0010100??????????100?????0110011")
  val XPERM4 = BitPat("b0010100??????????010?????0110011")

  val FN_Len   = 4
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

class ZBKImp(xLen:Int) extends Module {
  val io = IO(new ZBKInterface(xLen))

  val zbkb_res = if (xLen == 64) {
    val zbkb_u = Module(new zbkb64)
    zbkb_u.io.zbk_fn := io.zbk_fn
    zbkb_u.io.dw     := io.dw
    zbkb_u.io.valid  := io.valid
    zbkb_u.io.rs1    := io.rs1
    zbkb_u.io.rs2    := io.rs2
    zbkb_u.io.rd
  } else if (xLen == 32) {
    val zbkb_u = Module(new zbkb32)
    zbkb_u.io.zbk_fn := io.zbk_fn
    zbkb_u.io.dw     := io.dw
    zbkb_u.io.valid  := io.valid
    zbkb_u.io.rs1    := io.rs1
    zbkb_u.io.rs2    := io.rs2
    zbkb_u.io.rd
  } else 0.U

  val zbkc_res = if (xLen == 64) {
    val zbkc_u = Module(new zbkc64)
    zbkc_u.io.zbk_fn := io.zbk_fn
    zbkc_u.io.dw     := io.dw
    zbkc_u.io.valid  := io.valid
    zbkc_u.io.rs1    := io.rs1
    zbkc_u.io.rs2    := io.rs2
    zbkc_u.io.rd
  } else if (xLen == 32) {
    val zbkc_u = Module(new zbkc32)
    zbkc_u.io.zbk_fn := io.zbk_fn
    zbkc_u.io.dw     := io.dw
    zbkc_u.io.valid  := io.valid
    zbkc_u.io.rs1    := io.rs1
    zbkc_u.io.rs2    := io.rs2
    zbkc_u.io.rd
  } else 0.U

  val zbkx_res = if (xLen == 64) {
    val zbkx_u = Module(new zbkx64)
    zbkx_u.io.zbk_fn := io.zbk_fn
    zbkx_u.io.dw     := io.dw
    zbkx_u.io.valid  := io.valid
    zbkx_u.io.rs1    := io.rs1
    zbkx_u.io.rs2    := io.rs2
    zbkx_u.io.rd
  } else if (xLen == 32) {
    val zbkx_u = Module(new zbkx32)
    zbkx_u.io.zbk_fn := io.zbk_fn
    zbkx_u.io.dw     := io.dw
    zbkx_u.io.valid  := io.valid
    zbkx_u.io.rs1    := io.rs1
    zbkx_u.io.rs2    := io.rs2
    zbkx_u.io.rd
  } else 0.U

  io.rd := zbkb_res | zbkc_res | zbkx_res
  val t = poly16_mul(0.U, 0.U) //pseudo object call to include the verilog file of the poly16_mul module
}

class zbkb32 extends BlackBox with HasBlackBoxInline {
  val io = IO(new ZBKInterface(32))
  setInline("zbkb32.v",
    s"""
       #module zbkb32  (
       #input  [ 3:0] zbk_fn,
       #input         dw,
       #input         valid,
       #input  [31:0] rs1,
       #input  [31:0] rs2,
       #output [31:0] rd);
       #
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
       #`define unzip32(x, l)  {zh``l, zl``l}; \\
       #    wire [15:0]   zh``l, zl``l; \\
       #    for (genvar i = 0;  i < 16; i = i + 1) begin: zip32``l\\
       #        assign zh``l[i] = x[2*i + 1]; \\
       #        assign zl``l[i] = x[2*i    ];\\
       #    end
       #
       #`define zip32(x, l) uz``l;\\
       #    wire [31:0]   uz``l;\\
       #    for (genvar i = 0;  i < 16; i = i + 1) begin: unzip32``l\\
       #        assign uz``l[2*i  ] = x[i];\\
       #        assign uz``l[2*i+1] = x[i+16];\\
       #    end
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
       #
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
       #
       #wire [ 4:0] shamt  = rs2[4:0];
       #wire [31:0] wror   = `ror32(rs1, shamt, iror32);
       #wire [31:0] wrol   = `rol32(rs1, shamt, irol32);
       #wire [31:0] wandn  = rs1 & (~rs2);
       #wire [31:0] worn   = rs1 | (~rs2);
       #wire [31:0] wxnor  = rs1 ^ (~rs2);
       #wire [31:0] wpack  = {       rs2[15:0], rs1[15:0]};
       #wire [31:0] wpackh = {16'd0, rs2[ 7:0], rs1[ 7:0]};
       #
       #wire [ 7:0] rs1_b0  = rs1[ 7: 0];
       #wire [ 7:0] rs1_b1  = rs1[15: 8];
       #wire [ 7:0] rs1_b2  = rs1[23:16];
       #wire [ 7:0] rs1_b3  = rs1[31:24];
       #
       #wire [ 7:0] brev8_0 = `rev(rs1_b0, 8, irev8_0)
       #wire [ 7:0] brev8_1 = `rev(rs1_b1, 8, irev8_1)
       #wire [ 7:0] brev8_2 = `rev(rs1_b2, 8, irev8_2)
       #wire [ 7:0] brev8_3 = `rev(rs1_b3, 8, irev8_3)
       #wire [31:0] wbrev8  = {brev8_3, brev8_2, brev8_1, brev8_0};
       #
       #wire [31:0] wrev8   = {rs1_b0,  rs1_b1,  rs1_b2,  rs1_b3};
       #
       #wire [31:0] wzip   = `zip32(  rs1, izip32)
       #wire [31:0] wunzip = `unzip32(rs1, iunzip32)
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
       #                   {32{ unzip_sel}} & wunzip ;
       #
       #endmodule
     """.stripMargin('#'))
}

class zbkb64 extends BlackBox with HasBlackBoxInline {
  val io = IO(new ZBKInterface(64))
  setInline("zbkb64.v",
    s"""
       #module zbkb64  (
       #input  [ 3:0] zbk_fn,
       #input         dw,
       #input         valid,
       #input  [63:0] rs1,
       #input  [63:0] rs2,
       #output [63:0] rd);
       #`define ror64(x,  imm,  l)  ({64{imm[5]}} & {l16``l[31:0],l16``l[63:32]}) | ({64{!imm[5]}} &l16``l[63:0]); \\
       #        wire [63:0] l16``l= ({64{imm[4]}} & { l8``l[15:0], l8``l[63:16]}) | ({64{!imm[4]}} & l8``l[63:0]); \\
       #        wire [63:0] l8``l = ({64{imm[3]}} & { l4``l[ 7:0], l4``l[63: 8]}) | ({64{!imm[3]}} & l4``l[63:0]); \\
       #        wire [63:0] l4``l = ({64{imm[2]}} & { l2``l[ 3:0], l2``l[63: 4]}) | ({64{!imm[2]}} & l2``l[63:0]); \\
       #        wire [63:0] l2``l = ({64{imm[1]}} & { l1``l[ 1:0], l1``l[63: 2]}) | ({64{!imm[1]}} & l1``l[63:0]); \\
       #        wire [63:0] l1``l = ({64{imm[0]}} & { l0``l[   0], l0``l[63: 1]}) | ({64{!imm[0]}} & l0``l[63:0]); \\
       #        wire [63:0] l0``l = x
       #
       #`define rol64(x,  imm,  l)  ({64{imm[5]}} & {l16``l[31:0],l16``l[63:32]}) | ({64{!imm[5]}} &l16``l[63:0]); \\
       #        wire [63:0] l16``l= ({64{imm[4]}} & { l8``l[47:0], l8``l[63:48]}) | ({64{!imm[4]}} & l8``l[63:0]); \\
       #        wire [63:0] l8``l = ({64{imm[3]}} & { l4``l[55:0], l4``l[63:56]}) | ({64{!imm[3]}} & l4``l[63:0]); \\
       #        wire [63:0] l4``l = ({64{imm[2]}} & { l2``l[59:0], l2``l[63:60]}) | ({64{!imm[2]}} & l2``l[63:0]); \\
       #        wire [63:0] l2``l = ({64{imm[1]}} & { l1``l[61:0], l1``l[63:62]}) | ({64{!imm[1]}} & l1``l[63:0]); \\
       #        wire [63:0] l1``l = ({64{imm[0]}} & { l0``l[62:0], l0``l[63   ]}) | ({64{!imm[0]}} & l0``l[63:0]); \\
       #        wire [63:0] l0``l = x
       #
       #`define rev(x, d, l)  rb``l; \\
       #    wire [7:0]  rb``l; \\
       #    for (genvar i = 0;  i < d; i = i + 1) begin: rev``l \\
       #        assign rb``l[i] = x[d-i-1]; \\
       #    end
       #
       #localparam [3:0] FN_ROR   = 4'd00;
       #localparam [3:0] FN_ROL   = 4'd01;
       #localparam [3:0] FN_RORI  = 4'd02;
       #localparam [3:0] FN_RORW  = 4'd00;
       #localparam [3:0] FN_ROLW  = 4'd01;
       #localparam [3:0] FN_RORIW = 4'd02;
       #localparam [3:0] FN_ANDN  = 4'd03;
       #localparam [3:0] FN_ORN   = 4'd04;
       #localparam [3:0] FN_XNOR  = 4'd05;
       #localparam [3:0] FN_PACK  = 4'd06;
       #localparam [3:0] FN_PACKW = 4'd06;
       #localparam [3:0] FN_PACKH = 4'd07;
       #localparam [3:0] FN_BREV8 = 4'd08;
       #localparam [3:0] FN_REV8  = 4'd09;
       #
       #wire   ror_sel = valid && (zbk_fn == FN_ROR)  && ( dw);
       #wire   rol_sel = valid && (zbk_fn == FN_ROL)  && ( dw);
       #wire  rori_sel = valid && (zbk_fn == FN_RORI) && ( dw);
       #wire  rorw_sel = valid && (zbk_fn == FN_ROR)  && (~dw);
       #wire  rolw_sel = valid && (zbk_fn == FN_ROL)  && (~dw);
       #wire roriw_sel = valid && (zbk_fn == FN_RORI) && (~dw);
       #wire  andn_sel = valid && (zbk_fn == FN_ANDN);
       #wire   orn_sel = valid && (zbk_fn == FN_ORN);
       #wire  xnor_sel = valid && (zbk_fn == FN_XNOR);
       #wire  pack_sel = valid && (zbk_fn == FN_PACK) && ( dw);
       #wire packw_sel = valid && (zbk_fn == FN_PACK) && (~dw);
       #wire packh_sel = valid && (zbk_fn == FN_PACKH);
       #wire brev8_sel = valid && (zbk_fn == FN_BREV8);
       #wire  rev8_sel = valid && (zbk_fn == FN_REV8);
       #
       #wire      roxw = rorw_sel | rolw_sel | roriw_sel;
       #
       #wire [ 5:0] shamt  = roxw? {        1'b0 , rs2[ 4:0]} : rs2[5:0];
       #wire [63:0] win1   = roxw? {rs1[31:0], rs1[31:0]} : rs1;
       #
       #wire [63:0] wror   = `ror64(win1, shamt, iror64);
       #wire [63:0] wrol   = `rol64(win1, shamt, irol64);
       #wire [63:0] wandn  = rs1 & (~rs2);
       #wire [63:0] worn   = rs1 | (~rs2);
       #wire [63:0] wxnor  = rs1 ^ (~rs2);
       #wire [63:0] wpack  = {       rs2[31:0], rs1[31:0]};
       #wire [63:0] wpackw = {32'd0, rs2[15:0], rs1[15:0]};
       #wire [63:0] wpackh = {48'd0, rs2[ 7:0], rs1[ 7:0]};
       #
       #wire [ 7:0] rs1_b0  = rs1[ 7: 0];
       #wire [ 7:0] rs1_b1  = rs1[15: 8];
       #wire [ 7:0] rs1_b2  = rs1[23:16];
       #wire [ 7:0] rs1_b3  = rs1[31:24];
       #wire [ 7:0] rs1_b4  = rs1[39:32];
       #wire [ 7:0] rs1_b5  = rs1[47:40];
       #wire [ 7:0] rs1_b6  = rs1[55:48];
       #wire [ 7:0] rs1_b7  = rs1[63:56];
       #
       #wire [ 7:0] brev8_0 = `rev(rs1_b0, 8, irev8_0)
       #wire [ 7:0] brev8_1 = `rev(rs1_b1, 8, irev8_1)
       #wire [ 7:0] brev8_2 = `rev(rs1_b2, 8, irev8_2)
       #wire [ 7:0] brev8_3 = `rev(rs1_b3, 8, irev8_3)
       #wire [ 7:0] brev8_4 = `rev(rs1_b4, 8, irev8_4)
       #wire [ 7:0] brev8_5 = `rev(rs1_b5, 8, irev8_5)
       #wire [ 7:0] brev8_6 = `rev(rs1_b6, 8, irev8_6)
       #wire [ 7:0] brev8_7 = `rev(rs1_b7, 8, irev8_7)
       #
       #wire [63:0] wbrev8  = {brev8_7, brev8_6, brev8_5, brev8_4,
       #                       brev8_3, brev8_2, brev8_1, brev8_0};
       #wire [63:0] wrev8   = {rs1_b0,  rs1_b1,  rs1_b2,  rs1_b3,
       #                       rs1_b4,  rs1_b5,  rs1_b6,  rs1_b7 };
       #
       #assign         rd   = {64{  ror_sel}} & wror  |
       #                      {64{  rol_sel}} & wrol  |
       #                      {64{ rori_sel}} & wror  |
       #                      {64{ rorw_sel}} & {32'd0, wror[31:0]} |
       #                      {64{ rolw_sel}} & {32'd0, wrol[31:0]} |
       #                      {64{roriw_sel}} & {32'd0, wror[31:0]} |
       #                      {64{ andn_sel}} & wandn |
       #                      {64{  orn_sel}} & worn  |
       #                      {64{ xnor_sel}} & wxnor |
       #                      {64{ pack_sel}} & wpack |
       #                      {64{packw_sel}} & wpackw|
       #                      {64{packh_sel}} & wpackh|
       #                      {64{brev8_sel}} & wbrev8|
       #                      {64{ rev8_sel}} & wrev8 ;
       #
       #endmodule
       """.stripMargin('#'))
}

class zbkc32 extends BlackBox with HasBlackBoxInline {
  val io = IO(new ZBKInterface(32))
  setInline("zbkc32.v",
    s"""
       #module zbkc32  (
       #input  [ 3:0] zbk_fn,
       #input         dw,
       #input         valid,
       #input  [31:0] rs1,
       #input  [31:0] rs2,
       #output [31:0] rd);
       #localparam [3:0] FN_CLMUL = 4'd12;
       #localparam [3:0] FN_CLMULH= 4'd13;
       #
       #wire clmull_sel = valid && (zbk_fn == FN_CLMUL );
       #wire clmulh_sel = valid && (zbk_fn == FN_CLMULH);
       #
       #wire [15:0] lhs0 = clmulh_sel? rs1[31:16] : rs1[15: 0];
       #wire [15:0] rhs0 = clmulh_sel? rs2[31:16] : rs2[15: 0];
       #
       #wire [15:0] lhs1 = rs1[15: 0];
       #wire [15:0] rhs1 = rs2[31:16];
       #
       #wire [15:0] lhs2 = rs1[31:16];
       #wire [15:0] rhs2 = rs2[15: 0];
       #
       #wire [31:0]  polymul0, polymul1, polymul2;
       #poly16_mul mul16_ins0(lhs0, rhs0, polymul0);
       #poly16_mul mul16_ins1(lhs1, rhs1, polymul1);
       #poly16_mul mul16_ins2(lhs2, rhs2, polymul2);
       #
       #wire [31:0] clmulm  = polymul1 ^ polymul2;
       #
       #wire [31:0] wclmulh = {polymul0[31:16], (polymul0[15: 0] ^ clmulm[31:16])                 };
       #wire [31:0] wclmull = {                 (polymul0[31:16] ^ clmulm[15: 0]), polymul0[15: 0]};
       #
       #
       #assign rd  =       {32{clmull_sel}} & wclmull|
       #                   {32{clmulh_sel}} & wclmulh;
       #
       #endmodule
       """.stripMargin('#'))
  }
class zbkc64 extends BlackBox with HasBlackBoxInline {
  val io = IO(new ZBKInterface(64))
  setInline("zbkc64.v",
    s"""
       #module zbkc64  (
       #input  [ 3:0] zbk_fn,
       #input         dw,
       #input         valid,
       #input  [63:0] rs1,
       #input  [63:0] rs2,
       #output [63:0] rd);
       #localparam [3:0] FN_CLMUL = 4'd12;
       #localparam [3:0] FN_CLMULH= 4'd13;
       #
       #wire clmull_sel = valid && (zbk_fn == FN_CLMUL );
       #wire clmulh_sel = valid && (zbk_fn == FN_CLMULH);
       #/*
       #a0, a1, a2, a3 are the 16 bit parts of rs1
       #b0, b1, b2, b3 are the 16 bit parts of rs2
       #
       #(a0 + a1.x^16 + a2.x^32 + a3.x^48) .* (b0 + b1.x^16 + b2.x^32 + b3.x^48)
       #
       #         polymul0     polymul1  polymul2     polymul3 polymul4 polymul5
       #clmull:   a0.b0      + (a0.b1 + a1.b0).x^16 + (a0.b2 + a1.b1 + a2.b0).x^32
       #clmulh: ( a3.b3.x^32 + (a3.b2 + a2.b3).x^16 + (a3.b1 + a2.b2 + a1.b3)      ).x^64
       #
       #clmulm: ( a0.b3 + a1.b2 + a2.b1 + a3.b0) .x^48
       #*/
       #
       #wire [15:0] lhs0 = clmulh_sel? rs1[63 -:16] : rs1[15 -:16];
       #wire [15:0] rhs0 = clmulh_sel? rs2[63 -:16] : rs2[15 -:16];
       #
       #wire [15:0] lhs1 = clmulh_sel? rs1[47 -:16] : rs1[31 -:16];
       #wire [15:0] rhs1 = clmulh_sel? rs2[47 -:16] : rs2[31 -:16];
       #
       #wire [15:0] lhs2 = clmulh_sel? rs1[31 -:16] : rs1[47 -:16];
       #wire [15:0] rhs2 = clmulh_sel? rs2[31 -:16] : rs2[47 -:16];
       #
       #// clmulh and clmull
       #
       #wire [31:0]  polymul0, polymul1,polymul2,polymul3,polymul4,polymul5;
       #poly16_mul mul16_ins0(lhs0, rhs0, polymul0);
       #poly16_mul mul16_ins1(lhs0, rhs1, polymul1);
       #poly16_mul mul16_ins2(lhs1, rhs0, polymul2);
       #poly16_mul mul16_ins3(lhs0, rhs2, polymul3);
       #poly16_mul mul16_ins4(lhs1, rhs1, polymul4);
       #poly16_mul mul16_ins5(lhs2, rhs0, polymul5);
       #
       #wire [31:0] clmul_p2   = polymul1 ^ polymul2;
       #wire [31:0] clmul_p3   = polymul3 ^ polymul4 ^ polymul5;
       #
       #wire [63:0] clmulh_int = {polymul0[31:16],(polymul0[15:0] ^ clmul_p2[31:16]),(clmul_p2[15:0] ^ clmul_p3[31:16]),clmul_p3[15:0]};
       #wire [63:0] clmull_int = {clmul_p3[31:16],(clmul_p3[15:0] ^ clmul_p2[31:16]),(clmul_p2[15:0] ^ polymul0[31:16]),polymul0[15:0]};
       #
       #// clmulm: ( a0.b3 + a1.b2 + a2.b1 + a3.b0) .x^48
       #wire [15:0] lhs6 = rs1[15 -:16];
       #wire [15:0] lhs7 = rs1[31 -:16];
       #wire [15:0] lhs8 = rs1[47 -:16];
       #wire [15:0] lhs9 = rs1[63 -:16];
       #
       #wire [15:0] rhs6 = rs2[63 -:16];
       #wire [15:0] rhs7 = rs2[47 -:16];
       #wire [15:0] rhs8 = rs2[31 -:16];
       #wire [15:0] rhs9 = rs2[15 -:16];
       #
       #wire [31:0] polymul6, polymul7, polymul8, polymul9;
       #poly16_mul  mul16_ins6(lhs6, rhs6, polymul6);
       #poly16_mul  mul16_ins7(lhs7, rhs7, polymul7);
       #poly16_mul  mul16_ins8(lhs8, rhs8, polymul8);
       #poly16_mul  mul16_ins9(lhs9, rhs9, polymul9);
       #
       #wire [31:0] clmulm =  polymul6 ^ polymul7 ^ polymul8 ^ polymul9;
       #
       #wire [63:0] wclmulh = { clmulh_int[63:16], (clmulh_int[15: 0] ^ clmulm[31:16])                    };
       #wire [63:0] wclmull = {                    (clmull_int[63:48] ^ clmulm[15: 0]), clmull_int[47: 0] };
       #
       #assign rd  =       {64{clmull_sel}} & wclmull|
       #                   {64{clmulh_sel}} & wclmulh;
       #
       #endmodule
       """.stripMargin('#'))
}
class zbkx32 extends BlackBox with HasBlackBoxInline {
  val io = IO(new ZBKInterface(32))
  setInline("zbkx32.v",
    s"""
       #module zbkx32  (
       #input  [ 3:0] zbk_fn,
       #input         dw,
       #input         valid,
       #input  [31:0] rs1,
       #input  [31:0] rs2,
       #output [31:0] rd);
       #
       #`define lut4(x, i, l) res``l; \\
       #    wire [3:0]    res``l = (i == 0) ? x[ 3: 0] : \\
       #                           (i == 1) ? x[ 7: 4] : \\
       #                           (i == 2) ? x[11: 8] : \\
       #                           (i == 3) ? x[15:12] : \\
       #                           (i == 4) ? x[19:16] : \\
       #                           (i == 5) ? x[23:20] : \\
       #                           (i == 6) ? x[27:24] : \\
       #                           (i == 7) ? x[31:28] : \\
       #                         /*(i == 0)*/ 0
       #
       #`define lut8(x, i, l) res``l; \\
       #    wire [7:0]    res``l = (i == 0) ? x[ 7: 0] : \\
       #                           (i == 1) ? x[15: 8] : \\
       #                           (i == 2) ? x[23:16] : \\
       #                           (i == 3) ? x[31:24] : \\
       #                         /*(i == 0)*/ 0
       #
       #localparam [3:0] FN_XPERM8= 4'd14;
       #localparam [3:0] FN_XPERM4= 4'd15;
       #
       #wire xperm8_sel = valid && (zbk_fn == FN_XPERM8);
       #wire xperm4_sel = valid && (zbk_fn == FN_XPERM4);
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
       #assign rd  =       {32{xperm8_sel}} & wxperm8|
       #                   {32{xperm4_sel}} & wxperm4;
       #endmodule
     """.stripMargin('#'))
}

class zbkx64 extends BlackBox with HasBlackBoxInline {
  val io = IO(new ZBKInterface(64))
  setInline("zbkx64.v",
    s"""
       #module zbkx64  (
       #input  [ 3:0] zbk_fn,
       #input         dw,
       #input         valid,
       #input  [63:0] rs1,
       #input  [63:0] rs2,
       #output [63:0] rd);
       #`define lut4(x, i, l) res``l; \\
       #    wire [3:0]    res``l = (i == 0) ? x[ 3: 0] : \\
       #                           (i == 1) ? x[ 7: 4] : \\
       #                           (i == 2) ? x[11: 8] : \\
       #                           (i == 3) ? x[15:12] : \\
       #                           (i == 4) ? x[19:16] : \\
       #                           (i == 5) ? x[23:20] : \\
       #                           (i == 6) ? x[27:24] : \\
       #                           (i == 7) ? x[31:28] : \\
       #                           (i == 8) ? x[35:32] : \\
       #                           (i == 9) ? x[39:36] : \\
       #                           (i ==10) ? x[43:40] : \\
       #                           (i ==11) ? x[47:44] : \\
       #                           (i ==12) ? x[51:48] : \\
       #                           (i ==13) ? x[55:52] : \\
       #                           (i ==14) ? x[59:56] : \\
       #                           (i ==15) ? x[63:60] : \\
       #                        /*(i == 0)*/ 0
       #
       #`define lut8(x, i, l) res``l; \\
       #    wire [7:0]    res``l = (i == 0) ? x[ 7: 0] : \\
       #                           (i == 1) ? x[15: 8] : \\
       #                           (i == 2) ? x[23:16] : \\
       #                           (i == 3) ? x[31:24] : \\
       #                           (i == 4) ? x[39:32] : \\
       #                           (i == 5) ? x[47:40] : \\
       #                           (i == 6) ? x[55:48] : \\
       #                           (i == 7) ? x[63:56] : \\
       #                         /*(i == 0)*/ 0
       #
       #localparam [3:0] FN_XPERM8= 4'd14;
       #localparam [3:0] FN_XPERM4= 4'd15;
       #
       #wire xperm8_sel = valid && (zbk_fn == FN_XPERM8);
       #wire xperm4_sel = valid && (zbk_fn == FN_XPERM4);
       #
       #wire [ 7:0] wlut8_0 = `lut8(rs1, rs2[ 7: 0], ilut8_0);
       #wire [ 7:0] wlut8_1 = `lut8(rs1, rs2[15: 8], ilut8_1);
       #wire [ 7:0] wlut8_2 = `lut8(rs1, rs2[23:16], ilut8_2);
       #wire [ 7:0] wlut8_3 = `lut8(rs1, rs2[31:24], ilut8_3);
       #wire [ 7:0] wlut8_4 = `lut8(rs1, rs2[39:32], ilut8_4);
       #wire [ 7:0] wlut8_5 = `lut8(rs1, rs2[47:40], ilut8_5);
       #wire [ 7:0] wlut8_6 = `lut8(rs1, rs2[55:48], ilut8_6);
       #wire [ 7:0] wlut8_7 = `lut8(rs1, rs2[63:56], ilut8_7);
       #wire [63:0] wxperm8 = { wlut8_7, wlut8_6, wlut8_5, wlut8_4,
       #                        wlut8_3, wlut8_2, wlut8_1, wlut8_0 };
       #
       #wire [ 3:0] wlut4_0 = `lut4(rs1, rs2[ 3: 0], ilut4_0);
       #wire [ 3:0] wlut4_1 = `lut4(rs1, rs2[ 7: 4], ilut4_1);
       #wire [ 3:0] wlut4_2 = `lut4(rs1, rs2[11: 8], ilut4_2);
       #wire [ 3:0] wlut4_3 = `lut4(rs1, rs2[15:12], ilut4_3);
       #wire [ 3:0] wlut4_4 = `lut4(rs1, rs2[19:16], ilut4_4);
       #wire [ 3:0] wlut4_5 = `lut4(rs1, rs2[23:20], ilut4_5);
       #wire [ 3:0] wlut4_6 = `lut4(rs1, rs2[27:24], ilut4_6);
       #wire [ 3:0] wlut4_7 = `lut4(rs1, rs2[31:28], ilut4_7);
       #wire [ 3:0] wlut4_8 = `lut4(rs1, rs2[35:32], ilut4_8);
       #wire [ 3:0] wlut4_9 = `lut4(rs1, rs2[39:36], ilut4_9);
       #wire [ 3:0] wlut4_10= `lut4(rs1, rs2[43:40], ilut4_10);
       #wire [ 3:0] wlut4_11= `lut4(rs1, rs2[47:44], ilut4_11);
       #wire [ 3:0] wlut4_12= `lut4(rs1, rs2[51:48], ilut4_12);
       #wire [ 3:0] wlut4_13= `lut4(rs1, rs2[55:52], ilut4_13);
       #wire [ 3:0] wlut4_14= `lut4(rs1, rs2[59:56], ilut4_14);
       #wire [ 3:0] wlut4_15= `lut4(rs1, rs2[63:60], ilut4_15);
       #wire [63:0] wxperm4 = { wlut4_15, wlut4_14, wlut4_13, wlut4_12,
       #                        wlut4_11, wlut4_10, wlut4_9,  wlut4_8,
       #                        wlut4_7,  wlut4_6,  wlut4_5,  wlut4_4,
       #                        wlut4_3,  wlut4_2,  wlut4_1,  wlut4_0 };
       #
       #assign rd  =       {64{xperm8_sel}} & wxperm8|
       #                   {64{xperm4_sel}} & wxperm4;
       #
       #endmodule
       """.stripMargin('#'))
}

class poly16_mul extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val a = Input(UInt(16.W))
    val b = Input(UInt(16.W))
    val r = Output(UInt(32.W))
  })
  setInline("poly16_mul.v",
    s"""
       #module poly16_mul (
       #    input  wire  [15:0] a,
       #    input  wire  [15:0] b,
       #    output wire  [31:0] r
       #);
       #wire z30 = a[15] & b[15];
       #wire t1  = a[15] & b[12];
       #wire t2  = a[15] & b[13];
       #wire t3  = a[15] & b[14];
       #wire t4  = a[12] & b[15];
       #wire t5  = a[13] & b[15];
       #wire t6  = a[14] & b[15];
       #wire t7  = a[14] & b[14];
       #wire t8  = a[14] & b[12];
       #wire t9  = a[14] & b[13];
       #wire t10 = a[12] & b[14];
       #wire t11 = a[13] & b[14];
       #wire t12 = a[13] & b[13];
       #wire t13 = a[13] & b[12];
       #wire t14 = a[12] & b[13];
       #wire t15 = a[12] & b[12];
       #wire t16 = a[11] & b[11];
       #wire t17 = a[11] & b[ 8];
       #wire t18 = a[11] & b[ 9];
       #wire t19 = a[11] & b[10];
       #wire t20 = a[ 8] & b[11];
       #wire t21 = a[ 9] & b[11];
       #wire t22 = a[10] & b[11];
       #wire t23 = a[10] & b[10];
       #wire t24 = a[10] & b[ 8];
       #wire t25 = a[10] & b[ 9];
       #wire t26 = a[ 8] & b[10];
       #wire t27 = a[ 9] & b[10];
       #wire t28 = a[ 9] & b[ 9];
       #wire t29 = a[ 9] & b[ 8];
       #wire t30 = a[ 8] & b[ 9];
       #wire t31 = a[ 8] & b[ 8];
       #wire t32 = a[ 7] & b[ 7];
       #wire t33 = a[ 7] & b[ 4];
       #wire t34 = a[ 7] & b[ 5];
       #wire t35 = a[ 7] & b[ 6];
       #wire t36 = a[ 4] & b[ 7];
       #wire t37 = a[ 5] & b[ 7];
       #wire t38 = a[ 6] & b[ 7];
       #wire t39 = a[ 6] & b[ 6];
       #wire t40 = a[ 6] & b[ 4];
       #wire t41 = a[ 6] & b[ 5];
       #wire t42 = a[ 4] & b[ 6];
       #wire t43 = a[ 5] & b[ 6];
       #wire t44 = a[ 5] & b[ 5];
       #wire t45 = a[ 5] & b[ 4];
       #wire t46 = a[ 4] & b[ 5];
       #wire t47 = a[ 4] & b[ 4];
       #wire t48 = a[ 3] & b[ 3];
       #wire t49 = a[ 3] & b[ 0];
       #wire t50 = a[ 3] & b[ 1];
       #wire t51 = a[ 3] & b[ 2];
       #wire t52 = a[ 0] & b[ 3];
       #wire t53 = a[ 1] & b[ 3];
       #wire t54 = a[ 2] & b[ 3];
       #wire t55 = a[ 2] & b[ 2];
       #wire t56 = a[ 2] & b[ 0];
       #wire t57 = a[ 2] & b[ 1];
       #wire t58 = a[ 0] & b[ 2];
       #wire t59 = a[ 1] & b[ 2];
       #wire t60 = a[ 1] & b[ 1];
       #wire t61 = a[ 1] & b[ 0];
       #wire t62 = a[ 0] & b[ 1];
       #wire  z0 = a[ 0] & b[ 0];
       #wire t63 = b[ 8] ^ b[12];
       #wire t64 = b[ 9] ^ b[13];
       #wire t65 = b[10] ^ b[14];
       #wire t66 = b[11] ^ b[15];
       #wire t67 = a[ 8] ^ a[12];
       #wire t68 = a[ 9] ^ a[13];
       #wire t69 = a[10] ^ a[14];
       #wire t70 = a[11] ^ a[15];
       #wire t71 = t70 & t66;
       #wire t72 = t70 & t63;
       #wire t73 = t70 & t64;
       #wire t74 = t70 & t65;
       #wire t75 = t67 & t66;
       #wire t76 = t68 & t66;
       #wire t77 = t69 & t66;
       #wire t78 = t69 & t65;
       #wire t79 = t69 & t63;
       #wire t80 = t69 & t64;
       #wire t81 = t67 & t65;
       #wire t82 = t68 & t65;
       #wire t83 = t68 & t64;
       #wire t84 = t68 & t63;
       #wire t85 = t67 & t64;
       #wire t86 = t67 & t63;
       #wire t87 = b[0] ^ b[ 4];
       #wire t88 = b[1] ^ b[ 5];
       #wire t89 = b[2] ^ b[ 6];
       #wire t90 = b[3] ^ b[ 7];
       #wire t91 = a[0] ^ a[ 4];
       #wire t92 = a[1] ^ a[ 5];
       #wire t93 = a[2] ^ a[ 6];
       #wire t94 = a[3] ^ a[ 7];
       #wire t95 = t94  & t90;
       #wire t96 = t94  & t87;
       #wire t97 = t94  & t88;
       #wire t98 = t94  & t89;
       #wire t99 = t91  & t90;
       #
       #wire t100 = t92 & t90;
       #wire t101 = t93 & t90;
       #wire t102 = t93 & t89;
       #wire t103 = t93 & t87;
       #wire t104 = t93 & t88;
       #wire t105 = t91 & t89;
       #wire t106 = t92 & t89;
       #wire t107 = t92 & t88;
       #wire t108 = t92 & t87;
       #wire t109 = t91 & t88;
       #wire t110 = t91 & t87;
       #wire t111 = b[4] ^ b[12];
       #wire t112 = b[5] ^ b[13];
       #wire t113 = b[6] ^ b[14];
       #wire t114 = b[7] ^ b[15];
       #wire t115 = b[0] ^ b[ 8];
       #wire t116 = b[1] ^ b[ 9];
       #wire t117 = b[2] ^ b[10];
       #wire t118 = b[3] ^ b[11];
       #wire t119 = a[4] ^ a[12];
       #wire t120 = a[5] ^ a[13];
       #wire t121 = a[6] ^ a[14];
       #wire t122 = a[7] ^ a[15];
       #wire t123 = a[0] ^ a[ 8];
       #wire t124 = a[1] ^ a[ 9];
       #wire t125 = a[2] ^ a[10];
       #wire t126 = a[3] ^ a[11];
       #wire t127 = t126 & t118;
       #wire t128 = t126 & t115;
       #wire t129 = t126 & t116;
       #wire t130 = t126 & t117;
       #wire t131 = t123 & t118;
       #wire t132 = t124 & t118;
       #wire t133 = t125 & t118;
       #wire t134 = t125 & t117;
       #wire t135 = t125 & t115;
       #wire t136 = t125 & t116;
       #wire t137 = t123 & t117;
       #wire t138 = t124 & t117;
       #wire t139 = t124 & t116;
       #wire t140 = t124 & t115;
       #wire t141 = t123 & t116;
       #wire t142 = t123 & t115;
       #wire t143 = t122 & t114;
       #wire t144 = t122 & t111;
       #wire t145 = t122 & t112;
       #wire t146 = t122 & t113;
       #wire t147 = t119 & t114;
       #wire t148 = t120 & t114;
       #wire t149 = t121 & t114;
       #wire t150 = t121 & t113;
       #wire t151 = t121 & t111;
       #wire t152 = t121 & t112;
       #wire t153 = t119 & t113;
       #wire t154 = t120 & t113;
       #wire t155 = t120 & t112;
       #wire t156 = t120 & t111;
       #wire t157 = t119 & t112;
       #wire t158 = t119 & t111;
       #wire t159 = t115 ^ t111;
       #wire t160 = t116 ^ t112;
       #wire t161 = t117 ^ t113;
       #wire t162 = t118 ^ t114;
       #wire t163 = t123 ^ t119;
       #wire t164 = t124 ^ t120;
       #wire t165 = t125 ^ t121;
       #wire t166 = t126 ^ t122;
       #wire t167 = t166 & t162;
       #wire t168 = t166 & t159;
       #wire t169 = t166 & t160;
       #wire t170 = t166 & t161;
       #wire t171 = t163 & t162;
       #wire t172 = t164 & t162;
       #wire t173 = t165 & t162;
       #wire t174 = t165 & t161;
       #wire t175 = t165 & t159;
       #wire t176 = t165 & t160;
       #wire t177 = t163 & t161;
       #wire t178 = t164 & t161;
       #wire t179 = t164 & t160;
       #wire t180 = t164 & t159;
       #wire t181 = t163 & t160;
       #wire t182 = t163 & t159;
       #wire t183 = t73  ^ t76;
       #wire t184 = t97  ^ t100;
       #wire t185 = t15  ^ t18;
       #wire t186 = t129 ^ t132;
       #wire t187 = t134 ^ t158;
       #wire t188 = t145 ^ t148;
       #wire t189 = t169 ^ t172;
       #wire t190 = t2   ^ t5;
       #wire t191 = t21  ^ t23;
       #wire t192 = t31  ^ t34;
       #wire t193 = t37  ^ t39;
       #wire t194 = t47  ^ t50;
       #wire t195 = t53  ^ t55;
       #wire t196 = t183 ^ t78;
       #wire t197 = t192 ^ t193;
       #wire t198 = t194 ^ t195;
       #wire t199 = t184 ^ t102;
       #wire t200 = t185 ^ t191;
       #wire t201 = t186 ^ t187;
       #wire t202 = t188 ^ t150;
       #wire t203 = t189 ^ t174;
       #wire  z28 = t190 ^ t7;
       #wire t204 = t198 ^   z0;
       #wire   z4 = t110 ^ t204;
       #wire t205 = t200 ^  z28;
       #wire  z24 = t196 ^ t205;
       #wire t206 = t197 ^ t199;
       #wire t207 = t197 ^ t86;
       #wire t208 = t202 ^ t205;
       #wire  z20 = t207 ^ t208;
       #wire t209 = t142 ^ t204;
       #wire   z8 = t206 ^ t209;
       #wire t210 = t196 ^ t198;
       #wire t211 = t201 ^ t206;
       #wire t212 = t208 ^ t210;
       #wire t213 = t211 ^ t212;
       #wire t214 = t200 ^ t201;
       #wire t215 = t110 ^ t182;
       #wire t216 = t209 ^ t214;
       #wire t217 = t215 ^ t207;
       #wire  z12 = t217 ^ t216;
       #wire  z16 = t213 ^ t203;
       #wire t218 = t74  ^ t77;
       #wire t219 = t84  ^ t85;
       #wire t220 = t13  ^ t14;
       #wire t221 = t98  ^ t101;
       #wire t222 = t108 ^ t109;
       #wire t223 = t130 ^ t133;
       #wire t224 = t140 ^ t141;
       #wire t225 = t146 ^ t149;
       #wire t226 = t156 ^ t157;
       #wire t227 = t170 ^ t173;
       #wire t228 = t19  ^ t22;
       #wire t229 = t180 ^ t181;
       #wire t230 = t29  ^ t30;
       #wire  z29 = t3   ^ t6;
       #wire t231 = t35  ^ t38;
       #wire t232 = t45  ^ t46;
       #wire t233 = t51  ^ t54;
       #wire   z1 = t61  ^ t62;
       #wire t234 = t228 ^ t220;
       #wire t235 = t230 ^ t231;
       #wire t236 = t232 ^ t233;
       #wire t237 = t223 ^ t226;
       #wire t238 =  z29 ^ t234;
       #wire  z25 = t218 ^ t238;
       #wire t239 =   z1 ^ t236;
       #wire   z5 = t222 ^ t239;
       #wire t240 = t219 ^ t235;
       #wire t241 = t235 ^ t221;
       #wire t242 = t224 ^ t239;
       #wire   z9 = t241 ^ t242;
       #wire t243 = t225 ^ t238;
       #wire  z21 = t240 ^ t243;
       #wire t244 = t218 ^ t236;
       #wire t245 = t237 ^ t241;
       #wire t246 = t243 ^ t244;
       #wire t247 = t245 ^ t227;
       #wire t248 = t234 ^ t237;
       #wire t249 = t222 ^ t240;
       #wire t250 = t242 ^ t248;
       #wire t251 = t249 ^ t229;
       #wire  z17 = t247 ^ t246;
       #wire  z13 = t251 ^ t250;
       #wire t252 = t10  ^ t12;
       #wire t253 = t79  ^ t81;
       #wire t254 = t103 ^ t105;
       #wire t255 = t127 ^ t151;
       #wire t256 = t135 ^ t137;
       #wire t257 = t153 ^ t155;
       #wire t258 = t16  ^ t8;
       #wire t259 = t175 ^ t177;
       #wire t260 = t24  ^ t26;
       #wire t261 = t28  ^ t32;
       #wire t262 = t40  ^ t42;
       #wire t263 = t44  ^ t48;
       #wire t264 = t56  ^ t58;
       #wire t265 = t252 ^ t258;
       #wire t266 = t261 ^ t260;
       #wire t267 = t262 ^ t263;
       #wire   z2 = t264 ^ t60;
       #wire t268 = t253 ^ t83;
       #wire t269 = t254 ^ t107;
       #wire t270 = t255 ^ t257;
       #wire t271 = t256 ^ t139;
       #wire t272 = t259 ^ t179;
       #wire t273 = t265 ^  z30;
       #wire  z26 = t71  ^ t273;
       #wire t274 = t267 ^   z2;
       #wire   z6 = t269 ^ t274;
       #wire t275 = t266 ^ t268;
       #wire t276 = t266 ^ t95 ;
       #wire t277 = t271 ^ t274;
       #wire  z10 = t276 ^ t277;
       #wire t278 = t143 ^ t273;
       #wire  z22 = t275 ^ t278;
       #wire t279 = t265 ^ t269;
       #wire t280 = t270 ^ t275;
       #wire t281 = t277 ^ t279;
       #wire t282 = t280 ^ t281;
       #wire t283 = t267 ^ t270;
       #wire t284 = t71  ^ t167;
       #wire t285 = t278 ^ t283;
       #wire t286 = t284 ^ t276;
       #wire  z14 = t282 ^ t272;
       #wire  z18 = t286 ^ t285;
       #wire t287 = t9   ^ t11;
       #wire t288 = t72  ^ t75;
       #wire t289 = t80  ^ t82;
       #wire t290 = t96  ^ t99;
       #wire t291 = t104 ^ t106;
       #wire t292 = t1   ^ t4;
       #wire t293 = t128 ^ t131;
       #wire t294 = t136 ^ t138;
       #wire t295 = t144 ^ t147;
       #wire t296 = t152 ^ t154;
       #wire t297 = t17  ^ t20;
       #wire t298 = t168 ^ t171;
       #wire t299 = t176 ^ t178;
       #wire t300 = t25  ^ t27;
       #wire t301 = t33  ^ t36;
       #wire t302 = t41  ^ t43;
       #wire t303 = t49  ^ t52;
       #wire t304 = t57  ^ t59;
       #wire  z27 = t287 ^ t292;
       #wire t305 = t296 ^ t295;
       #wire t306 = t297 ^ t300;
       #wire t307 = t298 ^ t299;
       #wire t308 = t301 ^ t302;
       #wire   z3 = t303 ^ t304;
       #wire t309 = t288 ^ t289;
       #wire t310 = t290 ^ t291;
       #wire t311 = t293 ^ t294;
       #wire t312 =  z27 ^ t306;
       #wire  z23 = t309 ^ t312;
       #wire t313 = t308 ^   z3;
       #wire   z7 = t310 ^ t313;
       #wire t314 = t305 ^ t308;
       #wire  z19 = t312 ^ t314;
       #wire t315 = t306 ^ t311;
       #wire  z11 = t313 ^ t315;
       #wire t316 = t305 ^ t311;
       #wire t317 =  z23 ^   z7;
       #wire t318 = t316 ^ t307;
       #wire  z15 = t318 ^ t317;
       #
       #assign r = {1'b0,z30,z29,z28,z27,z26,z25,z24,z23,z22,z21,z20,z19,z18,z17,z16,z15,z14,z13,z12,z11,z10,z9,z8,z7,z6,z5,z4,z3,z2,z1,z0};
       #endmodule
     """.stripMargin('#'))
}
object poly16_mul {
  def apply(in0: UInt, in1: UInt) = {
    val m = Module(new poly16_mul)
    m.io.a := in0
    m.io.b := in1
    m.io.r
  }
}
