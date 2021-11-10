// See LICENSE.SiFive for license details.

package freechips.rocketchip.zk

import chisel3._
import chisel3.util.{BitPat, HasBlackBoxInline}

object ZKS {
  val SM4ED       = BitPat("b??11000??????????000?????0110011")
  val SM4KS       = BitPat("b??11010??????????000?????0110011")
  val SM3P0       = BitPat("b000100001000?????001?????0010011")
  val SM3P1       = BitPat("b000100001001?????001?????0010011")

  val FN_Len      = 4
  def FN_SM4ED    =  0.U(FN_Len.W)
  def FN_SM4KS    =  1.U(FN_Len.W)
  def FN_SM3P0    =  2.U(FN_Len.W)
  def FN_SM3P1    =  3.U(FN_Len.W)
}

class ZKSInterface(xLen: Int) extends Bundle {
  val zks_fn = Input(UInt(ZKN.FN_Len.W))
  val valid  = Input(Bool())
  val bs     = Input(UInt(2.W))
  val rs1    = Input(UInt(xLen.W))
  val rs2    = Input(UInt(xLen.W))
  val rd     = Output(UInt(xLen.W))
}

class ZKSImp(xLen:Int) extends Module {
  val io = IO(new ZKSInterface(xLen))

  val zks_res = if (xLen == 64) {
    val zks_u = Module(new zks64)
    zks_u.io.zks_fn := io.zks_fn
    zks_u.io.valid  := io.valid
    zks_u.io.bs     := io.bs
    zks_u.io.rs1    := io.rs1
    zks_u.io.rs2    := io.rs2
    zks_u.io.rd
  } else if (xLen == 32) {
    val zks_u = Module(new zks32)
    zks_u.io.zks_fn := io.zks_fn
    zks_u.io.valid  := io.valid
    zks_u.io.bs     := io.bs
    zks_u.io.rs1    := io.rs1
    zks_u.io.rs2    := io.rs2
    zks_u.io.rd
  } else 0.U

  io.rd := zks_res 
}

class zks32 extends BlackBox with HasBlackBoxInline {
  val io = IO(new ZKSInterface(32))
  setInline("zks32.v",
    s"""
       #module zks32  (
       #input  [ 3:0] zks_fn,
       #input         valid,
       #input  [ 1:0] bs,
       #input  [31:0] rs1,
       #input  [31:0] rs2,
       #output [31:0] rd);
       #
       #`define ROLI32(a,b) ((a << b) | (a >> 32-b))
       #
       #localparam [3:0] FN_SM4ED  = 4'd00;
       #localparam [3:0] FN_SM4KS  = 4'd01;
       #localparam [3:0] FN_SM3P0  = 4'd02;
       #localparam [3:0] FN_SM3P1  = 4'd03;
       #
       #wire      sm4ed_sel = valid && (zks_fn == FN_SM4ED);
       #wire      sm4ks_sel = valid && (zks_fn == FN_SM4KS);
       #wire      sm3p0_sel = valid && (zks_fn == FN_SM3P0);
       #wire      sm3p1_sel = valid && (zks_fn == FN_SM3P1);
       #
       #wire [7:0] in_bytes [3:0];
       #
       #assign in_bytes[0]  = rs2[ 7: 0];
       #assign in_bytes[1]  = rs2[15: 8];
       #assign in_bytes[2]  = rs2[23:16];
       #assign in_bytes[3]  = rs2[31:24];
       #
       #wire [ 7:0] sbox_in = in_bytes[bs];
       #wire [ 7:0] sbox_out;
       #
       #wire [31:0] s       = {24'b0, sbox_out};
       #
       #// ED Instruction
       #wire [31:0] wed1    = s    ^  (s           <<  8) ^ (s << 2) ^ (s << 18);
       #wire [31:0] wed2    = wed1 ^ ((s & 32'h3F) << 26) ^
       #                            ((s & 32'hC0) << 10) ;
       #// KS Instruction
       #wire [31:0] wks1    = s    ^ ((s & 32'h07) << 29) ^ ((s & 32'hFE) << 7);
       #wire [31:0] wks2    = wks1 ^ ((s & 32'h01) << 23) ^
       #                            ((s & 32'hF8) << 13) ;
       #
       #//
       #// Rotate and XOR result
       #wire [31:0] rot_in  = sm4ks_sel ? wks2 : wed2;
       #wire [31:0] rot_out = {32{bs == 2'b00}} & {rot_in                      } |
       #                      {32{bs == 2'b01}} & {rot_in[23:0], rot_in[31:24] } |
       #                      {32{bs == 2'b10}} & {rot_in[15:0], rot_in[31:16] } |
       #                      {32{bs == 2'b11}} & {rot_in[ 7:0], rot_in[31: 8] } ;
       #wire [31:0] wsm4    = rot_out ^ rs1 ;
       #
       #wire [31:0] wsm3_p0 = rs1 ^ `ROLI32(rs1,  9) ^ `ROLI32(rs1,17);
       #wire [31:0] wsm3_p1 = rs1 ^ `ROLI32(rs1, 15) ^ `ROLI32(rs1,23);
       #
       #assign           rd = {32{sm4ed_sel}} & wsm4    |
       #                      {32{sm4ks_sel}} & wsm4    |
       #                      {32{sm3p0_sel}} & wsm3_p0 |
       #                      {32{sm3p1_sel}} & wsm3_p1 ;
       #// Submodule - SBox
       #sm4_sbox ism4_sbox ( .in(sbox_in ), .fx(sbox_out));
       #
       #`undef ROLI32
       #
       #endmodule
       #
       #// SM4 SBox
       #module sm4_sbox (
       #input  [7:0] in,
       #output [7:0] fx);
       #
       #wire [20:0] t1;
       #wire [17:0] t2;
       #
       #sm4_sbox_top     top ( .y(t1), .x(in) );
       #    sbox_inv_mid mid ( .y(t2), .x(t1) );
       #sm4_sbox_out     out ( .y(fx), .x(t2) );
       #
       #endmodule
       #
       #module sm4_sbox_top(
       #input   [ 7:0] x,
       #output  [20:0] y);
       #
       #wire y18 = x[ 2] ^  x[ 6];
       #wire t0  = x[ 3] ^  x[ 4];
       #wire t1  = x[ 2] ^  x[ 7];
       #wire t2  = x[ 7] ^  y18  ;
       #wire t3  = x[ 1] ^  t1   ;
       #wire t4  = x[ 6] ^  x[ 7];
       #wire t5  = x[ 0] ^  y18  ;
       #wire t6  = x[ 3] ^  x[ 6];
       #wire y10 = x[ 1] ^  y18;
       #wire y0  = x[ 5] ^~ y10;
       #wire y1  = t0    ^  t3 ;
       #wire y2  = x[ 0] ^  t0 ;
       #wire y4  = x[ 0] ^  t3 ;
       #wire y3  = x[ 3] ^  y4 ;
       #wire y5  = x[ 5] ^  t5 ;
       #wire y6  = x[ 0] ^~ x[ 1];
       #wire y7  = t0    ^~ y10;
       #wire y8  = t0    ^  t5 ;
       #wire y9  = x[ 3];
       #wire y11 = t0    ^  t4 ;
       #wire y12 = x[ 5] ^  t4 ;
       #wire y13 = x[ 5] ^~ y1 ;
       #wire y14 = x[ 4] ^~ t2 ;
       #wire y15 = x[ 1] ^~ t6 ;
       #wire y16 = x[ 0] ^~ t2 ;
       #wire y17 = t0    ^~ t2 ;
       #wire y19 = x[ 5] ^~ y14;
       #wire y20 = x[ 0] ^  t1 ;
       #
       #assign y[0 ] = y0 ;
       #assign y[1 ] = y1 ;
       #assign y[10] = y10;
       #assign y[11] = y11;
       #assign y[12] = y12;
       #assign y[13] = y13;
       #assign y[14] = y14;
       #assign y[15] = y15;
       #assign y[16] = y16;
       #assign y[17] = y17;
       #assign y[18] = y18;
       #assign y[19] = y19;
       #assign y[2 ] = y2 ;
       #assign y[20] = y20;
       #assign y[3 ] = y3 ;
       #assign y[4 ] = y4 ;
       #assign y[5 ] = y5 ;
       #assign y[6 ] = y6 ;
       #assign y[7 ] = y7 ;
       #assign y[8 ] = y8 ;
       #assign y[9 ] = y9 ;
       #endmodule
       #
       #module sm4_sbox_out(
       #input   [17:0] x,
       #output  [ 7:0] y);
       #
       #wire   t0   = x[ 4] ^  x[ 7];
       #wire   t1   = x[13] ^  x[15];
       #wire   t2   = x[ 2] ^  x[16];
       #wire   t3   = x[ 6] ^  t0;
       #wire   t4   = x[12] ^  t1;
       #wire   t5   = x[ 9] ^  x[10];
       #wire   t6   = x[11] ^  t2;
       #wire   t7   = x[ 1] ^  t4;
       #wire   t8   = x[ 0] ^  x[17];
       #wire   t9   = x[ 3] ^  x[17];
       #wire   t10  = x[ 8] ^  t3;
       #wire   t11  = t2    ^  t5;
       #wire   t12  = x[14] ^  t6;
       #wire   t13  = t7    ^  t9;
       #wire   t14  = x[ 0] ^  x[ 6];
       #wire   t15  = x[ 7] ^  x[16];
       #wire   t16  = x[ 5] ^  x[13];
       #wire   t17  = x[ 3] ^  x[15];
       #wire   t18  = x[10] ^  x[12];
       #wire   t19  = x[ 9] ^  t1 ;
       #wire   t20  = x[ 4] ^  t4 ;
       #wire   t21  = x[14] ^  t3 ;
       #wire   t22  = x[16] ^  t5 ;
       #wire   t23  = t7    ^  t14;
       #wire   t24  = t8    ^  t11;
       #wire   t25  = t0    ^  t12;
       #wire   t26  = t17   ^  t3 ;
       #wire   t27  = t18   ^  t10;
       #wire   t28  = t19   ^  t6 ;
       #wire   t29  = t8    ^  t10;
       #assign y[0] = t11   ^~ t13;
       #assign y[1] = t15   ^~ t23;
       #assign y[2] = t20   ^  t24;
       #assign y[3] = t16   ^  t25;
       #assign y[4] = t26   ^~ t22;
       #assign y[5] = t21   ^  t13;
       #assign y[6] = t27   ^~ t12;
       #assign y[7] = t28   ^~ t29;
       #
       #endmodule
       #
       #
       #module sbox_inv_mid(
       #input   [20:0] x,
       #output  [17:0] y);
       #
       #wire   t0    = x[ 3] ^     x[12];
       #wire   t1    = x[ 9] &     x[ 5];
       #wire   t2    = x[17] &     x[ 6];
       #wire   t3    = x[10] ^     t1   ;
       #wire   t4    = x[14] &     x[ 0];
       #wire   t5    = t4    ^     t1   ;
       #wire   t6    = x[ 3] &     x[12];
       #wire   t7    = x[16] &     x[ 7];
       #wire   t8    = t0    ^     t6   ;
       #wire   t9    = x[15] &     x[13];
       #wire   t10   = t9    ^     t6   ;
       #wire   t11   = x[ 1] &     x[11];
       #wire   t12   = x[ 4] &     x[20];
       #wire   t13   = t12   ^     t11  ;
       #wire   t14   = x[ 2] &     x[ 8];
       #wire   t15   = t14   ^     t11  ;
       #wire   t16   = t3    ^     t2   ;
       #wire   t17   = t5    ^     x[18];
       #wire   t18   = t8    ^     t7   ;
       #wire   t19   = t10   ^     t15  ;
       #wire   t20   = t16   ^     t13  ;
       #wire   t21   = t17   ^     t15  ;
       #wire   t22   = t18   ^     t13  ;
       #wire   t23   = t19   ^     x[19];
       #wire   t24   = t22   ^     t23  ;
       #wire   t25   = t22   &     t20  ;
       #wire   t26   = t21   ^     t25  ;
       #wire   t27   = t20   ^     t21  ;
       #wire   t28   = t23   ^     t25  ;
       #wire   t29   = t28   &     t27  ;
       #wire   t30   = t26   &     t24  ;
       #wire   t31   = t20   &     t23  ;
       #wire   t32   = t27   &     t31  ;
       #wire   t33   = t27   ^     t25  ;
       #wire   t34   = t21   &     t22  ;
       #wire   t35   = t24   &     t34  ;
       #wire   t36   = t24   ^     t25  ;
       #wire   t37   = t21   ^     t29  ;
       #wire   t38   = t32   ^     t33  ;
       #wire   t39   = t23   ^     t30  ;
       #wire   t40   = t35   ^     t36  ;
       #wire   t41   = t38   ^     t40  ;
       #wire   t42   = t37   ^     t39  ;
       #wire   t43   = t37   ^     t38  ;
       #wire   t44   = t39   ^     t40  ;
       #wire   t45   = t42   ^     t41  ;
       #assign y[ 0] = t38   &     x[ 7];
       #assign y[ 1] = t37   &     x[13];
       #assign y[ 2] = t42   &     x[11];
       #assign y[ 3] = t45   &     x[20];
       #assign y[ 4] = t41   &     x[ 8];
       #assign y[ 5] = t44   &     x[ 9];
       #assign y[ 6] = t40   &     x[17];
       #assign y[ 7] = t39   &     x[14];
       #assign y[ 8] = t43   &     x[ 3];
       #assign y[ 9] = t38   &     x[16];
       #assign y[10] = t37   &     x[15];
       #assign y[11] = t42   &     x[ 1];
       #assign y[12] = t45   &     x[ 4];
       #assign y[13] = t41   &     x[ 2];
       #assign y[14] = t44   &     x[ 5];
       #assign y[15] = t40   &     x[ 6];
       #assign y[16] = t39   &     x[ 0];
       #assign y[17] = t43   &     x[12];
       #
       #endmodule
     """.stripMargin('#'))
}

class zks64 extends BlackBox with HasBlackBoxInline {
  val io = IO(new ZKSInterface(64))
  setInline("zks64.v",
    s"""
       #module zks64  (
       #input  [ 3:0] zks_fn,
       #input         valid,
       #input  [ 1:0] bs,
       #input  [63:0] rs1,
       #input  [63:0] rs2,
       #output [63:0] rd);
       #`define ROLI32(a,b) ((a << b) | (a >> 32-b))
       #
       #localparam [3:0] FN_SM4ED  = 4'd00;
       #localparam [3:0] FN_SM4KS  = 4'd01;
       #localparam [3:0] FN_SM3P0  = 4'd02;
       #localparam [3:0] FN_SM3P1  = 4'd03;
       #
       #wire      sm4ed_sel = valid && (zks_fn == FN_SM4ED);
       #wire      sm4ks_sel = valid && (zks_fn == FN_SM4KS);
       #wire      sm3p0_sel = valid && (zks_fn == FN_SM3P0);
       #wire      sm3p1_sel = valid && (zks_fn == FN_SM3P1);
       #
       #wire [7:0] in_bytes [3:0];
       #
       #assign in_bytes[0]  = rs2[ 7: 0];
       #assign in_bytes[1]  = rs2[15: 8];
       #assign in_bytes[2]  = rs2[23:16];
       #assign in_bytes[3]  = rs2[31:24];
       #
       #wire [ 7:0] sbox_in = in_bytes[bs];
       #wire [ 7:0] sbox_out;
       #
       #wire [31:0] s       = {24'b0, sbox_out};
       #
       #// ED Instruction
       #wire [31:0] wed1    = s    ^  (s           <<  8) ^ (s << 2) ^ (s << 18);
       #wire [31:0] wed2    = wed1 ^ ((s & 32'h3F) << 26) ^ ((s & 32'hC0) << 10);
       #// KS Instruction
       #wire [31:0] wks1    = s    ^ ((s & 32'h07) << 29) ^ ((s & 32'hFE) << 7);
       #wire [31:0] wks2    = wks1 ^ ((s & 32'h01) << 23) ^ ((s & 32'hF8) << 13);
       #
       #// Rotate and XOR result
       #wire [31:0] rot_in  = sm4ks_sel ? wks2 : wed2;
       #wire [31:0] rot_out = {32{bs == 2'b00}} & {rot_in                      } |
       #                      {32{bs == 2'b01}} & {rot_in[23:0], rot_in[31:24] } |
       #                      {32{bs == 2'b10}} & {rot_in[15:0], rot_in[31:16] } |
       #                      {32{bs == 2'b11}} & {rot_in[ 7:0], rot_in[31: 8] } ;
       #wire [31:0] wsm4    = rot_out ^ rs1[31:0] ;
       #
       #wire [31:0] wsm3_in = rs1[31:0];
       #wire [31:0] wsm3_p0 = wsm3_in ^ `ROLI32(wsm3_in,  9) ^ `ROLI32(wsm3_in,17);
       #wire [31:0] wsm3_p1 = wsm3_in ^ `ROLI32(wsm3_in, 15) ^ `ROLI32(wsm3_in,23);
       #
       #wire [31:0] res32   = {32{sm4ed_sel}} & wsm4    |
       #                      {32{sm4ks_sel}} & wsm4    |
       #                      {32{sm3p0_sel}} & wsm3_p0 |
       #                      {32{sm3p1_sel}} & wsm3_p1 ;
       #
       #// Sign extended 32-bit result
       #assign      rd      ={{32{res32[31]}}, res32};
       #
       #// Submodule - SBox
       #sm4_sbox ism4_sbox ( .in (sbox_in ), .fx(sbox_out) );
       #
       #`undef ROLI32
       #
       #endmodule
       #
       #// SM4 SBox
       #module sm4_sbox (
       #input  [7:0] in,
       #output [7:0] fx);
       #
       #wire [20:0] t1;
       #wire [17:0] t2;
       #
       #sm4_sbox_top     top ( .y(t1), .x(in) );
       #    sbox_inv_mid mid ( .y(t2), .x(t1) );
       #sm4_sbox_out     out ( .y(fx), .x(t2) );
       #
       #endmodule
       #
       #module sm4_sbox_top(
       #input   [ 7:0] x,
       #output  [20:0] y );
       #wire y18 = x[ 2] ^  x[ 6];
       #wire t0  = x[ 3] ^  x[ 4];
       #wire t1  = x[ 2] ^  x[ 7];
       #wire t2  = x[ 7] ^  y18  ;
       #wire t3  = x[ 1] ^  t1   ;
       #wire t4  = x[ 6] ^  x[ 7];
       #wire t5  = x[ 0] ^  y18  ;
       #wire t6  = x[ 3] ^  x[ 6];
       #wire y10 = x[ 1] ^  y18;
       #wire y0  = x[ 5] ^~ y10;
       #wire y1  = t0    ^  t3 ;
       #wire y2  = x[ 0] ^  t0 ;
       #wire y4  = x[ 0] ^  t3 ;
       #wire y3  = x[ 3] ^  y4 ;
       #wire y5  = x[ 5] ^  t5 ;
       #wire y6  = x[ 0] ^~ x[ 1];
       #wire y7  = t0    ^~ y10;
       #wire y8  = t0    ^  t5 ;
       #wire y9  = x[ 3];
       #wire y11 = t0    ^  t4 ;
       #wire y12 = x[ 5] ^  t4 ;
       #wire y13 = x[ 5] ^~ y1 ;
       #wire y14 = x[ 4] ^~ t2 ;
       #wire y15 = x[ 1] ^~ t6 ;
       #wire y16 = x[ 0] ^~ t2 ;
       #wire y17 = t0    ^~ t2 ;
       #wire y19 = x[ 5] ^~ y14;
       #wire y20 = x[ 0] ^  t1 ;
       #
       #assign y[0 ] = y0 ;
       #assign y[1 ] = y1 ;
       #assign y[10] = y10;
       #assign y[11] = y11;
       #assign y[12] = y12;
       #assign y[13] = y13;
       #assign y[14] = y14;
       #assign y[15] = y15;
       #assign y[16] = y16;
       #assign y[17] = y17;
       #assign y[18] = y18;
       #assign y[19] = y19;
       #assign y[2 ] = y2 ;
       #assign y[20] = y20;
       #assign y[3 ] = y3 ;
       #assign y[4 ] = y4 ;
       #assign y[5 ] = y5 ;
       #assign y[6 ] = y6 ;
       #assign y[7 ] = y7 ;
       #assign y[8 ] = y8 ;
       #assign y[9 ] = y9 ;
       #endmodule
       #
       #module sm4_sbox_out(
       #input   [17:0] x,
       #output  [ 7:0] y);
       #
       #wire   t0   = x[ 4] ^  x[ 7];
       #wire   t1   = x[13] ^  x[15];
       #wire   t2   = x[ 2] ^  x[16];
       #wire   t3   = x[ 6] ^  t0;
       #wire   t4   = x[12] ^  t1;
       #wire   t5   = x[ 9] ^  x[10];
       #wire   t6   = x[11] ^  t2;
       #wire   t7   = x[ 1] ^  t4;
       #wire   t8   = x[ 0] ^  x[17];
       #wire   t9   = x[ 3] ^  x[17];
       #wire   t10  = x[ 8] ^  t3;
       #wire   t11  = t2    ^  t5;
       #wire   t12  = x[14] ^  t6;
       #wire   t13  = t7    ^  t9;
       #wire   t14  = x[ 0] ^  x[ 6];
       #wire   t15  = x[ 7] ^  x[16];
       #wire   t16  = x[ 5] ^  x[13];
       #wire   t17  = x[ 3] ^  x[15];
       #wire   t18  = x[10] ^  x[12];
       #wire   t19  = x[ 9] ^  t1 ;
       #wire   t20  = x[ 4] ^  t4 ;
       #wire   t21  = x[14] ^  t3 ;
       #wire   t22  = x[16] ^  t5 ;
       #wire   t23  = t7    ^  t14;
       #wire   t24  = t8    ^  t11;
       #wire   t25  = t0    ^  t12;
       #wire   t26  = t17   ^  t3 ;
       #wire   t27  = t18   ^  t10;
       #wire   t28  = t19   ^  t6 ;
       #wire   t29  = t8    ^  t10;
       #assign y[0] = t11   ^~ t13;
       #assign y[1] = t15   ^~ t23;
       #assign y[2] = t20   ^  t24;
       #assign y[3] = t16   ^  t25;
       #assign y[4] = t26   ^~ t22;
       #assign y[5] = t21   ^  t13;
       #assign y[6] = t27   ^~ t12;
       #assign y[7] = t28   ^~ t29;
       #
       #endmodule
       #
       #module sbox_inv_mid(
       #input   [20:0] x,
       #output  [17:0] y
       #);
       #
       #wire   t0    = x[ 3] ^     x[12];
       #wire   t1    = x[ 9] &     x[ 5];
       #wire   t2    = x[17] &     x[ 6];
       #wire   t3    = x[10] ^     t1   ;
       #wire   t4    = x[14] &     x[ 0];
       #wire   t5    = t4    ^     t1   ;
       #wire   t6    = x[ 3] &     x[12];
       #wire   t7    = x[16] &     x[ 7];
       #wire   t8    = t0    ^     t6   ;
       #wire   t9    = x[15] &     x[13];
       #wire   t10   = t9    ^     t6   ;
       #wire   t11   = x[ 1] &     x[11];
       #wire   t12   = x[ 4] &     x[20];
       #wire   t13   = t12   ^     t11  ;
       #wire   t14   = x[ 2] &     x[ 8];
       #wire   t15   = t14   ^     t11  ;
       #wire   t16   = t3    ^     t2   ;
       #wire   t17   = t5    ^     x[18];
       #wire   t18   = t8    ^     t7   ;
       #wire   t19   = t10   ^     t15  ;
       #wire   t20   = t16   ^     t13  ;
       #wire   t21   = t17   ^     t15  ;
       #wire   t22   = t18   ^     t13  ;
       #wire   t23   = t19   ^     x[19];
       #wire   t24   = t22   ^     t23  ;
       #wire   t25   = t22   &     t20  ;
       #wire   t26   = t21   ^     t25  ;
       #wire   t27   = t20   ^     t21  ;
       #wire   t28   = t23   ^     t25  ;
       #wire   t29   = t28   &     t27  ;
       #wire   t30   = t26   &     t24  ;
       #wire   t31   = t20   &     t23  ;
       #wire   t32   = t27   &     t31  ;
       #wire   t33   = t27   ^     t25  ;
       #wire   t34   = t21   &     t22  ;
       #wire   t35   = t24   &     t34  ;
       #wire   t36   = t24   ^     t25  ;
       #wire   t37   = t21   ^     t29  ;
       #wire   t38   = t32   ^     t33  ;
       #wire   t39   = t23   ^     t30  ;
       #wire   t40   = t35   ^     t36  ;
       #wire   t41   = t38   ^     t40  ;
       #wire   t42   = t37   ^     t39  ;
       #wire   t43   = t37   ^     t38  ;
       #wire   t44   = t39   ^     t40  ;
       #wire   t45   = t42   ^     t41  ;
       #assign y[ 0] = t38   &     x[ 7];
       #assign y[ 1] = t37   &     x[13];
       #assign y[ 2] = t42   &     x[11];
       #assign y[ 3] = t45   &     x[20];
       #assign y[ 4] = t41   &     x[ 8];
       #assign y[ 5] = t44   &     x[ 9];
       #assign y[ 6] = t40   &     x[17];
       #assign y[ 7] = t39   &     x[14];
       #assign y[ 8] = t43   &     x[ 3];
       #assign y[ 9] = t38   &     x[16];
       #assign y[10] = t37   &     x[15];
       #assign y[11] = t42   &     x[ 1];
       #assign y[12] = t45   &     x[ 4];
       #assign y[13] = t41   &     x[ 2];
       #assign y[14] = t44   &     x[ 5];
       #assign y[15] = t40   &     x[ 6];
       #assign y[16] = t39   &     x[ 0];
       #assign y[17] = t43   &     x[12];
       #
       #endmodule
       """.stripMargin('#'))
}


