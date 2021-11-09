// See LICENSE.SiFive for license details.

package freechips.rocketchip.zk

import chisel3._
import chisel3.util.{BitPat, HasBlackBoxInline}

object ZKN {
  val AES32DSI    = BitPat("b??10101??????????000?????0110011")
  val AES32DSMI   = BitPat("b??10111??????????000?????0110011")
  val AES32ESI    = BitPat("b??10001??????????000?????0110011")
  val AES32ESMI   = BitPat("b??10011??????????000?????0110011")
  val AES64DS     = BitPat("b0011101??????????000?????0110011")
  val AES64DSM    = BitPat("b0011111??????????000?????0110011")
  val AES64ES     = BitPat("b0011001??????????000?????0110011")
  val AES64ESM    = BitPat("b0011011??????????000?????0110011")
  val AES64IM     = BitPat("b001100000000?????001?????0010011")
  val AES64KS1I   = BitPat("b00110001?????????001?????0010011")
  val AES64KS2    = BitPat("b0111111??????????000?????0110011")
  val SHA256SUM0  = BitPat("b000100000000?????001?????0010011")
  val SHA256SUM1  = BitPat("b000100000001?????001?????0010011")
  val SHA256SIG0  = BitPat("b000100000010?????001?????0010011")
  val SHA256SIG1  = BitPat("b000100000011?????001?????0010011")
  val SHA512SUM0  = BitPat("b000100000100?????001?????0010011")
  val SHA512SUM1  = BitPat("b000100000101?????001?????0010011")
  val SHA512SIG0  = BitPat("b000100000110?????001?????0010011")
  val SHA512SIG1  = BitPat("b000100000111?????001?????0010011")
  val SHA512SUM0R = BitPat("b0101000??????????000?????0110011")
  val SHA512SUM1R = BitPat("b0101001??????????000?????0110011")
  val SHA512SIG0L = BitPat("b0101010??????????000?????0110011")
  val SHA512SIG1L = BitPat("b0101011??????????000?????0110011")
  val SHA512SIG0H = BitPat("b0101110??????????000?????0110011")
  val SHA512SIG1H = BitPat("b0101111??????????000?????0110011")

  val FN_Len         = 4
  def FN_AES_DS      =  0.U(FN_Len.W)
  def FN_AES_DSM     =  1.U(FN_Len.W)
  def FN_AES_ES      =  2.U(FN_Len.W)
  def FN_AES_ESM     =  3.U(FN_Len.W)
  def FN_AES_IM      =  4.U(FN_Len.W)
  def FN_AES_KS1     =  5.U(FN_Len.W)
  def FN_AES_KS2     =  6.U(FN_Len.W)
  def FN_SHA256_SIG0 =  7.U(FN_Len.W)
  def FN_SHA256_SIG1 =  8.U(FN_Len.W)
  def FN_SHA256_SUM0 =  9.U(FN_Len.W)
  def FN_SHA256_SUM1 = 10.U(FN_Len.W)
  def FN_SHA512_SIG0 = 11.U(FN_Len.W)
  def FN_SHA512_SIG1 = 12.U(FN_Len.W)
  def FN_SHA512_SUM0 = 13.U(FN_Len.W)
  def FN_SHA512_SUM1 = 14.U(FN_Len.W)
}

class ZKNInterface(xLen: Int) extends Bundle {
  val zkn_fn = Input(UInt(ZKN.FN_Len.W))
  val valid  = Input(Bool())
  val hl     = Input(Bool())
  val bs     = Input(UInt(2.W))
  val rcon   = Input(UInt(4.W))
  val rs1    = Input(UInt(xLen.W))
  val rs2    = Input(UInt(xLen.W))
  val rd     = Output(UInt(xLen.W))
}
class ZKNDE32Interface(xLen: Int) extends Bundle {
  val zkn_fn = Input(UInt(ZKN.FN_Len.W))
  val valid  = Input(Bool())
  val bs     = Input(UInt(2.W))
  val rs1    = Input(UInt(xLen.W))
  val rs2    = Input(UInt(xLen.W))
  val rd     = Output(UInt(xLen.W))
}
class ZKNDE64Interface(xLen: Int) extends Bundle {
  val zkn_fn = Input(UInt(ZKN.FN_Len.W))
  val valid  = Input(Bool())
  val rcon   = Input(UInt(4.W))
  val rs1    = Input(UInt(xLen.W))
  val rs2    = Input(UInt(xLen.W))
  val rd     = Output(UInt(xLen.W))
}
class ZKNHInterface(xLen: Int) extends Bundle {
  val zkn_fn = Input(UInt(ZKN.FN_Len.W))
  val valid  = Input(Bool())
  val hl     = Input(Bool())
  val rs1    = Input(UInt(xLen.W))
  val rs2    = Input(UInt(xLen.W))
  val rd     = Output(UInt(xLen.W))
}
class ZKNImp(xLen:Int) extends Module {
  val io = IO(new ZKNInterface(xLen))

  val zknde_res = if (xLen == 64) {
    val zknde_u = Module(new zknde64)
    zknde_u.io.zkn_fn := io.zkn_fn
    zknde_u.io.valid  := io.valid
    zknde_u.io.rcon   := io.rcon
    zknde_u.io.rs1    := io.rs1
    zknde_u.io.rs2    := io.rs2
    zknde_u.io.rd
  } else if (xLen == 32) {
    val zknde_u = Module(new zknde32)
    zknde_u.io.zkn_fn := io.zkn_fn
    zknde_u.io.valid  := io.valid
    zknde_u.io.bs     := io.bs
    zknde_u.io.rs1    := io.rs1
    zknde_u.io.rs2    := io.rs2
    zknde_u.io.rd
  } else 0.U

  val zknh_res = if (xLen == 64) {
    val zknh_u = Module(new zknh64)
    zknh_u.io.zkn_fn := io.zkn_fn
    zknh_u.io.valid  := io.valid
    zknh_u.io.hl     := io.hl
    zknh_u.io.rs1    := io.rs1
    zknh_u.io.rs2    := io.rs2
    zknh_u.io.rd
  } else if (xLen == 32) {
    val zknh_u = Module(new zknh32)
    zknh_u.io.zkn_fn := io.zkn_fn
    zknh_u.io.valid  := io.valid
    zknh_u.io.hl     := io.hl
    zknh_u.io.rs1    := io.rs1
    zknh_u.io.rs2    := io.rs2
    zknh_u.io.rd
  } else 0.U

  io.rd := zknde_res | zknh_res
}

class zknde32 extends BlackBox with HasBlackBoxInline {
  val io = IO(new ZKNDE32Interface(32))
  setInline("zknde32.v",
    s"""
       #module zknde32  (
       #input  [ 3:0] zkn_fn,
       #input         valid,
       #input  [ 1:0] bs,
       #input  [31:0] rs1,
       #input  [31:0] rs2,
       #output [31:0] rd);
       #//
       #// Multiply by 2 in GF(2^8) modulo 8'h1b
       #function [7:0] xtime2;
       #    input [7:0] a;
       #
       #    xtime2  = {a[6:0],1'b0} ^ (a[7] ? 8'h1b : 8'b0 );
       #
       #endfunction
       #
       #//
       #// Paired down multiply by X in GF(2^8)
       #function [7:0] xtimeN;
       #    input[7:0] a;
       #    input[3:0] b;
       #
       #    xtimeN =
       #        (b[0] ?                         a   : 0) ^
       #        (b[1] ? xtime2(                 a)  : 0) ^
       #        (b[2] ? xtime2(xtime2(          a)) : 0) ^
       #        (b[3] ? xtime2(xtime2(xtime2(   a))): 0) ;
       #
       #endfunction
       #
       #localparam [3:0] FN_AES_DS   = 4'd00;
       #localparam [3:0] FN_AES_DSM  = 4'd01;
       #localparam [3:0] FN_AES_ES   = 4'd02;
       #localparam [3:0] FN_AES_ESM  = 4'd03;
       #
       #wire    decs_sel  = valid && (zkn_fn == FN_AES_DS);
       #wire    encs_sel  = valid && (zkn_fn == FN_AES_ES);
       #wire    decsm_sel = valid && (zkn_fn == FN_AES_DSM);
       #wire    encsm_sel = valid && (zkn_fn == FN_AES_ESM);
       #
       #wire    aes32_sel = decs_sel || encs_sel || decsm_sel || encsm_sel ;
       #
       #wire [7:0] sel_byte     = (bs == 2'b00)? rs2[ 7: 0]:
       #                          (bs == 2'b01)? rs2[15: 8]:
       #                          (bs == 2'b10)? rs2[23:16]:
       #                         /*bs == 2'b11*/ rs2[31:24];
       #
       #wire       dec          =  decs_sel  || decsm_sel  ;
       #wire       mix          =  encsm_sel || decsm_sel  ;
       #
       #wire [7:0] sbox_fwd_out;
       #wire [7:0] sbox_inv_out;
       #wire [7:0] sbox_out     = dec ? sbox_inv_out : sbox_fwd_out ;
       #
       #
       #wire [ 7:0] mix_b3 =       xtimeN(sbox_out, (dec ? 11  : 3))            ;
       #wire [ 7:0] mix_b2 = dec ? xtimeN(sbox_out, (           13)) : sbox_out ;
       #wire [ 7:0] mix_b1 = dec ? xtimeN(sbox_out, (            9)) : sbox_out ;
       #wire [ 7:0] mix_b0 =       xtimeN(sbox_out, (dec ? 14  : 2))            ;
       #
       #wire [31:0] result_mix  = {mix_b3, mix_b2, mix_b1, mix_b0};
       #
       #wire [31:0] result      = mix ? result_mix : {24'b0, sbox_out};
       #
       #wire [31:0] rotated     = {32{bs == 2'b00}} & {result                      } |
       #                          {32{bs == 2'b01}} & {result[23:0], result[31:24] } |
       #                          {32{bs == 2'b10}} & {result[15:0], result[31:16] } |
       #                          {32{bs == 2'b11}} & {result[ 7:0], result[31: 8] } ;
       #
       #assign      rd          = {32{aes32_sel}} & (rotated ^ rs1);
       #
       #//
       #// SBOX instances
       #// ------------------------------------------------------------
       #aes_fwd_sbox i_aes_sbox_fwd (
       #.in (sel_byte    ),
       #.fx (sbox_fwd_out)
       #);
       #
       #aes_inv_sbox i_aes_sbox_inv (
       #.in (sel_byte    ),
       #.fx (sbox_inv_out)
       #);
       #
       #endmodule
       #
       #//
       #// Forward AES SBox
       #module aes_fwd_sbox (
       #output [7:0] fx,
       #input  [7:0] in
       #);
       #
       #wire [20:0] t1;
       #wire [17:0] t2;
       #
       #aes_sbox_top     top ( .y(t1), .x(in) );
       #aes_sbox_inv_mid mid ( .y(t2), .x(t1) );
       #aes_sbox_out     out ( .y(fx), .x(t2) );
       #
       #endmodule
       #
       #module aes_sbox_top(
       #input   [ 7:0] x,
       #output  [20:0] y
       #);
       #
       #wire   y0    = x[ 0] ;
       #wire   y1    = x[ 7] ^     x[ 4];
       #wire   y2    = x[ 7] ^     x[ 2];
       #wire   y3    = x[ 7] ^     x[ 1];
       #wire   y4    = x[ 4] ^     x[ 2];
       #wire   t0    = x[ 3] ^     x[ 1];
       #wire   y5    = y1    ^     t0   ;
       #wire   t1    = x[ 6] ^     x[ 5];
       #wire   y6    = x[ 0] ^     y5   ;
       #wire   y7    = x[ 0] ^     t1   ;
       #wire   y8    = y5    ^     t1   ;
       #wire   t2    = x[ 6] ^     x[ 2];
       #wire   t3    = x[ 5] ^     x[ 2];
       #wire   y9    = y3    ^     y4   ;
       #wire   y10   = y5    ^     t2   ;
       #wire   y11   = t0    ^     t2   ;
       #wire   y12   = t0    ^     t3   ;
       #wire   y13   = y7    ^     y12  ;
       #wire   t4    = x[ 4] ^     x[ 0];
       #wire   y14   = t1    ^     t4   ;
       #wire   y15   = y1    ^     y14  ;
       #wire   t5    = x[ 1] ^     x[ 0];
       #wire   y16   = t1    ^     t5   ;
       #wire   y17   = y2    ^     y16  ;
       #wire   y18   = y2    ^     y8   ;
       #wire   y19   = y15   ^     y13  ;
       #wire   y20   = y1    ^     t3   ;
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
       #
       #endmodule
       #
       #module aes_sbox_out(
       #input   [17:0] x    ,
       #output  [ 7:0] y
       #);
       #
       #wire    t0   = x[11] ^  x[12];
       #wire    t1   = x[0] ^   x[6];
       #wire    t2   = x[14] ^  x[16];
       #wire    t3   = x[15] ^  x[5];
       #wire    t4   = x[4] ^   x[8];
       #wire    t5   = x[17] ^  x[11];
       #wire    t6   = x[12] ^  t5;
       #wire    t7   = x[14] ^  t3;
       #wire    t8   = x[1] ^   x[9];
       #wire    t9   = x[2] ^   x[3];
       #wire    t10  = x[3] ^   t4;
       #wire    t11  = x[10] ^  t2;
       #wire    t12  = x[16] ^  x[1];
       #wire    t13  = x[0] ^   t0;
       #wire    t14  = x[2] ^   x[11];
       #wire    t15  = x[5] ^   t1;
       #wire    t16  = x[6] ^   t0;
       #wire    t17  = x[7] ^   t1;
       #wire    t18  = x[8] ^   t8;
       #wire    t19  = x[13] ^  t4;
       #wire    t20  = t0 ^     t1;
       #wire    t21  = t1 ^     t7;
       #wire    t22  = t3 ^     t12;
       #wire    t23  = t18 ^    t2;
       #wire    t24  = t15 ^    t9;
       #wire    t25  = t6 ^     t10;
       #wire    t26  = t7 ^     t9;
       #wire    t27  = t8 ^     t10;
       #wire    t28  = t11 ^    t14;
       #wire    t29  = t11 ^    t17;
       #assign  y[0] = t6 ^~  t23;
       #assign  y[1] = t13 ^~ t27;
       #assign  y[2] = t25 ^  t29;
       #assign  y[3] = t20 ^  t22;
       #assign  y[4] = t6 ^   t21;
       #assign  y[5] = t19 ^~ t28;
       #assign  y[6] = t16 ^~ t26;
       #assign  y[7] = t6 ^   t24;
       #endmodule
       #
       #//
       #// Inverse AES Sbox
       #module aes_inv_sbox (
       #output [7:0] fx,
       #input  [7:0] in
       #);
       #
       #wire [20:0] t1;
       #wire [17:0] t2;
       #
       #aes_inv_sbox_top top ( .y(t1), .x(in) );
       #aes_sbox_inv_mid mid ( .y(t2), .x(t1) );
       #aes_inv_sbox_out out ( .y(fx), .x(t2) );
       #
       #endmodule
       #
       #module aes_inv_sbox_top(
       #output  [20:0] y    ,
       #input   [ 7:0] x
       #);
       #
       #wire  y17 = x[ 7] ^     x[ 4];
       #wire  y16 = x[ 6] ^~ x[ 4];
       #wire  y2  = x[ 7] ^~ x[ 6];
       #wire  y1  = x[ 4] ^     x[ 3];
       #wire  y18 = x[ 3] ^~ x[ 0];
       #wire  t0  = x[ 1] ^     x[ 0];
       #wire  y6  = x[ 6] ^~ y17 ;
       #wire  y14 = y16  ^     t0;
       #wire  y7  = x[ 0] ^~ y1;
       #wire  y8  = y2  ^     y18;
       #wire  y9  = y2  ^     t0;
       #wire  y3  = y1  ^     t0;
       #wire  y19 = x[ 5] ^~ y1;
       #wire  t1  = x[ 6] ^    x[ 1];
       #wire  y13 = x[ 5] ^~ y14;
       #wire  y15 = y18  ^     t1;
       #wire  y4  = x[ 3] ^     y6;
       #wire  t2  = x[ 5] ^~ x[ 2];
       #wire  t3  = x[ 2] ^~ x[ 1];
       #wire  t4  = x[ 5] ^~ x[ 3];
       #wire  y5  = y16  ^     t2 ;
       #wire  y12 = t1  ^     t4 ;
       #wire  y20 = y1  ^     t3 ;
       #wire  y11 = y8  ^     y20 ;
       #wire  y10 = y8  ^     t3 ;
       #wire  y0  = x[ 7] ^     t2 ;
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
       #module aes_inv_sbox_out(
       #output  [ 7:0] y,
       #input   [17:0] x
       #);
       #wire      t0  = x[ 2] ^     x[11];
       #wire      t1  = x[ 8] ^     x[ 9];
       #wire      t2  = x[ 4] ^     x[12];
       #wire      t3  = x[15] ^     x[ 0];
       #wire      t4  = x[16] ^     x[ 6];
       #wire      t5  = x[14] ^     x[ 1];
       #wire      t6  = x[17] ^     x[10];
       #wire      t7  = t0    ^     t1   ;
       #wire      t8  = x[ 0] ^     x[ 3];
       #wire      t9  = x[ 5] ^     x[13];
       #wire      t10 = x[ 7] ^     t4   ;
       #wire      t11 = t0    ^     t3   ;
       #wire      t12 = x[14] ^     x[16];
       #wire      t13 = x[17] ^     x[ 1];
       #wire      t14 = x[17] ^     x[12];
       #wire      t15 = x[ 4] ^     x[ 9];
       #wire      t16 = x[ 7] ^     x[11];
       #wire      t17 = x[ 8] ^     t2 ;
       #wire      t18 = x[13] ^     t5 ;
       #wire      t19 = t2   ^     t3 ;
       #wire      t20 = t4   ^     t6 ;
       #wire      t22 = t2   ^     t7 ;
       #wire      t23 = t7   ^     t8 ;
       #wire      t24 = t5   ^     t7 ;
       #wire      t25 = t6   ^     t10;
       #wire      t26 = t9   ^     t11;
       #wire      t27 = t10  ^     t18;
       #wire      t28 = t11  ^     t25;
       #wire      t29 = t15  ^     t20;
       #
       #assign  y[ 0] = t9   ^     t16;
       #assign  y[ 1] = t14  ^     t23;
       #assign  y[ 2] = t19  ^     t24;
       #assign  y[ 3] = t23  ^     t27;
       #assign  y[ 4] = t12  ^     t22;
       #assign  y[ 5] = t17  ^     t28;
       #assign  y[ 6] = t26  ^     t29;
       #assign  y[ 7] = t13  ^     t22;
       #
       #endmodule
       #
       #module aes_sbox_inv_mid(
       #input   [20:0] x,
       #output  [17:0] y
       #);
       #
       #wire    t0  = x[ 3] ^     x[12];
       #wire    t1  = x[ 9] &     x[ 5];
       #wire    t2  = x[17] &     x[ 6];
       #wire    t3  = x[10] ^     t1   ;
       #wire    t4  = x[14] &     x[ 0];
       #wire    t5  = t4    ^     t1   ;
       #wire    t6  = x[ 3] &     x[12];
       #wire    t7  = x[16] &     x[ 7];
       #wire    t8  = t0    ^     t6   ;
       #wire    t9  = x[15] &     x[13];
       #wire    t10 = t9    ^     t6   ;
       #wire    t11 = x[ 1] &     x[11];
       #wire    t12 = x[ 4] &     x[20];
       #wire    t13 = t12   ^     t11  ;
       #wire    t14 = x[ 2] &     x[ 8];
       #wire    t15 = t14   ^     t11  ;
       #wire    t16 = t3    ^     t2   ;
       #wire    t17 = t5    ^     x[18];
       #wire    t18 = t8    ^     t7   ;
       #wire    t19 = t10   ^     t15  ;
       #wire    t20 = t16   ^     t13  ;
       #wire    t21 = t17   ^     t15  ;
       #wire    t22 = t18   ^     t13  ;
       #wire    t23 = t19   ^     x[19];
       #wire    t24 = t22   ^     t23  ;
       #wire    t25 = t22   &     t20  ;
       #wire    t26 = t21   ^     t25  ;
       #wire    t27 = t20   ^     t21  ;
       #wire    t28 = t23   ^     t25  ;
       #wire    t29 = t28   &     t27  ;
       #wire    t30 = t26   &     t24  ;
       #wire    t31 = t20   &     t23  ;
       #wire    t32 = t27   &     t31  ;
       #wire    t33 = t27   ^     t25  ;
       #wire    t34 = t21   &     t22  ;
       #wire    t35 = t24   &     t34  ;
       #wire    t36 = t24   ^     t25  ;
       #wire    t37 = t21   ^     t29  ;
       #wire    t38 = t32   ^     t33  ;
       #wire    t39 = t23   ^     t30  ;
       #wire    t40 = t35   ^     t36  ;
       #wire    t41 = t38   ^     t40  ;
       #wire    t42 = t37   ^     t39  ;
       #wire    t43 = t37   ^     t38  ;
       #wire    t44 = t39   ^     t40  ;
       #wire    t45 = t42   ^     t41  ;
       #assign  y[ 0] = t38 &     x[ 7];
       #assign  y[ 1] = t37 &     x[13];
       #assign  y[ 2] = t42 &     x[11];
       #assign  y[ 3] = t45 &     x[20];
       #assign  y[ 4] = t41 &     x[ 8];
       #assign  y[ 5] = t44 &     x[ 9];
       #assign  y[ 6] = t40 &     x[17];
       #assign  y[ 7] = t39 &     x[14];
       #assign  y[ 8] = t43 &     x[ 3];
       #assign  y[ 9] = t38 &     x[16];
       #assign  y[10] = t37 &     x[15];
       #assign  y[11] = t42 &     x[ 1];
       #assign  y[12] = t45 &     x[ 4];
       #assign  y[13] = t41 &     x[ 2];
       #assign  y[14] = t44 &     x[ 5];
       #assign  y[15] = t40 &     x[ 6];
       #assign  y[16] = t39 &     x[ 0];
       #assign  y[17] = t43 &     x[12];
       #
       #endmodule
     """.stripMargin('#'))
}

class zknde64 extends BlackBox with HasBlackBoxInline {
  val io = IO(new ZKNDE64Interface(64))
  setInline("zknde64.v",
    s"""
       #module zknde64  (
       #input  [ 3:0] zkn_fn,
       #input  [ 3:0] rcon,
       #input         valid,
       #input  [63:0] rs1,
       #input  [63:0] rs2,
       #output [63:0] rd);
       #
       #// Select I'th byte of X.
       #`define BY(X,I) X[7+8*I:8*I]
       #
       #localparam [3:0] FN_AES_DS   = 4'd00;
       #localparam [3:0] FN_AES_DSM  = 4'd01;
       #localparam [3:0] FN_AES_ES   = 4'd02;
       #localparam [3:0] FN_AES_ESM  = 4'd03;
       #
       #localparam [3:0] FN_AES_IM   = 4'd04;
       #localparam [3:0] FN_AES_KS1  = 4'd05;
       #localparam [3:0] FN_AES_KS2  = 4'd06;
       #
       #wire     decs_sel = valid && (zkn_fn == FN_AES_DS);
       #wire     encs_sel = valid && (zkn_fn == FN_AES_ES);
       #wire    decsm_sel = valid && (zkn_fn == FN_AES_DSM);
       #wire    encsm_sel = valid && (zkn_fn == FN_AES_ESM);
       #wire       im_sel = valid && (zkn_fn == FN_AES_IM);
       #wire      ks1_sel = valid && (zkn_fn == FN_AES_KS1);
       #wire      ks2_sel = valid && (zkn_fn == FN_AES_KS2);
       #
       #// AES Round Constants
       #wire [ 7:0] RC [0:15];
       #assign RC[ 0] = 8'h01; assign RC[ 8] = 8'h1b;
       #assign RC[ 1] = 8'h02; assign RC[ 9] = 8'h36;
       #assign RC[ 2] = 8'h04; assign RC[10] = 8'h00;
       #assign RC[ 3] = 8'h08; assign RC[11] = 8'h00;
       #assign RC[ 4] = 8'h10; assign RC[12] = 8'h00;
       #assign RC[ 5] = 8'h20; assign RC[13] = 8'h00;
       #assign RC[ 6] = 8'h40; assign RC[14] = 8'h00;
       #assign RC[ 7] = 8'h80; assign RC[15] = 8'h00;
       #
       #//
       #// Shift Rows
       #// ------------------------------------------------------------
       #wire [31:0] row_0   = {`BY(rs1,0),`BY(rs1,4),`BY(rs2,0),`BY(rs2,4)};
       #wire [31:0] row_1   = {`BY(rs1,1),`BY(rs1,5),`BY(rs2,1),`BY(rs2,5)};
       #wire [31:0] row_2   = {`BY(rs1,2),`BY(rs1,6),`BY(rs2,2),`BY(rs2,6)};
       #wire [31:0] row_3   = {`BY(rs1,3),`BY(rs1,7),`BY(rs2,3),`BY(rs2,7)};
       #
       #// Forward shift rows
       #wire [31:0] fsh_1   = {row_1[23: 0], row_1[31:24]};
       #wire [31:0] fsh_2   = {row_2[15: 0], row_2[31:16]};
       #wire [31:0] fsh_3   = {row_3[ 7: 0], row_3[31: 8]};
       #
       #// Inverse shift rows
       #wire [31:0] ish_1   = {row_1[ 7: 0], row_1[31: 8]};
       #wire [31:0] ish_2   = {row_2[15: 0], row_2[31:16]};
       #wire [31:0] ish_3   = {row_3[23: 0], row_3[31:24]};
       #
       #//
       #// Re-construct columns from rows
       #wire [31:0] f_col_1 = {`BY(fsh_3,2),`BY(fsh_2,2),`BY(fsh_1,2),`BY(row_0,2)};
       #wire [31:0] f_col_0 = {`BY(fsh_3,3),`BY(fsh_2,3),`BY(fsh_1,3),`BY(row_0,3)};
       #
       #wire [31:0] i_col_1 = {`BY(ish_3,2),`BY(ish_2,2),`BY(ish_1,2),`BY(row_0,2)};
       #wire [31:0] i_col_0 = {`BY(ish_3,3),`BY(ish_2,3),`BY(ish_1,3),`BY(row_0,3)};
       #
       #//
       #// Hi/Lo selection
       #
       #wire [63:0] shiftrows_enc = {f_col_1, f_col_0};
       #wire [63:0] shiftrows_dec = {i_col_1, i_col_0};
       #
       #//
       #// SubBytes
       #// ------------------------------------------------------------
       #
       #//
       #// SBox input/output
       #wire [ 7:0] sb_fwd_in  [7:0];
       #wire [ 7:0] sb_inv     [7:0];
       #wire [ 7:0] sb_inv_in  [7:0];
       #wire [ 7:0] sb_fwd     [7:0];
       #
       #//
       #// KeySchedule 1 SBox input selection
       #wire        rcon_rot    = rcon != 4'hA;
       #wire [ 7:0] rconst      = rcon_rot ? RC[rcon] : 8'b0;
       #
       #wire [ 7:0] ks1_sb3     = rcon_rot ? rs1[39:32] : rs1[63:56];
       #wire [ 7:0] ks1_sb2     = rcon_rot ? rs1[63:56] : rs1[55:48];
       #wire [ 7:0] ks1_sb1     = rcon_rot ? rs1[55:48] : rs1[47:40];
       #wire [ 7:0] ks1_sb0     = rcon_rot ? rs1[47:40] : rs1[39:32];
       #
       #wire [31:0] ks1_sbout   = e_sbout[31:0] ^ {24'b0, rconst};
       #
       #// If just doing sub-bytes, sbox inputs direct from rs1.
       #assign      sb_fwd_in[0]= ks1_sel ? ks1_sb0 : `BY(shiftrows_enc, 0);
       #assign      sb_fwd_in[1]= ks1_sel ? ks1_sb1 : `BY(shiftrows_enc, 1);
       #assign      sb_fwd_in[2]= ks1_sel ? ks1_sb2 : `BY(shiftrows_enc, 2);
       #assign      sb_fwd_in[3]= ks1_sel ? ks1_sb3 : `BY(shiftrows_enc, 3);
       #assign      sb_fwd_in[4]=                     `BY(shiftrows_enc, 4);
       #assign      sb_fwd_in[5]=                     `BY(shiftrows_enc, 5);
       #assign      sb_fwd_in[6]=                     `BY(shiftrows_enc, 6);
       #assign      sb_fwd_in[7]=                     `BY(shiftrows_enc, 7);
       #
       #assign      sb_inv_in[0]= `BY(shiftrows_dec, 0);
       #assign      sb_inv_in[1]= `BY(shiftrows_dec, 1);
       #assign      sb_inv_in[2]= `BY(shiftrows_dec, 2);
       #assign      sb_inv_in[3]= `BY(shiftrows_dec, 3);
       #assign      sb_inv_in[4]= `BY(shiftrows_dec, 4);
       #assign      sb_inv_in[5]= `BY(shiftrows_dec, 5);
       #assign      sb_inv_in[6]= `BY(shiftrows_dec, 6);
       #assign      sb_inv_in[7]= `BY(shiftrows_dec, 7);
       #
       #// Decrypt sbox output
       #wire [63:0] d_sbout     = { sb_inv[7], sb_inv[6], sb_inv[5], sb_inv[4], sb_inv[3], sb_inv[2], sb_inv[1], sb_inv[0] };
       #// Encrypt sbox output
       #wire [63:0] e_sbout     = { sb_fwd[7], sb_fwd[6], sb_fwd[5], sb_fwd[4], sb_fwd[3], sb_fwd[2], sb_fwd[1], sb_fwd[0] };
       #
       #//
       #// MixColumns
       #// ------------------------------------------------------------
       #
       #// Forward MixColumns inputs.
       #wire [31:0] mix_enc_i0  =                               e_sbout[31: 0];
       #wire [31:0] mix_enc_i1  =                               e_sbout[63:32];
       #
       #// Inverse MixColumns inputs.
       #wire [31:0] mix_dec_i0  = im_sel ? rs1[31: 0] : d_sbout[31: 0];
       #wire [31:0] mix_dec_i1  = im_sel ? rs1[63:32] : d_sbout[63:32];
       #
       #// Forward MixColumns outputs.
       #wire [31:0] mix_enc_o0  ;
       #wire [31:0] mix_enc_o1  ;
       #
       #// Inverse MixColumns outputs.
       #wire [31:0] mix_dec_o0  ;
       #wire [31:0] mix_dec_o1  ;
       #
       #//
       #// Result gathering
       #// ------------------------------------------------------------
       #wire [63:0] result_imix = { mix_dec_o1                     , mix_dec_o0            };
       #wire [63:0] result_ks1  = { ks1_sbout                      , ks1_sbout             };
       #wire [63:0] result_ks2  = { rs1[63:32]^rs2[63:32]^rs2[31:0], rs1[63:32]^rs2[31: 0] };
       #
       #wire [63:0] result_enc  = (encsm_sel || decsm_sel) ? {mix_enc_o1, mix_enc_o0} : e_sbout  ;
       #wire [63:0] result_dec  = (encsm_sel || decsm_sel) ? {mix_dec_o1, mix_dec_o0} : d_sbout  ;
       #
       #wire        enc_sel     = encs_sel || encsm_sel;
       #wire        dec_sel     = decs_sel || decsm_sel;
       #
       #assign      rd          = {64{ ks1_sel }} & result_ks1 |
       #                          {64{ ks2_sel }} & result_ks2 |
       #                          {64{ enc_sel }} & result_enc |
       #                          {64{ dec_sel }} & result_dec |
       #                          {64{  im_sel }} & result_imix;
       #
       #// Generate AES SBox instances
       #genvar i;
       #generate
       #    for(i = 0; i < 8; i = i + 1) begin: saes64_sboxes
       #        aes_fwd_sbox i_fwd_sbox ( .in(sb_fwd_in [i]), .fx(sb_fwd[i]) );
       #        aes_inv_sbox i_inv_sbox ( .in(sb_inv_in [i]), .fx(sb_inv[i]) );
       #    end
       #endgenerate
       #
       #// Mix Column Instances
       #aes_mixcolumn #( .ENC(1) ) i_mix_e0( .col_in (mix_enc_i0), .col_out(mix_enc_o0) );
       #aes_mixcolumn #( .ENC(1) ) i_mix_e1( .col_in (mix_enc_i1), .col_out(mix_enc_o1) );
       #
       #aes_mixcolumn #( .ENC(0) ) i_mix_d0( .col_in (mix_dec_i0), .col_out(mix_dec_o0) );
       #aes_mixcolumn #( .ENC(0) ) i_mix_d1( .col_in (mix_dec_i1), .col_out(mix_dec_o1) );
       #
       #`undef BY
       #
       #endmodule
       #
       #//
       #// Forward AES SBox
       #module aes_fwd_sbox (
       #input  [7:0] in,
       #output [7:0] fx);
       #
       #wire [20:0] t1;
       #wire [17:0] t2;
       #
       #aes_sbox_top     top ( .y(t1), .x(in) );
       #aes_sbox_inv_mid mid ( .y(t2), .x(t1) );
       #aes_sbox_out     out ( .y(fx), .x(t2) );
       #
       #endmodule
       #
       #module aes_sbox_top(
       #input   [ 7:0] x,
       #output  [20:0] y);
       #
       #wire   y0    = x[ 0] ;
       #wire   y1    = x[ 7] ^     x[ 4];
       #wire   y2    = x[ 7] ^     x[ 2];
       #wire   y3    = x[ 7] ^     x[ 1];
       #wire   y4    = x[ 4] ^     x[ 2];
       #wire   t0    = x[ 3] ^     x[ 1];
       #wire   y5    = y1    ^     t0   ;
       #wire   t1    = x[ 6] ^     x[ 5];
       #wire   y6    = x[ 0] ^     y5   ;
       #wire   y7    = x[ 0] ^     t1   ;
       #wire   y8    = y5    ^     t1   ;
       #wire   t2    = x[ 6] ^     x[ 2];
       #wire   t3    = x[ 5] ^     x[ 2];
       #wire   y9    = y3    ^     y4   ;
       #wire   y10   = y5    ^     t2   ;
       #wire   y11   = t0    ^     t2   ;
       #wire   y12   = t0    ^     t3   ;
       #wire   y13   = y7    ^     y12  ;
       #wire   t4    = x[ 4] ^     x[ 0];
       #wire   y14   = t1    ^     t4   ;
       #wire   y15   = y1    ^     y14  ;
       #wire   t5    = x[ 1] ^     x[ 0];
       #wire   y16   = t1    ^     t5   ;
       #wire   y17   = y2    ^     y16  ;
       #wire   y18   = y2    ^     y8   ;
       #wire   y19   = y15   ^     y13  ;
       #wire   y20   = y1    ^     t3   ;
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
       #
       #endmodule
       #
       #module aes_sbox_out(
       #input   [17:0] x,
       #output  [ 7:0] y);
       #
       #wire    t0   = x[11] ^  x[12];
       #wire    t1   = x[0] ^   x[6];
       #wire    t2   = x[14] ^  x[16];
       #wire    t3   = x[15] ^  x[5];
       #wire    t4   = x[4] ^   x[8];
       #wire    t5   = x[17] ^  x[11];
       #wire    t6   = x[12] ^  t5;
       #wire    t7   = x[14] ^  t3;
       #wire    t8   = x[1] ^   x[9];
       #wire    t9   = x[2] ^   x[3];
       #wire    t10  = x[3] ^   t4;
       #wire    t11  = x[10] ^  t2;
       #wire    t12  = x[16] ^  x[1];
       #wire    t13  = x[0] ^   t0;
       #wire    t14  = x[2] ^   x[11];
       #wire    t15  = x[5] ^   t1;
       #wire    t16  = x[6] ^   t0;
       #wire    t17  = x[7] ^   t1;
       #wire    t18  = x[8] ^   t8;
       #wire    t19  = x[13] ^  t4;
       #wire    t20  = t0 ^     t1;
       #wire    t21  = t1 ^     t7;
       #wire    t22  = t3 ^     t12;
       #wire    t23  = t18 ^    t2;
       #wire    t24  = t15 ^    t9;
       #wire    t25  = t6 ^     t10;
       #wire    t26  = t7 ^     t9;
       #wire    t27  = t8 ^     t10;
       #wire    t28  = t11 ^    t14;
       #wire    t29  = t11 ^    t17;
       #assign  y[0] = t6 ^~  t23;
       #assign  y[1] = t13 ^~ t27;
       #assign  y[2] = t25 ^  t29;
       #assign  y[3] = t20 ^  t22;
       #assign  y[4] = t6 ^   t21;
       #assign  y[5] = t19 ^~ t28;
       #assign  y[6] = t16 ^~ t26;
       #assign  y[7] = t6 ^   t24;
       #endmodule
       #
       #//
       #// Inverse AES Sbox
       #module aes_inv_sbox (
       #input  [7:0] in,
       #output [7:0] fx);
       #
       #wire [20:0] t1;
       #wire [17:0] t2;
       #
       #aes_inv_sbox_top top ( .y(t1), .x(in) );
       #aes_sbox_inv_mid mid ( .y(t2), .x(t1) );
       #aes_inv_sbox_out out ( .y(fx), .x(t2) );
       #
       #endmodule
       #
       #module aes_inv_sbox_top(
       #input   [ 7:0] x,
       #output  [20:0] y);
       #
       #wire  y17 = x[ 7] ^     x[ 4];
       #wire  y16 = x[ 6] ^~ x[ 4];
       #wire  y2  = x[ 7] ^~ x[ 6];
       #wire  y1  = x[ 4] ^     x[ 3];
       #wire  y18 = x[ 3] ^~ x[ 0];
       #wire  t0  = x[ 1] ^     x[ 0];
       #wire  y6  = x[ 6] ^~ y17 ;
       #wire  y14 = y16  ^     t0;
       #wire  y7  = x[ 0] ^~ y1;
       #wire  y8  = y2  ^     y18;
       #wire  y9  = y2  ^     t0;
       #wire  y3  = y1  ^     t0;
       #wire  y19 = x[ 5] ^~ y1;
       #wire  t1  = x[ 6] ^    x[ 1];
       #wire  y13 = x[ 5] ^~ y14;
       #wire  y15 = y18  ^     t1;
       #wire  y4  = x[ 3] ^     y6;
       #wire  t2  = x[ 5] ^~ x[ 2];
       #wire  t3  = x[ 2] ^~ x[ 1];
       #wire  t4  = x[ 5] ^~ x[ 3];
       #wire  y5  = y16  ^     t2 ;
       #wire  y12 = t1  ^     t4 ;
       #wire  y20 = y1  ^     t3 ;
       #wire  y11 = y8  ^     y20 ;
       #wire  y10 = y8  ^     t3 ;
       #wire  y0  = x[ 7] ^     t2 ;
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
       #module aes_inv_sbox_out(
       #input   [17:0] x,
       #output  [ 7:0] y);
       #
       #wire      t0  = x[ 2] ^     x[11];
       #wire      t1  = x[ 8] ^     x[ 9];
       #wire      t2  = x[ 4] ^     x[12];
       #wire      t3  = x[15] ^     x[ 0];
       #wire      t4  = x[16] ^     x[ 6];
       #wire      t5  = x[14] ^     x[ 1];
       #wire      t6  = x[17] ^     x[10];
       #wire      t7  = t0    ^     t1   ;
       #wire      t8  = x[ 0] ^     x[ 3];
       #wire      t9  = x[ 5] ^     x[13];
       #wire      t10 = x[ 7] ^     t4   ;
       #wire      t11 = t0    ^     t3   ;
       #wire      t12 = x[14] ^     x[16];
       #wire      t13 = x[17] ^     x[ 1];
       #wire      t14 = x[17] ^     x[12];
       #wire      t15 = x[ 4] ^     x[ 9];
       #wire      t16 = x[ 7] ^     x[11];
       #wire      t17 = x[ 8] ^     t2 ;
       #wire      t18 = x[13] ^     t5 ;
       #wire      t19 = t2   ^     t3 ;
       #wire      t20 = t4   ^     t6 ;
       #wire      t22 = t2   ^     t7 ;
       #wire      t23 = t7   ^     t8 ;
       #wire      t24 = t5   ^     t7 ;
       #wire      t25 = t6   ^     t10;
       #wire      t26 = t9   ^     t11;
       #wire      t27 = t10  ^     t18;
       #wire      t28 = t11  ^     t25;
       #wire      t29 = t15  ^     t20;
       #
       #assign  y[ 0] = t9   ^     t16;
       #assign  y[ 1] = t14  ^     t23;
       #assign  y[ 2] = t19  ^     t24;
       #assign  y[ 3] = t23  ^     t27;
       #assign  y[ 4] = t12  ^     t22;
       #assign  y[ 5] = t17  ^     t28;
       #assign  y[ 6] = t26  ^     t29;
       #assign  y[ 7] = t13  ^     t22;
       #
       #endmodule
       #
       #module aes_sbox_inv_mid(
       #input   [20:0] x,
       #output  [17:0] y);
       #
       #wire    t0  = x[ 3] ^     x[12];
       #wire    t1  = x[ 9] &     x[ 5];
       #wire    t2  = x[17] &     x[ 6];
       #wire    t3  = x[10] ^     t1   ;
       #wire    t4  = x[14] &     x[ 0];
       #wire    t5  = t4    ^     t1   ;
       #wire    t6  = x[ 3] &     x[12];
       #wire    t7  = x[16] &     x[ 7];
       #wire    t8  = t0    ^     t6   ;
       #wire    t9  = x[15] &     x[13];
       #wire    t10 = t9    ^     t6   ;
       #wire    t11 = x[ 1] &     x[11];
       #wire    t12 = x[ 4] &     x[20];
       #wire    t13 = t12   ^     t11  ;
       #wire    t14 = x[ 2] &     x[ 8];
       #wire    t15 = t14   ^     t11  ;
       #wire    t16 = t3    ^     t2   ;
       #wire    t17 = t5    ^     x[18];
       #wire    t18 = t8    ^     t7   ;
       #wire    t19 = t10   ^     t15  ;
       #wire    t20 = t16   ^     t13  ;
       #wire    t21 = t17   ^     t15  ;
       #wire    t22 = t18   ^     t13  ;
       #wire    t23 = t19   ^     x[19];
       #wire    t24 = t22   ^     t23  ;
       #wire    t25 = t22   &     t20  ;
       #wire    t26 = t21   ^     t25  ;
       #wire    t27 = t20   ^     t21  ;
       #wire    t28 = t23   ^     t25  ;
       #wire    t29 = t28   &     t27  ;
       #wire    t30 = t26   &     t24  ;
       #wire    t31 = t20   &     t23  ;
       #wire    t32 = t27   &     t31  ;
       #wire    t33 = t27   ^     t25  ;
       #wire    t34 = t21   &     t22  ;
       #wire    t35 = t24   &     t34  ;
       #wire    t36 = t24   ^     t25  ;
       #wire    t37 = t21   ^     t29  ;
       #wire    t38 = t32   ^     t33  ;
       #wire    t39 = t23   ^     t30  ;
       #wire    t40 = t35   ^     t36  ;
       #wire    t41 = t38   ^     t40  ;
       #wire    t42 = t37   ^     t39  ;
       #wire    t43 = t37   ^     t38  ;
       #wire    t44 = t39   ^     t40  ;
       #wire    t45 = t42   ^     t41  ;
       #assign  y[ 0] = t38 &     x[ 7];
       #assign  y[ 1] = t37 &     x[13];
       #assign  y[ 2] = t42 &     x[11];
       #assign  y[ 3] = t45 &     x[20];
       #assign  y[ 4] = t41 &     x[ 8];
       #assign  y[ 5] = t44 &     x[ 9];
       #assign  y[ 6] = t40 &     x[17];
       #assign  y[ 7] = t39 &     x[14];
       #assign  y[ 8] = t43 &     x[ 3];
       #assign  y[ 9] = t38 &     x[16];
       #assign  y[10] = t37 &     x[15];
       #assign  y[11] = t42 &     x[ 1];
       #assign  y[12] = t45 &     x[ 4];
       #assign  y[13] = t41 &     x[ 2];
       #assign  y[14] = t44 &     x[ 5];
       #assign  y[15] = t40 &     x[ 6];
       #assign  y[16] = t39 &     x[ 0];
       #assign  y[17] = t43 &     x[12];
       #
       #endmodule
       #
       #//
       #// AES MixColumn Word module
       #module aes_mixcolumn #(parameter ENC =1) (
       #input   wire [31:0] col_in,
       #output  wire [31:0] col_out );
       #
       #wire [ 7:0] b0 = col_in[ 7: 0];
       #wire [ 7:0] b1 = col_in[15: 8];
       #wire [ 7:0] b2 = col_in[23:16];
       #wire [ 7:0] b3 = col_in[31:24];
       #
       #wire [31:0] mix_in_3 = {b3, b0, b1, b2};
       #wire [31:0] mix_in_2 = {b2, b3, b0, b1};
       #wire [31:0] mix_in_1 = {b1, b2, b3, b0};
       #wire [31:0] mix_in_0 = {b0, b1, b2, b3};
       #
       #wire [ 7:0]             mix_out_3, mix_out_2, mix_out_1, mix_out_0;
       #assign      col_out  = {mix_out_3, mix_out_2, mix_out_1, mix_out_0};
       #
       #aes_mixcolumn_byte #(.ENC(ENC)) i_mc_enc_0(.col_in(mix_in_0), .byte_out(mix_out_0));
       #aes_mixcolumn_byte #(.ENC(ENC)) i_mc_enc_1(.col_in(mix_in_1), .byte_out(mix_out_1));
       #aes_mixcolumn_byte #(.ENC(ENC)) i_mc_enc_2(.col_in(mix_in_2), .byte_out(mix_out_2));
       #aes_mixcolumn_byte #(.ENC(ENC)) i_mc_enc_3(.col_in(mix_in_3), .byte_out(mix_out_3));
       #
       #endmodule
       #
       #module aes_mixcolumn_byte # (parameter ENC = 1) (
       #input   wire [31:0] col_in,
       #output  wire [ 7:0] byte_out);
       #
       #// Multiply by 2 in GF(2^8) modulo 8'h1b
       #function [7:0] xt2;
       #    input [7:0] a;
       #    xt2 = (a << 1) ^ (a[7] ? 8'h1b : 8'b0) ;
       #endfunction
       #
       #// Paired down multiply by X in GF(2^8)
       #function [7:0] xtN;
       #    input[7:0] a;
       #    input[3:0] b;
       #    xtN = (b[0] ?             a   : 0) ^
       #          (b[1] ? xt2(        a)  : 0) ^
       #          (b[2] ? xt2(xt2(    a)) : 0) ^
       #          (b[3] ? xt2(xt2(xt2(a))): 0) ;
       #endfunction
       #
       #wire [7:0] b3 = col_in[ 7: 0];
       #wire [7:0] b2 = col_in[15: 8];
       #wire [7:0] b1 = col_in[23:16];
       #wire [7:0] b0 = col_in[31:24];
       #
       #generate if (ENC == 1) begin : doEnc
       #         assign byte_out = xtN(b0,4'd2) ^ xtN(b1,4'd3) ^ b2 ^ b3 ;
       #end else begin:                doDec
       #         assign byte_out = xtN(b0,4'he) ^ xtN(b1,4'hb) ^ xtN(b2,4'hd) ^ xtN(b3,4'h9);
       #end
       #endgenerate
       #
       #endmodule
       """.stripMargin('#'))
}

class zknh32 extends BlackBox with HasBlackBoxInline {
  val io = IO(new ZKNHInterface(32))
  setInline("zknh32.v",
    s"""
       #module zknh32(
       #input  [ 3:0] zkn_fn,
       #input         valid,
       #input         hl,
       #input  [31:0] rs1,
       #input  [31:0] rs2,
       #output [31:0] rd);
       #
       #`define RORI32(a,b) ((a >> b) | (a << 32-b))
       #`define SRLI32(a,b) ((a >> b)              )
       #`define SLLI32(a,b) ((a << b)              )
       #
       #localparam [3:0] FN_SHA256_SIG0 = 4'd7;
       #localparam [3:0] FN_SHA256_SIG1 = 4'd8;
       #localparam [3:0] FN_SHA256_SUM0 = 4'd9;
       #localparam [3:0] FN_SHA256_SUM1 = 4'd10;
       #localparam [3:0] FN_SHA512_SIG0 = 4'd11;
       #localparam [3:0] FN_SHA512_SIG1 = 4'd12;
       #localparam [3:0] FN_SHA512_SUM0 = 4'd13;
       #localparam [3:0] FN_SHA512_SUM1 = 4'd14;
       #
       #wire sha256_sig0_sel  = valid && (zkn_fn == FN_SHA256_SIG0);
       #wire sha256_sig1_sel  = valid && (zkn_fn == FN_SHA256_SIG1);
       #wire sha256_sum0_sel  = valid && (zkn_fn == FN_SHA256_SUM0);
       #wire sha256_sum1_sel  = valid && (zkn_fn == FN_SHA256_SUM1);
       #
       #wire sha512_sig0l_sel = valid && (zkn_fn == FN_SHA512_SIG0) && (~hl);
       #wire sha512_sig0h_sel = valid && (zkn_fn == FN_SHA512_SIG0) && ( hl);
       #wire sha512_sig1l_sel = valid && (zkn_fn == FN_SHA512_SIG1) && (~hl);
       #wire sha512_sig1h_sel = valid && (zkn_fn == FN_SHA512_SIG1) && ( hl);
       #wire sha512_sum0_sel  = valid && (zkn_fn == FN_SHA512_SUM0);
       #wire sha512_sum1_sel  = valid && (zkn_fn == FN_SHA512_SUM1);
       #
       #wire [31:0] wsha256_sig0  = `RORI32(rs1, 7) ^ `RORI32(rs1,18) ^ `SRLI32(rs1, 3);
       #wire [31:0] wsha256_sig1  = `RORI32(rs1,17) ^ `RORI32(rs1,19) ^ `SRLI32(rs1,10);
       #wire [31:0] wsha256_sum0  = `RORI32(rs1, 2) ^ `RORI32(rs1,13) ^ `RORI32(rs1,22);
       #wire [31:0] wsha256_sum1  = `RORI32(rs1, 6) ^ `RORI32(rs1,11) ^ `RORI32(rs1,25);
       #
       #wire [31:0] wsha512_sum0  = `SLLI32(rs1,25)^`SLLI32(rs1,30)^`SRLI32(rs1,28)^
       #                            `SRLI32(rs2, 7)^`SRLI32(rs2, 2)^`SLLI32(rs2, 4);
       #wire [31:0] wsha512_sum1  = `SLLI32(rs1,23)^`SRLI32(rs1,14)^`SRLI32(rs1,18)^
       #                            `SRLI32(rs2, 9)^`SLLI32(rs2,18)^`SLLI32(rs2,14);
       #wire [31:0] wsha512_sig0l = `SRLI32(rs1, 1)^`SRLI32(rs1, 7)^`SRLI32(rs1, 8)^
       #                            `SLLI32(rs2,31)^`SLLI32(rs2,25)^`SLLI32(rs2,24);
       #wire [31:0] wsha512_sig0h = `SRLI32(rs1, 1)^`SRLI32(rs1, 7)^`SRLI32(rs1, 8)^
       #                            `SLLI32(rs2,31)                ^`SLLI32(rs2,24);
       #wire [31:0] wsha512_sig1l = `SLLI32(rs1, 3)^`SRLI32(rs1, 6)^`SRLI32(rs1,19)^
       #                            `SRLI32(rs2,29)^`SLLI32(rs2,26)^`SLLI32(rs2,13);
       #wire [31:0] wsha512_sig1h = `SLLI32(rs1, 3)^`SRLI32(rs1, 6)^`SRLI32(rs1,19)^
       #                            `SRLI32(rs2,29)                ^`SLLI32(rs2,13);
       #
       #assign      rd            = {32{sha256_sig0_sel }} & wsha256_sig0  |
       #                            {32{sha256_sig1_sel }} & wsha256_sig1  |
       #                            {32{sha256_sum0_sel }} & wsha256_sum0  |
       #                            {32{sha256_sum1_sel }} & wsha256_sum1  |
       #                            {32{sha512_sig0l_sel}} & wsha512_sig0l |
       #                            {32{sha512_sig0h_sel}} & wsha512_sig0h |
       #                            {32{sha512_sig1l_sel}} & wsha512_sig1l |
       #                            {32{sha512_sig1h_sel}} & wsha512_sig1h |
       #                            {32{sha512_sum0_sel }} & wsha512_sum0  |
       #                            {32{sha512_sum1_sel }} & wsha512_sum1  ;
       #
       #endmodule
       """.stripMargin('#'))
}
class zknh64 extends BlackBox with HasBlackBoxInline {
  val io = IO(new ZKNHInterface(64))
  setInline("zknh64.v",
    s"""
       #module zknh64  (
       #input  [ 3:0] zkn_fn,
       #input         valid,
       #input         hl,
       #input  [63:0] rs1,
       #input  [63:0] rs2,
       #output [63:0] rd);
       #
       #`define RORI32(a,b) ((a >> b) | (a << 32-b))
       #`define RORI64(a,b) ((a >> b) | (a << 64-b))
       #`define SRLI32(a,b) ((a >> b)              )
       #`define SLLI32(a,b) ((a << b)              )
       #`define SRLI64(a,b) ((a >> b)              )
       #
       #localparam [3:0] FN_SHA256_SIG0 = 4'd7;
       #localparam [3:0] FN_SHA256_SIG1 = 4'd8;
       #localparam [3:0] FN_SHA256_SUM0 = 4'd9;
       #localparam [3:0] FN_SHA256_SUM1 = 4'd10;
       #localparam [3:0] FN_SHA512_SIG0 = 4'd11;
       #localparam [3:0] FN_SHA512_SIG1 = 4'd12;
       #localparam [3:0] FN_SHA512_SUM0 = 4'd13;
       #localparam [3:0] FN_SHA512_SUM1 = 4'd14;
       #
       #wire sha256_sig0_sel = valid && (zkn_fn == FN_SHA256_SIG0);
       #wire sha256_sig1_sel = valid && (zkn_fn == FN_SHA256_SIG1);
       #wire sha256_sum0_sel = valid && (zkn_fn == FN_SHA256_SUM0);
       #wire sha256_sum1_sel = valid && (zkn_fn == FN_SHA256_SUM1);
       #
       #wire sha512_sig0_sel = valid && (zkn_fn == FN_SHA512_SIG0);
       #wire sha512_sig1_sel = valid && (zkn_fn == FN_SHA512_SIG1);
       #wire sha512_sum0_sel = valid && (zkn_fn == FN_SHA512_SUM0);
       #wire sha512_sum1_sel = valid && (zkn_fn == FN_SHA512_SUM1);
       #
       #wire [31:0] rs1l = rs1[31:0];
       #
       #wire [31:0] wsha256_sig0  = `RORI32(rs1l, 7) ^ `RORI32(rs1l,18) ^ `SRLI32(rs1l, 3);
       #wire [31:0] wsha256_sig1  = `RORI32(rs1l,17) ^ `RORI32(rs1l,19) ^ `SRLI32(rs1l,10);
       #wire [31:0] wsha256_sum0  = `RORI32(rs1l, 2) ^ `RORI32(rs1l,13) ^ `RORI32(rs1l,22);
       #wire [31:0] wsha256_sum1  = `RORI32(rs1l, 6) ^ `RORI32(rs1l,11) ^ `RORI32(rs1l,25);
       #
       #wire [31:0] sha256_res    = {32{sha256_sig0_sel }} & wsha256_sig0 |
       #                            {32{sha256_sig1_sel }} & wsha256_sig1 |
       #                            {32{sha256_sum0_sel }} & wsha256_sum0 |
       #                            {32{sha256_sum1_sel }} & wsha256_sum1 ;
       #
       #wire [63:0] wsha512_sig0 = `RORI64(rs1, 1) ^ `RORI64(rs1, 8) ^`SRLI64(rs1, 7);
       #wire [63:0] wsha512_sig1 = `RORI64(rs1,19) ^ `RORI64(rs1,61) ^`SRLI64(rs1, 6);
       #wire [63:0] wsha512_sum0 = `RORI64(rs1,28) ^ `RORI64(rs1,34) ^`RORI64(rs1,39);
       #wire [63:0] wsha512_sum1 = `RORI64(rs1,14) ^ `RORI64(rs1,18) ^`RORI64(rs1,41);
       #
       #assign      rd           =               { 32'd0 ,  sha256_res  } |
       #                           {64{sha512_sig0_sel}} & wsha512_sig0   |
       #                           {64{sha512_sig1_sel}} & wsha512_sig1   |
       #                           {64{sha512_sum0_sel}} & wsha512_sum0   |
       #                           {64{sha512_sum1_sel}} & wsha512_sum1   ;
       #
       #endmodule
       """.stripMargin('#'))
}

