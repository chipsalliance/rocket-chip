// See LICENSE.SiFive for license details.

package freechips.rocketchip.scie

import chisel3._
import chisel3.util.{BitPat, HasBlackBoxInline}
import chisel3.experimental.{IntParam, fromIntToIntParam}

object SCIE {
  val opcode = BitPat("b?????????????????????????0?01011")
  val iLen = 32
}

class SCIEDecoderInterface extends Bundle {
  val insn = Input(UInt(SCIE.iLen.W))
  val unpipelined = Output(Bool())
  val pipelined = Output(Bool())
  val multicycle = Output(Bool())
}

class SCIEDecoder extends BlackBox with HasBlackBoxInline {
  val io = IO(new SCIEDecoderInterface)

  setInline("SCIEDecoder.v",
    s"""
      |module SCIEDecoder (
      |    input  [${SCIE.iLen-1}:0] insn,
      |    output unpipelined,
      |    output pipelined,
      |    output multicycle);
      |
      |  /* This module decodes a SCIE instruction and indicates which functional unit
      |     to send the instruction to (unpipelined, pipelined, or multicycle).  The
      |     outputs are don't-cares unless insn lies within the custom-0 or custom-1
      |     major opcodes.  If it is within custom-0 or custom-1, then at most one of
      |     the outputs may be high.  If none are high, an illegal-instruction trap
      |     occurs.  If multiple are high, the behavior is undefined.
      |
      |     This example implementation permits Funct3 = 0 or 1 within both custom-0
      |     and custom-1 as Unpipelined instructions.
      |
      |     It also permits Funct3 = 2 or 3 within custom-0 as Pipelined instructions.
      |  */
      |
      |  wire [2:0] funct3 = insn[14:12];
      |
      |  assign unpipelined = funct3 <= 3'h1;
      |  assign pipelined = funct3 == 3'h2 || funct3 == 3'h3;
      |  assign multicycle = 1'b0;
      |
      |endmodule
     """.stripMargin)
}

class SCIEUnpipelinedInterface(xLen: Int) extends Bundle {
  val insn = Input(UInt(SCIE.iLen.W))
  val rs1 = Input(UInt(xLen.W))
  val rs2 = Input(UInt(xLen.W))
  val rd = Output(UInt(xLen.W))
}

class SCIEUnpipelined(xLen: Int) extends BlackBox(Map("XLEN" -> xLen)) with HasBlackBoxInline {
  val io = IO(new SCIEUnpipelinedInterface(xLen))

  setInline("SCIEUnpipelined.v",
    s"""
      |module SCIEUnpipelined #(parameter XLEN = 32) (
      |    input  [${SCIE.iLen-1}:0] insn,
      |    input  [XLEN-1:0] rs1,
      |    input  [XLEN-1:0] rs2,
      |    output [XLEN-1:0] rd);
      |
      |  /* This example SCIE implementation provides the following instructions:
      |
      |     Major opcode custom-0:
      |     Funct3 = 0: MIN (rd = rs1 < rs2 ? rs1 : rs2)
      |     Funct3 = 1: MAX (rd = rs1 > rs2 ? rs1 : rs2)
      |
      |     Major opcode custom-1:
      |     Funct3 = 0: MINI (rd = rs1 < imm[11:0] ? rs1 : imm[11:0])
      |     Funct3 = 1: MAXI (rd = rs1 > imm[11:0] ? rs1 : imm[11:0])
      |  */
      |
      |  /* Decode the instruction. */
      |  wire use_immediate = insn[5];
      |  wire pick_smaller = !insn[12];
      |
      |  /* Mux the operands. */
      |  wire [XLEN-1:0] immediate = {{(XLEN-12){insn[31]}},  insn[31:20]};
      |  wire [XLEN-1:0] rhs = use_immediate ? immediate : rs2;
      |  wire [XLEN-1:0] lhs = rs1;
      |
      |  /* Perform the computation. */
      |  wire lhs_smaller = $$signed(lhs) < $$signed(rhs);
      |  wire [XLEN-1:0] result = lhs_smaller == pick_smaller ? lhs : rhs;
      |
      |  /* Drive the output. */
      |  assign rd = result;
      |
      |endmodule
     """.stripMargin)
}

class SCIEPipelinedInterface(xLen: Int) extends Bundle {
  val clock = Input(Clock())
  val valid = Input(Bool())
  val insn = Input(UInt(SCIE.iLen.W))
  val rs1 = Input(UInt(xLen.W))
  val rs2 = Input(UInt(xLen.W))
  val rd = Output(UInt(xLen.W))
}

class SCIEPipelined(xLen: Int) extends BlackBox(Map("XLEN" -> xLen)) with HasBlackBoxInline {
  val io = IO(new SCIEPipelinedInterface(xLen))

  setInline("SCIEPipelined.v",
    s"""
      |module SCIEPipelined #(parameter XLEN = 32) (
      |    input clock,
      |    input valid,
      |    input [${SCIE.iLen-1}:0] insn,
      |    input [XLEN-1:0] rs1,
      |    input [XLEN-1:0] rs2,
      |    output [XLEN-1:0] rd);
      |
      |  /* This example SCIE implementation provides the following instructions:
      |
      |     Major opcode custom-0:
      |     Funct3 = 2: AD.U8, compute absolute differences of packed uint8
      |       rd[7:0] = abs(rs1[7:0] - rs2[7:0])
      |       rd[15:8] = abs(rs1[15:8] - rs2[15:8])
      |       ...
      |       rd[XLEN-1:XLEN-8] = abs(rs1[XLEN-1:XLEN-8] - rs2[XLEN-1:XLEN-8])
      |
      |     Funct3 = 3: SAD.U8, compute sum of absolute differences of packed uint8
      |       tmp[7:0] = abs(rs1[7:0] - rs2[7:0])
      |       tmp[15:8] = abs(rs1[15:8] - rs2[15:8])
      |       ...
      |       tmp[XLEN-1:XLEN-8] = abs(rs1[XLEN-1:XLEN-8] - rs2[XLEN-1:XLEN-8])
      |
      |       rd = tmp[7:0] + tmp[15:8] + ... + tmp[XLEN-1:XLEN-8]
      |  */
      |
      |  integer i;
      |  reg [XLEN-1:0] absolute_differences;
      |  reg funct3_0;
      |  reg [XLEN-1:0] result;
      |
      |  always @(posedge clock)
      |  begin
      |    /* Gating using the valid signal is optional, but saves power. */
      |    if (valid)
      |    begin
      |      /* Register Funct3[0] opcode bit for result muxing in next stage. */
      |      funct3_0 <= insn[12];
      |
      |      /* Compute each absolute difference and register each result. */
      |      for (i = 0; i < XLEN/8; i = i + 1)
      |      begin
      |        absolute_differences[8*i +: 8] <= rs1[8*i +: 8] < rs2[8*i +: 8] ?
      |                                          rs2[8*i +: 8] - rs1[8*i +: 8] :
      |                                          rs1[8*i +: 8] - rs2[8*i +: 8];
      |      end
      |    end
      |  end
      |
      |  /* In the second pipeline stage, compute the final result. */
      |  always @(*)
      |  begin
      |    if (!funct3_0)
      |    begin
      |      /* If Funct3[0] = 0, the output is the packed absolute differences. */
      |      result = absolute_differences;
      |    end
      |    else
      |    begin
      |      /* If Funct3[0] = 1, the output is their sum. */
      |      result = 0;
      |      for (i = 0; i < XLEN/8; i = i + 1)
      |      begin
      |        result = result + {{(XLEN-8){1'b0}}, absolute_differences[8*i +: 8]};
      |      end
      |    end
      |  end
      |
      |  /* Drive the output. */
      |  assign rd = result;
      |
      | /* Suppress Xs at simulation start */
      | `ifdef RANDOMIZE_REG_INIT
      | initial begin
      |   `ifndef VERILATOR
      |   #`RANDOMIZE_DELAY begin end
      |   `endif
      |   absolute_differences = {(XLEN / 32){`RANDOM}};
      |   funct3_0 = absolute_differences[0];
      | end
      | `endif
      |
      |endmodule
     """.stripMargin)
}
