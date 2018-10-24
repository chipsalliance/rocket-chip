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
      |  */
      |
      |  assign unpipelined = (insn[14:12] <= 3'b1);
      |  assign pipelined = 1'b0;
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
