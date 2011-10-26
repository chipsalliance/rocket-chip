package Top {

import Chisel._

object Constants
{
  val BR_N    = UFix(0, 4);
  val BR_EQ   = UFix(1, 4);
  val BR_NE   = UFix(2, 4);
  val BR_LT   = UFix(3, 4);
  val BR_LTU  = UFix(4, 4);
  val BR_GE   = UFix(5, 4);
  val BR_GEU  = UFix(6, 4);
  val BR_J    = UFix(7, 4);
  val BR_JR   = UFix(8, 4);

  val PC_4   = UFix(0, 3);
  val PC_BTB = UFix(1, 3);
  val PC_EX4 = UFix(2, 3);
  val PC_BR  = UFix(3, 3);
  val PC_J   = UFix(4, 3);
  val PC_JR  = UFix(5, 3);
  val PC_PCR = UFix(6, 3);

  val KF_Y  = UFix(1, 1);
  val KF_N  = UFix(0, 1);

  val REN_Y = UFix(1, 1);
  val REN_N = UFix(0, 1);

  val AS_X    = UFix(0, 1);
  val AS_IMM  = UFix(0, 1);
  val AS_RS2  = UFix(1, 1);

  val A2_X     = UFix(0, 2);
  val A2_0     = UFix(0, 2);
  val A2_SEXT  = UFix(1, 2);
  val A2_RS2   = UFix(2, 2);
  val A2_SPLIT = UFix(3, 2);

  val A1_X    = UFix(0, 1);
  val A1_RS1  = UFix(0, 1);
  val A1_LUI  = UFix(1, 1);

  val MUL_X     = UFix(0, 3);
  val MUL_NO    = UFix(0, 3);
  val MUL_64    = UFix(1, 3);
  val MUL_64H   = UFix(2, 3);
  val MUL_64HU  = UFix(3, 3);
  val MUL_64HSU = UFix(4, 3);
  val MUL_32    = UFix(5, 3);

  val DIV_X    = UFix(0, 4);
  val DIV_NO   = UFix(0, 4);
  val DIV_64D  = UFix(1, 4);
  val DIV_64DU = UFix(2, 4);
  val DIV_64R  = UFix(3, 4);
  val DIV_64RU = UFix(4, 4);
  val DIV_32D  = UFix(5, 4);
  val DIV_32DU = UFix(6, 4);
  val DIV_32R  = UFix(7, 4);
  val DIV_32RU = UFix(8, 4);

  val M_N = UFix(0, 1);
  val M_Y = UFix(1, 1);

  val WEN_N = UFix(0, 1);
  val WEN_Y = UFix(1, 1);

  val WA_X  = UFix(0, 1);
  val WA_RD = UFix(0, 1);
  val WA_RA = UFix(1, 1);

  val WB_X   = UFix(0, 3);
  val WB_PC  = UFix(0, 3);
  val WB_ALU = UFix(1, 3);
  val WB_PCR = UFix(2, 3);
  val WB_CR  = UFix(3, 3);
  val WB_MUL = UFix(4, 3);

  val N = UFix(0, 1);
  val Y = UFix(1, 1);
  val Y_SH = UFix(1, 1);

//  val FPU_N = UFix(0, 1);
//  val FPU_Y = FPU_N;

  val FWBQ_N = UFix(0, 1);
  val FWBQ_Y = UFix(1, 1);

  val FSDQ_N = UFix(0, 1);
  val FSDQ_Y = UFix(1, 1);

  val FN_X     = UFix(0, 4);
  val FN_ADD   = UFix(0, 4);
  val FN_SUB   = UFix(1, 4);
  val FN_SLT   = UFix(2, 4);
  val FN_SLTU  = UFix(3, 4);
  val FN_AND   = UFix(4, 4);
  val FN_OR    = UFix(5, 4);
  val FN_XOR   = UFix(6, 4);
  val FN_SL    = UFix(7, 4);
  val FN_SR    = UFix(8, 4);
  val FN_SRA   = UFix(9, 4);

  val DW_X  = UFix(0, 1);
  val DW_32 = UFix(0, 1);
  val DW_64 = UFix(1, 1);
  val DW_XPR = UFix(1, 1);

  val RA = UFix(1, 5);

  val MT_X  = Bits("b000", 3);
  val MT_B  = Bits("b000", 3);
  val MT_H  = Bits("b001", 3);
  val MT_W  = Bits("b010", 3);
  val MT_D  = Bits("b011", 3);
  val MT_BU = Bits("b100", 3);
  val MT_HU = Bits("b101", 3);
  val MT_WU = Bits("b110", 3);

  val M_X       = UFix(0, 4);
  val M_XRD     = Bits("b0000", 4); // int load
  val M_XWR     = Bits("b0001", 4); // int store
  val M_FRD     = Bits("b0010", 4); // fp load
  val M_FWR     = Bits("b0011", 4); // fp store
  val M_FLA     = Bits("b0100", 4); // flush cache
  val M_XA_ADD  = Bits("b1000", 4);
  val M_XA_SWAP = Bits("b1001", 4);
  val M_XA_AND  = Bits("b1010", 4);
  val M_XA_OR   = Bits("b1011", 4);
  val M_XA_MIN  = Bits("b1100", 4);
  val M_XA_MAX  = Bits("b1101", 4);
  val M_XA_MINU = Bits("b1110", 4);
  val M_XA_MAXU = Bits("b1111", 4);

  val PCR_STATUS   = UFix( 0, 5);
  val PCR_EPC      = UFix( 1, 5);
  val PCR_BADVADDR = UFix( 2, 5);
  val PCR_EVEC     = UFix( 3, 5);
  val PCR_COUNT    = UFix( 4, 5);
  val PCR_COMPARE  = UFix( 5, 5);
  val PCR_CAUSE    = UFix( 6, 5);
  val PCR_MEMSIZE  = UFix( 8, 5);
  val PCR_LOG      = UFix(10, 5);
  val PCR_TOHOST   = UFix(16, 5);
  val PCR_FROMHOST = UFix(17, 5);
  val PCR_CONSOLE  = UFix(18, 5);
  val PCR_K0       = UFix(24, 5);
  val PCR_K1       = UFix(25, 5);
}

}
