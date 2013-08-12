package rocket
package constants

import Chisel._
import scala.math._

trait ScalarOpConstants {
  val SZ_BR = 3
  val BR_X    = Bits("b???", 3)
  val BR_EQ   = Bits(0, 3)
  val BR_NE   = Bits(1, 3)
  val BR_J    = Bits(2, 3)
  val BR_N    = Bits(3, 3)
  val BR_LT   = Bits(4, 3)
  val BR_GE   = Bits(5, 3)
  val BR_LTU  = Bits(6, 3)
  val BR_GEU  = Bits(7, 3)

  val PC_EX4 = UInt(0, 2)
  val PC_EX  = UInt(1, 2)
  val PC_WB  = UInt(2, 2)
  val PC_PCR = UInt(3, 2)

  val A2_X     = Bits("b???", 3)
  val A2_BTYPE = UInt(0, 3);
  val A2_LTYPE = UInt(1, 3);
  val A2_ITYPE = UInt(2, 3);
  val A2_ZERO  = UInt(4, 3);
  val A2_JTYPE = UInt(5, 3);
  val A2_RTYPE = UInt(6, 3);

  val X = Bits("b?", 1)
  val N = Bits(0, 1)
  val Y = Bits(1, 1)

  val WA_X  = UInt("b?", 1)
  val WA_RD = UInt(0, 1)
  val WA_RA = UInt(1, 1)

  val WB_X   = UInt("b???", 3)
  val WB_PC  = UInt(0, 3);
  val WB_ALU = UInt(2, 3);
  val WB_TSC = UInt(4, 3);
  val WB_IRT = UInt(5, 3);

  val SZ_DW = 1
  val DW_X  = X
  val DW_32 = N
  val DW_64 = Y
  val DW_XPR = Y

  val RA = UInt(1, 5);
}

trait InterruptConstants {
  val CAUSE_INTERRUPT = 32
}
 
trait VectorOpConstants {
  val VEC_X = Bits("b??", 2).toUInt
  val VEC_FN_N = UInt(0, 2)
  val VEC_VL = UInt(1, 2)
  val VEC_CFG = UInt(2, 2)
  val VEC_CFGVL = UInt(3, 2)

  val VCMD_I = UInt(0, 3)
  val VCMD_F = UInt(1, 3)
  val VCMD_TX = UInt(2, 3)
  val VCMD_TF = UInt(3, 3)
  val VCMD_MX = UInt(4, 3)
  val VCMD_MF = UInt(5, 3)
  val VCMD_A = UInt(6, 3)
  val VCMD_X = UInt(0, 3)

  val VIMM_VLEN = UInt(0, 1)
  val VIMM_ALU = UInt(1, 1)
  val VIMM_X = UInt(0, 1)

  val VIMM2_RS2 = UInt(0, 1)
  val VIMM2_ALU = UInt(1, 1)
  val VIMM2_X = UInt(0, 1)
}
