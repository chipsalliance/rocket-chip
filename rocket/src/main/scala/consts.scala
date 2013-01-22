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

  val PC_EX4 = UFix(0, 2)
  val PC_EX  = UFix(1, 2)
  val PC_WB  = UFix(2, 2)
  val PC_PCR = UFix(3, 2)

  val A2_X     = Bits("b???", 3)
  val A2_BTYPE = UFix(0, 3);
  val A2_LTYPE = UFix(1, 3);
  val A2_ITYPE = UFix(2, 3);
  val A2_ZERO  = UFix(4, 3);
  val A2_JTYPE = UFix(5, 3);
  val A2_RTYPE = UFix(6, 3);

  val X = Bits("b?", 1)
  val N = Bits(0, 1);
  val Y = Bits(1, 1);

  val WA_X  = X
  val WA_RD = N
  val WA_RA = Y

  val WB_X   = Bits("b???", 3)
  val WB_PC  = UFix(0, 3);
  val WB_ALU = UFix(2, 3);
  val WB_TSC = UFix(4, 3);
  val WB_IRT = UFix(5, 3);

  val SZ_DW = 1
  val DW_X  = X
  val DW_32 = N
  val DW_64 = Y
  val DW_XPR = Y

  val RA = UFix(1, 5);
}

trait InterruptConstants {
  val CAUSE_INTERRUPT = 32
  val IRQ_IPI = 5
  val IRQ_TIMER = 7
}
 
abstract trait RocketDcacheConstants extends uncore.constants.CacheConstants with uncore.constants.AddressConstants {
  require(OFFSET_BITS == log2Up(uncore.Constants.CACHE_DATA_SIZE_IN_BYTES))
  require(OFFSET_BITS <= uncore.Constants.ACQUIRE_WRITE_MASK_BITS)
  require(log2Up(OFFSET_BITS) <= uncore.Constants.ACQUIRE_SUBWORD_ADDR_BITS)
}

trait VectorOpConstants {
  val VEC_X = Bits("b??", 2).toUFix
  val VEC_FN_N = UFix(0, 2)
  val VEC_VL = UFix(1, 2)
  val VEC_CFG = UFix(2, 2)
  val VEC_CFGVL = UFix(3, 2)

  val VCMD_I = UFix(0, 3)
  val VCMD_F = UFix(1, 3)
  val VCMD_TX = UFix(2, 3)
  val VCMD_TF = UFix(3, 3)
  val VCMD_MX = UFix(4, 3)
  val VCMD_MF = UFix(5, 3)
  val VCMD_A = UFix(6, 3)
  val VCMD_X = UFix(0, 3)

  val VIMM_VLEN = UFix(0, 1)
  val VIMM_ALU = UFix(1, 1)
  val VIMM_X = UFix(0, 1)

  val VIMM2_RS2 = UFix(0, 1)
  val VIMM2_ALU = UFix(1, 1)
  val VIMM2_X = UFix(0, 1)
}
