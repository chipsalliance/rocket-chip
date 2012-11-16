package rocket
package constants

import Chisel._
import scala.math._

abstract trait TileConfigConstants {
  def HAVE_RVC: Boolean
  def HAVE_FPU: Boolean
  def HAVE_VEC: Boolean
  val FPU_N = UFix(0, 1)
  val FPU_Y = if (HAVE_FPU) UFix(1, 1) else FPU_N
  val VEC_N = UFix(0, 1);
  val VEC_Y = if (HAVE_VEC) UFix(1, 1) else VEC_N
}

trait ScalarOpConstants {
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

  val MUL_X   = Bits("b??", 2)
  val MUL_LO  = UFix(0, 2);
  val MUL_H   = UFix(1, 2);
  val MUL_HSU = UFix(2, 2);
  val MUL_HU  = UFix(3, 2);

  val DIV_X  = Bits("b??", 2)
  val DIV_D  = UFix(0, 2);
  val DIV_DU = UFix(1, 2);
  val DIV_R  = UFix(2, 2);
  val DIV_RU = UFix(3, 2);

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

  val DW_X  = X
  val DW_32 = N
  val DW_64 = Y
  val DW_XPR = Y

  val RA = UFix(1, 5);
}

trait PCRConstants {
  val PCR_X = Bits("b???", 3)
  val PCR_N = Bits(0,3)
  val PCR_F = Bits(1,3) // mfpcr
  val PCR_T = Bits(4,3) // mtpcr
  val PCR_C = Bits(6,3) // clearpcr
  val PCR_S = Bits(7,3) // setpcr
  
  val PCR_STATUS   = UFix( 0, 5);
  val PCR_EPC      = UFix( 1, 5);
  val PCR_BADVADDR = UFix( 2, 5);
  val PCR_EVEC     = UFix( 3, 5);
  val PCR_COUNT    = UFix( 4, 5);
  val PCR_COMPARE  = UFix( 5, 5);
  val PCR_CAUSE    = UFix( 6, 5);
  val PCR_PTBR     = UFix( 7, 5);
  val PCR_SEND_IPI = UFix( 8, 5);
  val PCR_CLR_IPI  = UFix( 9, 5);
  val PCR_COREID   = UFix(10, 5);
  val PCR_IMPL     = UFix(11, 5);
  val PCR_K0       = UFix(12, 5);
  val PCR_K1       = UFix(13, 5);
  val PCR_VECBANK  = UFix(18, 5);
  val PCR_VECCFG   = UFix(19, 5);
  val PCR_RESET    = UFix(29, 5);
  val PCR_TOHOST   = UFix(30, 5);
  val PCR_FROMHOST = UFix(31, 5);

  // definition of bits in PCR status reg
  val SR_ET   = 0;  // enable traps
  val SR_EF   = 1;  // enable floating point
  val SR_EV   = 2;  // enable vector unit
  val SR_EC   = 3;  // enable compressed instruction encoding
  val SR_PS   = 4;  // mode stack bit
  val SR_S    = 5;  // user/supervisor mode
  val SR_U64  = 6;  // 64 bit user mode
  val SR_S64  = 7;  // 64 bit supervisor mode
  val SR_VM   = 8   // VM enable
  val SR_IM   = 16  // interrupt mask
  val SR_IM_WIDTH = 8
}

trait InterruptConstants {
  val CAUSE_INTERRUPT = 32
  val IRQ_IPI = 5
  val IRQ_TIMER = 7
}
 
abstract trait RocketDcacheConstants extends TileConfigConstants with uncore.constants.CacheConstants with uncore.constants.AddressConstants {
  require(OFFSET_BITS == log2Up(uncore.Constants.CACHE_DATA_SIZE_IN_BYTES))
  require(OFFSET_BITS <= uncore.Constants.X_INIT_WRITE_MASK_BITS)
  require(log2Up(OFFSET_BITS) <= uncore.Constants.X_INIT_SUBWORD_ADDR_BITS)
}

trait TLBConstants {
  val BTB_ENTRIES = 8
  val ITLB_ENTRIES = 8
  val DTLB_ENTRIES = 16
  val VITLB_ENTRIES = 4
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
