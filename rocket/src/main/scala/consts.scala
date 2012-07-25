package rocket

import Chisel._
import scala.math._

object Constants
{
  val NTILES = 1
  val HAVE_RVC = false
  val HAVE_FPU = true
  val HAVE_VEC = true

  val MAX_THREADS = hwacha.Constants.NUM_PVFB * hwacha.Constants.WIDTH_PVFB / hwacha.Constants.SZ_BANK

  val HTIF_WIDTH = 8
  val MEM_BACKUP_WIDTH = HTIF_WIDTH

  val BR_X    = Bits("b????", 4)
  val BR_N    = UFix(0, 4);
  val BR_EQ   = UFix(1, 4);
  val BR_NE   = UFix(2, 4);
  val BR_LT   = UFix(3, 4);
  val BR_LTU  = UFix(4, 4);
  val BR_GE   = UFix(5, 4);
  val BR_GEU  = UFix(6, 4);
  val BR_J    = UFix(7, 4);
  val BR_JR   = UFix(8, 4);

  val PC_4    = UFix(0, 3);
  val PC_BTB  = UFix(1, 3);
  val PC_EX4  = UFix(2, 3);
  val PC_BR   = UFix(3, 3);
  val PC_PCR  = UFix(4, 3);
  val PC_WB   = UFix(5, 3);
  val PC_EVEC = UFix(6, 3);
  val PC_JR   = UFix(7, 3);

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
  val N = UFix(0, 1);
  val Y = UFix(1, 1);

  val WA_X  = X
  val WA_RD = N
  val WA_RA = Y

  val WB_X   = Bits("b???", 3)
  val WB_PC  = UFix(0, 3);
  val WB_ALU = UFix(2, 3);
  val WB_TSC = UFix(4, 3);
  val WB_IRT = UFix(5, 3);

  val FN_X     = Bits("b????", 4)
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
  val FN_OP2   = UFix(10, 4);

  val DW_X  = X
  val DW_32 = N
  val DW_64 = Y
  val DW_XPR = Y

  val RA = UFix(1, 5);

  val MT_X  = Bits("b???", 3);
  val MT_B  = Bits("b000", 3);
  val MT_H  = Bits("b001", 3);
  val MT_W  = Bits("b010", 3);
  val MT_D  = Bits("b011", 3);
  val MT_BU = Bits("b100", 3);
  val MT_HU = Bits("b101", 3);
  val MT_WU = Bits("b110", 3);

  val M_X       = Bits("b????", 4);
  val M_XRD     = Bits("b0000", 4); // int load
  val M_XWR     = Bits("b0001", 4); // int store
  val M_PFR     = Bits("b0010", 4); // prefetch with intent to read
  val M_PFW     = Bits("b0011", 4); // prefetch with intent to write
  val M_FLA     = Bits("b0100", 4); // write back and invlaidate all lines
  val M_FENCE   = Bits("b0101", 4); // memory fence
  val M_INV     = Bits("b0110", 4); // write back and invalidate line
  val M_CLN     = Bits("b0111", 4); // write back line
  val M_XA_ADD  = Bits("b1000", 4);
  val M_XA_SWAP = Bits("b1001", 4);
  val M_XA_AND  = Bits("b1010", 4);
  val M_XA_OR   = Bits("b1011", 4);
  val M_XA_MIN  = Bits("b1100", 4);
  val M_XA_MAX  = Bits("b1101", 4);
  val M_XA_MINU = Bits("b1110", 4);
  val M_XA_MAXU = Bits("b1111", 4);

  val PCR_X = Bits("b???", 3)
  val PCR_N = Bits(0,3)
  val PCR_F = Bits(1,3) // mfpcr
  val PCR_T = Bits(4,3) // mtpcr
  val PCR_C = Bits(6,3) // clearpcr
  val PCR_S = Bits(7,3) // setpcr
  
  val SYNC_X    = Bits("b??", 2)
  val SYNC_N    = Bits(0,2);
  val SYNC_D    = Bits(1,2);
  val SYNC_I    = Bits(2,2);

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

  val CAUSE_INTERRUPT = 32
  val IRQ_IPI = 5
  val IRQ_TIMER = 7
  
  val PADDR_BITS = 40;
  val VADDR_BITS = 43;
  val PGIDX_BITS = 13;
  val PPN_BITS = PADDR_BITS-PGIDX_BITS;
  val VPN_BITS = VADDR_BITS-PGIDX_BITS;
  val ASID_BITS = 7;
  val PERM_BITS = 6;

  // rocketNBDCache parameters
  val DCACHE_PORTS = 3
  val CPU_DATA_BITS = 64;
  val CPU_TAG_BITS = 9;
  val DCACHE_TAG_BITS = log2Up(DCACHE_PORTS) + CPU_TAG_BITS
  val OFFSET_BITS = 6; // log2(cache line size in bytes)
  val NMSHR = if (HAVE_VEC) 4 else 2 // number of primary misses
  val NRPQ = 16; // number of secondary misses
  val NSDQ = 17; // number of secondary stores/AMOs
  val LG_REFILL_WIDTH = 4; // log2(cache bus width in bytes)
  val IDX_BITS = 7;
  val TAG_BITS = PADDR_BITS - OFFSET_BITS - IDX_BITS;
  val NWAYS = 4
  require(IDX_BITS+OFFSET_BITS <= PGIDX_BITS);

  // coherence parameters
  val ENABLE_SHARING = true
  val ENABLE_CLEAN_EXCLUSIVE = true

  val COHERENCE_DATA_BITS = (1 << OFFSET_BITS)*8 
  val TILE_ID_BITS = log2Up(NTILES)+1
  val TILE_XACT_ID_BITS = log2Up(NMSHR)+3
  val GLOBAL_XACT_ID_BITS = log2Up(NTILES*NMSHR)+1
  val NGLOBAL_XACTS = 1 << GLOBAL_XACT_ID_BITS

  val X_INIT_TYPE_MAX_BITS = 2
  val X_REP_TYPE_MAX_BITS = 3
  val P_REQ_TYPE_MAX_BITS = 2
  val P_REP_TYPE_MAX_BITS = 3

  // external memory interface
  val MEM_TAG_BITS = max(TILE_XACT_ID_BITS, GLOBAL_XACT_ID_BITS)
  val MEM_DATA_BITS = 128
  val REFILL_CYCLES = (1 << OFFSET_BITS)*8/MEM_DATA_BITS
  
  val DTLB_ENTRIES = 16
  val ITLB_ENTRIES = 8;
  val VITLB_ENTRIES = 4
  
  val START_ADDR = 0x2000;

  val FPU_N = UFix(0, 1);
  val FPU_Y = if (HAVE_FPU) UFix(1, 1) else FPU_N;

  val VEC_N = UFix(0, 1);
  val VEC_Y = if (HAVE_VEC) UFix(1, 1) else VEC_N;

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

  val DTLB_CPU = 0
  val DTLB_VEC = 1
  val DTLB_VPF = 2

  val DMEM_CPU = 0
  val DMEM_PTW = 1
  val DMEM_VU = 2
}
