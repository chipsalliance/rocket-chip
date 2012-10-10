package uncore

import Chisel._
import scala.math._

object Constants
{
  val NTILES = 1
  val HAVE_RVC = false
  val HAVE_FPU = true
  val HAVE_VEC = true

  val HTIF_WIDTH = 16
  val MEM_BACKUP_WIDTH = HTIF_WIDTH

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
  val NGLOBAL_XACTS = 8
  val GLOBAL_XACT_ID_BITS = log2Up(NGLOBAL_XACTS)

  val X_INIT_TYPE_MAX_BITS = 2
  val X_INIT_WRITE_MASK_BITS = OFFSET_BITS
  val X_INIT_SUBWORD_ADDR_BITS = log2Up(OFFSET_BITS)
  val X_INIT_ATOMIC_OP_BITS = 4
  val X_REP_TYPE_MAX_BITS = 3
  val P_REQ_TYPE_MAX_BITS = 2
  val P_REP_TYPE_MAX_BITS = 3

  // external memory interface
  val MEM_TAG_BITS = max(TILE_XACT_ID_BITS, GLOBAL_XACT_ID_BITS)
  val MEM_DATA_BITS = 128
  val REFILL_CYCLES = (1 << OFFSET_BITS)*8/MEM_DATA_BITS

}
