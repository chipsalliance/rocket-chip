package uncore
package constants

import Chisel._
import scala.math.max

abstract trait CoherenceConfigConstants {
  val ENABLE_SHARING: Boolean
  val ENABLE_CLEAN_EXCLUSIVE: Boolean
}

trait UncoreConstants {
  val NGLOBAL_XACTS = 8
  val GLOBAL_XACT_ID_BITS = log2Up(NGLOBAL_XACTS)
  val CACHE_DATA_SIZE_IN_BYTES = 1 << 6 
}

trait CacheConstants extends UncoreConstants {
  val OFFSET_BITS = log2Up(CACHE_DATA_SIZE_IN_BYTES)
}

trait TileLinkTypeConstants {
  val X_INIT_TYPE_MAX_BITS = 2
  val X_REP_TYPE_MAX_BITS = 3
  val P_REQ_TYPE_MAX_BITS = 2
  val P_REP_TYPE_MAX_BITS = 3
}

trait TileLinkSizeConstants extends 
  TileLinkTypeConstants
{
  val TILE_XACT_ID_BITS = 5
  val X_INIT_WRITE_MASK_BITS = 6
  val X_INIT_SUBWORD_ADDR_BITS = 3
  val X_INIT_ATOMIC_OP_BITS = 4
}

trait MemoryOpConstants {
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
}

trait MemoryInterfaceConstants extends 
  UncoreConstants with 
  TileLinkSizeConstants 
{
  val MEM_TAG_BITS = max(TILE_XACT_ID_BITS, GLOBAL_XACT_ID_BITS)
  val MEM_DATA_BITS = 128
  val REFILL_CYCLES = CACHE_DATA_SIZE_IN_BYTES*8/MEM_DATA_BITS
}

trait AddressConstants { 
  val PADDR_BITS = 40;
  val VADDR_BITS = 43;
  val PGIDX_BITS = 13;
  val PPN_BITS = PADDR_BITS-PGIDX_BITS;
  val VPN_BITS = VADDR_BITS-PGIDX_BITS;
  val ASID_BITS = 7;
  val PERM_BITS = 6;
}

