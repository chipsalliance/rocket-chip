package uncore
package constants

import Chisel._
import scala.math.max

object MemoryOpConstants extends MemoryOpConstants
trait MemoryOpConstants {
  val MT_X  = Bits("b???", 3);
  val MT_B  = Bits("b000", 3);
  val MT_H  = Bits("b001", 3);
  val MT_W  = Bits("b010", 3);
  val MT_D  = Bits("b011", 3);
  val MT_BU = Bits("b100", 3);
  val MT_HU = Bits("b101", 3);
  val MT_WU = Bits("b110", 3);

  val M_SZ      = 5
  val M_X       = Bits("b?????");
  val M_XRD     = Bits("b00000"); // int load
  val M_XWR     = Bits("b00001"); // int store
  val M_PFR     = Bits("b00010"); // prefetch with intent to read
  val M_PFW     = Bits("b00011"); // prefetch with intent to write
  val M_XA_SWAP = Bits("b00100");
  val M_NOP     = Bits("b00101");
  val M_XLR     = Bits("b00110");
  val M_XSC     = Bits("b00111");
  val M_XA_ADD  = Bits("b01000");
  val M_XA_XOR  = Bits("b01001");
  val M_XA_OR   = Bits("b01010");
  val M_XA_AND  = Bits("b01011");
  val M_XA_MIN  = Bits("b01100");
  val M_XA_MAX  = Bits("b01101");
  val M_XA_MINU = Bits("b01110");
  val M_XA_MAXU = Bits("b01111");
  val M_INV     = Bits("b10000"); // write back and invalidate line
  val M_CLN     = Bits("b10001"); // write back line

  def isAMO(cmd: Bits) = cmd(3) || cmd === M_XA_SWAP
  def isPrefetch(cmd: Bits) = cmd === M_PFR || cmd === M_PFW
  def isRead(cmd: Bits) = cmd === M_XRD || cmd === M_XLR || isAMO(cmd)
  def isWrite(cmd: Bits) = cmd === M_XWR || cmd === M_XSC || isAMO(cmd)
  def isWriteIntent(cmd: Bits) = isWrite(cmd) || cmd === M_PFW || cmd === M_XLR
}

object AddressConstants extends AddressConstants
trait AddressConstants { 
  val PADDR_BITS = 32
  val VADDR_BITS = 43;
  val PGIDX_BITS = 13;
  val PPN_BITS = PADDR_BITS-PGIDX_BITS;
  val VPN_BITS = VADDR_BITS-PGIDX_BITS;
  val ASID_BITS = 7;
  val PERM_BITS = 6;
}

trait CacheConstants {
  val CACHE_DATA_SIZE_IN_BYTES = 1 << 6 
  val OFFSET_BITS = log2Up(CACHE_DATA_SIZE_IN_BYTES)
}

trait TileLinkSizeConstants {
  val ACQUIRE_WRITE_MASK_BITS = 6
  val ACQUIRE_SUBWORD_ADDR_BITS = 3
  val ACQUIRE_ATOMIC_OP_BITS = 4
}

trait MemoryInterfaceConstants extends 
  CacheConstants with 
  AddressConstants 
{
  val MEM_TAG_BITS = 5
  val MEM_DATA_BITS = 128
  val REFILL_CYCLES = CACHE_DATA_SIZE_IN_BYTES*8/MEM_DATA_BITS
  val MEM_ADDR_BITS = PADDR_BITS - OFFSET_BITS
}
