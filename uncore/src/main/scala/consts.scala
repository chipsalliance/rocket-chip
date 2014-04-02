package uncore
package constants

import Chisel._

object MemoryOpConstants extends MemoryOpConstants
trait MemoryOpConstants {
  val MT_SZ = 3
  val MT_X  = Bits("b???")
  val MT_B  = Bits("b000")
  val MT_H  = Bits("b001")
  val MT_W  = Bits("b010")
  val MT_D  = Bits("b011")
  val MT_BU = Bits("b100")
  val MT_HU = Bits("b101")
  val MT_WU = Bits("b110")

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
