// See LICENSE.Berkeley for license details.

package uncore
package constants

import Chisel._

object MemoryOpConstants extends MemoryOpConstants
trait MemoryOpConstants {
  val NUM_XA_OPS = 9
  val M_SZ      = 5
  val M_X       = BitPat("b?????");
  val M_XRD     = UInt("b00000"); // int load
  val M_XWR     = UInt("b00001"); // int store
  val M_PFR     = UInt("b00010"); // prefetch with intent to read
  val M_PFW     = UInt("b00011"); // prefetch with intent to write
  val M_XA_SWAP = UInt("b00100");
  val M_FLUSH_ALL = UInt("b00101")  // flush all lines
  val M_XLR     = UInt("b00110");
  val M_XSC     = UInt("b00111");
  val M_XA_ADD  = UInt("b01000");
  val M_XA_XOR  = UInt("b01001");
  val M_XA_OR   = UInt("b01010");
  val M_XA_AND  = UInt("b01011");
  val M_XA_MIN  = UInt("b01100");
  val M_XA_MAX  = UInt("b01101");
  val M_XA_MINU = UInt("b01110");
  val M_XA_MAXU = UInt("b01111");
  val M_FLUSH   = UInt("b10000") // write back dirty data and cede R/W permissions
  val M_PRODUCE = UInt("b10001") // write back dirty data and cede W permissions
  val M_CLEAN   = UInt("b10011") // write back dirty data and retain R/W permissions

  def isAMO(cmd: UInt) = cmd(3) || cmd === M_XA_SWAP
  def isPrefetch(cmd: UInt) = cmd === M_PFR || cmd === M_PFW
  def isRead(cmd: UInt) = cmd === M_XRD || cmd === M_XLR || cmd === M_XSC || isAMO(cmd)
  def isWrite(cmd: UInt) = cmd === M_XWR || cmd === M_XSC || isAMO(cmd)
  def isWriteIntent(cmd: UInt) = isWrite(cmd) || cmd === M_PFW || cmd === M_XLR
}

