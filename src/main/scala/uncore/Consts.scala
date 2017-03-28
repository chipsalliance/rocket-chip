// See LICENSE.Berkeley for license details.

package uncore
package constants

import Chisel._

object MemoryOpConstants extends MemoryOpConstants
trait MemoryOpConstants {
  val NUM_XA_OPS = 9
  val M_SZ      = 5
  def M_X       = BitPat("b?????");
  def M_XRD     = UInt("b00000"); // int load
  def M_XWR     = UInt("b00001"); // int store
  def M_PFR     = UInt("b00010"); // prefetch with intent to read
  def M_PFW     = UInt("b00011"); // prefetch with intent to write
  def M_XA_SWAP = UInt("b00100");
  def M_FLUSH_ALL = UInt("b00101")  // flush all lines
  def M_XLR     = UInt("b00110");
  def M_XSC     = UInt("b00111");
  def M_XA_ADD  = UInt("b01000");
  def M_XA_XOR  = UInt("b01001");
  def M_XA_OR   = UInt("b01010");
  def M_XA_AND  = UInt("b01011");
  def M_XA_MIN  = UInt("b01100");
  def M_XA_MAX  = UInt("b01101");
  def M_XA_MINU = UInt("b01110");
  def M_XA_MAXU = UInt("b01111");
  def M_FLUSH   = UInt("b10000") // write back dirty data and cede R/W permissions
  def M_PRODUCE = UInt("b10001") // write back dirty data and cede W permissions
  def M_CLEAN   = UInt("b10011") // write back dirty data and retain R/W permissions
  def M_SFENCE  = UInt("b10100") // flush TLB

  def isAMO(cmd: UInt) = (cmd(3).suggestName("cmd3Wire") ||
    (cmd === M_XA_SWAP).suggestName("mXaSwapWire")).suggestName("isAMOWire")
  def isPrefetch(cmd: UInt) = ((cmd === M_PFR).suggestName("pfrWire") ||
    (cmd === M_PFW).suggestName("pfwWire")).suggestName("isPrefetchWire")
  def isRead(cmd: UInt) = (((cmd === M_XRD).suggestName("xrdWire") ||
    (cmd === M_XLR).suggestName("xlrWire")).suggestName("xrdXlrWire") ||
    ((cmd === M_XSC).suggestName("xscWire") ||
    isAMO(cmd).suggestName("isAMOWire")).suggestName("xscAMOWire")).suggestName("isReadWire")
  def isWrite(cmd: UInt) = (((cmd === M_XWR).suggestName("xwrWire") ||
    (cmd === M_XSC).suggestName("xscWire")).suggestName("xwrXscWire") ||
    isAMO(cmd).suggestName("isAMOWire")).suggestName("isWriteWire")
  def isWriteIntent(cmd: UInt) = ((isWrite(cmd).suggestName("isWriteWire") ||
    (cmd === M_PFW).suggestName("pfwWire")).suggestName("writePfwWrite") ||
    (cmd === M_XLR).suggestName("xlrWire")).suggestName("isWriteIntent")
}

