// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket.constants

import Chisel._
import freechips.rocketchip.util._
import scala.math._

trait ScalarOpConstants {
  val MT_SZ = 3
  def MT_X  = BitPat("b???")
  def MT_B  = UInt("b000")
  def MT_H  = UInt("b001")
  def MT_W  = UInt("b010")
  def MT_D  = UInt("b011")
  def MT_BU = UInt("b100")
  def MT_HU = UInt("b101")
  def MT_WU = UInt("b110")
  def mtSize(mt: UInt) = mt(MT_SZ-2, 0)
  def mtSigned(mt: UInt) = !mt(MT_SZ-1)

  val SZ_BR = 3
  def BR_X    = BitPat("b???")
  def BR_EQ   = UInt(0, 3)
  def BR_NE   = UInt(1, 3)
  def BR_J    = UInt(2, 3)
  def BR_N    = UInt(3, 3)
  def BR_LT   = UInt(4, 3)
  def BR_GE   = UInt(5, 3)
  def BR_LTU  = UInt(6, 3)
  def BR_GEU  = UInt(7, 3)

  def A1_X    = BitPat("b??")
  def A1_ZERO = UInt(0, 2)
  def A1_RS1  = UInt(1, 2)
  def A1_PC   = UInt(2, 2)

  def IMM_X  = BitPat("b???")
  def IMM_S  = UInt(0, 3)
  def IMM_SB = UInt(1, 3)
  def IMM_U  = UInt(2, 3)
  def IMM_UJ = UInt(3, 3)
  def IMM_I  = UInt(4, 3)
  def IMM_Z  = UInt(5, 3)

  def A2_X    = BitPat("b??")
  def A2_ZERO = UInt(0, 2)
  def A2_SIZE = UInt(1, 2)
  def A2_RS2  = UInt(2, 2)
  def A2_IMM  = UInt(3, 2)

  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")

  val SZ_DW = 1
  def DW_X  = X
  def DW_32 = Bool(false)
  def DW_64 = Bool(true)
  def DW_XPR = DW_64
}

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
  def M_PWR     = UInt("b10001") // partial (masked) store
  def M_PRODUCE = UInt("b10010") // write back dirty data and cede W permissions
  def M_CLEAN   = UInt("b10011") // write back dirty data and retain R/W permissions
  def M_SFENCE  = UInt("b10100") // flush TLB

  def isAMOLogical(cmd: UInt) = cmd.isOneOf(M_XA_SWAP, M_XA_XOR, M_XA_OR, M_XA_AND)
  def isAMOArithmetic(cmd: UInt) = cmd.isOneOf(M_XA_ADD, M_XA_MIN, M_XA_MAX, M_XA_MINU, M_XA_MAXU)
  def isAMO(cmd: UInt) = isAMOLogical(cmd) || isAMOArithmetic(cmd)
  def isPrefetch(cmd: UInt) = cmd === M_PFR || cmd === M_PFW
  def isRead(cmd: UInt) = cmd === M_XRD || cmd === M_XLR || cmd === M_XSC || isAMO(cmd)
  def isWrite(cmd: UInt) = cmd === M_XWR || cmd === M_PWR || cmd === M_XSC || isAMO(cmd)
  def isWriteIntent(cmd: UInt) = isWrite(cmd) || cmd === M_PFW || cmd === M_XLR
}
