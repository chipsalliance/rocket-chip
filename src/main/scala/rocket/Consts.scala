// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket.constants

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._

trait ScalarOpConstants {
  val SZ_BR = 3
  def BR_X    = BitPat("b???")
  def BR_EQ   = 0.U(3.W)
  def BR_NE   = 1.U(3.W)
  def BR_J    = 2.U(3.W)
  def BR_N    = 3.U(3.W)
  def BR_LT   = 4.U(3.W)
  def BR_GE   = 5.U(3.W)
  def BR_LTU  = 6.U(3.W)
  def BR_GEU  = 7.U(3.W)

  def A1_X    = BitPat("b??")
  def A1_ZERO = 0.U(2.W)
  def A1_RS1  = 1.U(2.W)
  def A1_PC   = 2.U(2.W)

  def IMM_X  = BitPat("b???")
  def IMM_S  = 0.U(3.W)
  def IMM_SB = 1.U(3.W)
  def IMM_U  = 2.U(3.W)
  def IMM_UJ = 3.U(3.W)
  def IMM_I  = 4.U(3.W)
  def IMM_Z  = 5.U(3.W)

  def A2_X    = BitPat("b??")
  def A2_ZERO = 0.U(2.W)
  def A2_SIZE = 1.U(2.W)
  def A2_RS2  = 2.U(2.W)
  def A2_IMM  = 3.U(2.W)

  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")

  val SZ_DW = 1
  def DW_X  = X
  def DW_32 = false.B
  def DW_64 = true.B
  def DW_XPR = DW_64
}

trait MemoryOpConstants {
  val NUM_XA_OPS = 9
  val M_SZ      = 5
  def M_X       = BitPat("b?????");
  def M_XRD     = "b00000".U; // int load
  def M_XWR     = "b00001".U; // int store
  def M_PFR     = "b00010".U; // prefetch with intent to read
  def M_PFW     = "b00011".U; // prefetch with intent to write
  def M_XA_SWAP = "b00100".U
  def M_FLUSH_ALL = "b00101".U  // flush all lines
  def M_XLR     = "b00110".U
  def M_XSC     = "b00111".U
  def M_XA_ADD  = "b01000".U
  def M_XA_XOR  = "b01001".U
  def M_XA_OR   = "b01010".U
  def M_XA_AND  = "b01011".U
  def M_XA_MIN  = "b01100".U
  def M_XA_MAX  = "b01101".U
  def M_XA_MINU = "b01110".U
  def M_XA_MAXU = "b01111".U
  def M_FLUSH   = "b10000".U // write back dirty data and cede R/W permissions
  def M_PWR     = "b10001".U // partial (masked) store
  def M_PRODUCE = "b10010".U // write back dirty data and cede W permissions
  def M_CLEAN   = "b10011".U // write back dirty data and retain R/W permissions
  def M_SFENCE  = "b10100".U // SFENCE.VMA
  def M_HFENCEV = "b10101".U // HFENCE.VVMA
  def M_HFENCEG = "b10110".U // HFENCE.GVMA
  def M_WOK     = "b10111".U // check write permissions but don't perform a write
  def M_HLVX    = "b10000".U // HLVX instruction

  def isAMOLogical(cmd: UInt) = cmd.isOneOf(M_XA_SWAP, M_XA_XOR, M_XA_OR, M_XA_AND)
  def isAMOArithmetic(cmd: UInt) = cmd.isOneOf(M_XA_ADD, M_XA_MIN, M_XA_MAX, M_XA_MINU, M_XA_MAXU)
  def isAMO(cmd: UInt) = isAMOLogical(cmd) || isAMOArithmetic(cmd)
  def isPrefetch(cmd: UInt) = cmd === M_PFR || cmd === M_PFW
  def isRead(cmd: UInt) = cmd.isOneOf(M_XRD, M_HLVX, M_XLR, M_XSC) || isAMO(cmd)
  def isWrite(cmd: UInt) = cmd === M_XWR || cmd === M_PWR || cmd === M_XSC || isAMO(cmd)
  def isWriteIntent(cmd: UInt) = isWrite(cmd) || cmd === M_PFW || cmd === M_XLR
}
