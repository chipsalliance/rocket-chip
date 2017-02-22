// See LICENSE.Berkeley for license details.

package rocket
package constants

import Chisel._
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
