// See LICENSE for license details.

package rocket
package constants

import Chisel._
import scala.math._

trait ScalarOpConstants {
  val MT_SZ = 3
  val MT_X  = BitPat("b???")
  val MT_B  = UInt("b000")
  val MT_H  = UInt("b001")
  val MT_W  = UInt("b010")
  val MT_D  = UInt("b011")
  val MT_BU = UInt("b100")
  val MT_HU = UInt("b101")
  val MT_WU = UInt("b110")
  def mtSize(mt: UInt) = mt(MT_SZ-2, 0)
  def mtSigned(mt: UInt) = !mt(MT_SZ-1)

  val SZ_BR = 3
  val BR_X    = BitPat("b???")
  val BR_EQ   = UInt(0, 3)
  val BR_NE   = UInt(1, 3)
  val BR_J    = UInt(2, 3)
  val BR_N    = UInt(3, 3)
  val BR_LT   = UInt(4, 3)
  val BR_GE   = UInt(5, 3)
  val BR_LTU  = UInt(6, 3)
  val BR_GEU  = UInt(7, 3)

  val A1_X    = BitPat("b??")
  val A1_ZERO = UInt(0, 2)
  val A1_RS1  = UInt(1, 2)
  val A1_PC   = UInt(2, 2)

  val IMM_X  = BitPat("b???")
  val IMM_S  = UInt(0, 3)
  val IMM_SB = UInt(1, 3)
  val IMM_U  = UInt(2, 3)
  val IMM_UJ = UInt(3, 3)
  val IMM_I  = UInt(4, 3)
  val IMM_Z  = UInt(5, 3)

  val A2_X    = BitPat("b??")
  val A2_ZERO = UInt(0, 2)
  val A2_SIZE = UInt(1, 2)
  val A2_RS2  = UInt(2, 2)
  val A2_IMM  = UInt(3, 2)

  val X = BitPat("b?")
  val N = BitPat("b0")
  val Y = BitPat("b1")

  val SZ_DW = 1
  val DW_X  = X
  val DW_32 = Bool(false)
  val DW_64 = Bool(true)
  val DW_XPR = DW_64
}
