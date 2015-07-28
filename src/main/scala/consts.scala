// See LICENSE for license details.

package rocket
package constants

import Chisel._
import scala.math._

trait ScalarOpConstants {
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
  val A2_FOUR = UInt(1, 2)
  val A2_RS2  = UInt(2, 2)
  val A2_IMM  = UInt(3, 2)

  val X = BitPat("b?")
  val N = BitPat("b0")
  val Y = BitPat("b1")

  val SZ_DW = 1
  val DW_X  = X
  val DW_32 = N
  val DW_64 = Y
  val DW_XPR = Y

  val SZ_PRV = 2
  val PRV_U = 0
  val PRV_S = 1
  val PRV_H = 2
  val PRV_M = 3
}
