// See LICENSE for license details.

package rocket
package constants

import Chisel._
import scala.math._

trait ScalarOpConstants {
  val SZ_BR = 3
  val BR_X    = Bits("b???", 3)
  val BR_EQ   = Bits(0, 3)
  val BR_NE   = Bits(1, 3)
  val BR_J    = Bits(2, 3)
  val BR_N    = Bits(3, 3)
  val BR_LT   = Bits(4, 3)
  val BR_GE   = Bits(5, 3)
  val BR_LTU  = Bits(6, 3)
  val BR_GEU  = Bits(7, 3)

  val PC_EX  = UInt(0, 2)
  val PC_MEM = UInt(1, 2)
  val PC_WB  = UInt(2, 2)
  val PC_PCR = UInt(3, 2)

  val A1_X    = Bits("b??", 2)
  val A1_ZERO = UInt(0, 2)
  val A1_RS1  = UInt(1, 2)
  val A1_PC   = UInt(2, 2)

  val IMM_X  = Bits("b???", 3)
  val IMM_S  = UInt(0, 3)
  val IMM_SB = UInt(1, 3)
  val IMM_U  = UInt(2, 3)
  val IMM_UJ = UInt(3, 3)
  val IMM_I  = UInt(4, 3)
  val IMM_Z  = UInt(5, 3)

  val A2_X    = Bits("b??", 2)
  val A2_ZERO = UInt(0, 2)
  val A2_FOUR = UInt(1, 2)
  val A2_RS2  = UInt(2, 2)
  val A2_IMM  = UInt(3, 2)

  val X = Bool.DC
  val N = Bool(false)
  val Y = Bool(true)

  val NBYP = 4
  val SZ_BYP = log2Up(NBYP)
  val BYP_0   = 0
  val BYP_EX  = 1
  val BYP_MEM = 2
  val BYP_DC  = 3

  val SZ_DW = 1
  val DW_X  = X
  val DW_32 = N
  val DW_64 = Y
  val DW_XPR = Y

  val RA = UInt(1, 5)
}
