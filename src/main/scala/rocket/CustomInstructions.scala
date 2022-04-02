// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._

object CustomInstructions {
  def CEASE              = BitPat("b00110000010100000000000001110011")
  def CFLUSH_D_L1        = BitPat("b111111000000?????000000001110011")
  def CDISCARD_D_L1      = BitPat("b111111000010?????000000001110011")
}
