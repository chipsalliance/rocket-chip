// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import Chisel._

object APBParameters
{
  // These are all fixed by the AHB standard:
  val protBits  = 3

  def PROT_PRIVILEDGED = UInt(1, width = protBits)
  def PROT_NONSECURE   = UInt(2, width = protBits)
  def PROT_INSTRUCTION = UInt(4, width = protBits)
  def PROT_DEFAULT = PROT_PRIVILEDGED
}
