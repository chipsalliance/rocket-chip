// See LICENSE.SiFive for license details.

package uncore.apb

import Chisel._

object APBParameters
{
  // These are all fixed by the AHB standard:
  val protBits  = 3

  val PROT_PRIVILEDGED = UInt(1, width = protBits)
  val PROT_NONSECURE   = UInt(2, width = protBits)
  val PROT_INSTRUCTION = UInt(4, width = protBits)
  def PROT_DEFAULT = PROT_PRIVILEDGED
}
