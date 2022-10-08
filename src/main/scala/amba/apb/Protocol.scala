// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import chisel3._

object APBParameters
{
  // These are all fixed by the AHB standard:
  val protBits  = 3

  def PROT_PRIVILEGED = 1.U(protBits.W)
  def PROT_NONSECURE   = 2.U(protBits.W)
  def PROT_INSTRUCTION = 4.U(protBits.W)
  def PROT_DEFAULT = PROT_PRIVILEGED
}
