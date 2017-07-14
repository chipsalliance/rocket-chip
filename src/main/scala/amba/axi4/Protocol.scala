// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import chisel3.util.{Irrevocable, IrrevocableIO}

object AXI4Parameters
{
  // These are all fixed by the AXI4 standard:
  val lenBits   = 8
  val sizeBits  = 3
  val burstBits = 2
  val lockBits  = 1
  val cacheBits = 4
  val protBits  = 3
  val qosBits   = 4
  val respBits  = 2

  def CACHE_RALLOCATE  = UInt(8, width = cacheBits)
  def CACHE_WALLOCATE  = UInt(4, width = cacheBits)
  def CACHE_MODIFIABLE = UInt(2, width = cacheBits)
  def CACHE_BUFFERABLE = UInt(1, width = cacheBits)

  def PROT_PRIVILEDGED = UInt(1, width = protBits)
  def PROT_INSECURE    = UInt(2, width = protBits)
  def PROT_INSTRUCTION = UInt(4, width = protBits)

  def BURST_FIXED = UInt(0, width = burstBits)
  def BURST_INCR  = UInt(1, width = burstBits)
  def BURST_WRAP  = UInt(2, width = burstBits)

  def RESP_OKAY   = UInt(0, width = respBits)
  def RESP_EXOKAY = UInt(1, width = respBits)
  def RESP_SLVERR = UInt(2, width = respBits)
  def RESP_DECERR = UInt(3, width = respBits)
}
