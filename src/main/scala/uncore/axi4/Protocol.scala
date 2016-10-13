// See LICENSE for license details.

package uncore.axi4

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

  val CACHE_RALLOCATE  = UInt(8, width = cacheBits)
  val CACHE_WALLOCATE  = UInt(4, width = cacheBits)
  val CACHE_MODIFIABLE = UInt(2, width = cacheBits)
  val CACHE_BUFFERABLE = UInt(1, width = cacheBits)

  val PROT_PRIVILEDGED = UInt(1, width = protBits)
  val PROT_INSECURE    = UInt(2, width = protBits)
  val PROT_INSTRUCTION = UInt(4, width = protBits)

  val BURST_FIXED = UInt(0, width = burstBits)
  val BURST_INCR  = UInt(1, width = burstBits)
  val BURST_WRAP  = UInt(2, width = burstBits)

  val RESP_OKAY   = UInt(0, width = respBits)
  val RESP_EXOKAY = UInt(1, width = respBits)
  val RESP_SLVERR = UInt(2, width = respBits)
  val RESP_DECERR = UInt(3, width = respBits)
}
