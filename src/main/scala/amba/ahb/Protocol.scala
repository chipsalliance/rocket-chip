// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import Chisel._

object AHBParameters
{
  // These are all fixed by the AHB standard:
  val transBits = 2
  val burstBits = 3
  val protBits  = 4
  val sizeBits  = 3  // 8*2^s

  def TRANS_IDLE   = UInt(0, width = transBits) // No transfer requested, not in a burst
  def TRANS_BUSY   = UInt(1, width = transBits) // No transfer requested, in a burst
  def TRANS_NONSEQ = UInt(2, width = transBits) // First (potentially only) request in a burst
  def TRANS_SEQ    = UInt(3, width = transBits) // Following requests in a burst

  def BURST_SINGLE = UInt(0, width = burstBits) // Single access (no burst)
  def BURST_INCR   = UInt(1, width = burstBits) // Incrementing burst of arbitrary length, not crossing 1KB
  def BURST_WRAP4  = UInt(2, width = burstBits) // 4-beat wrapping burst
  def BURST_INCR4  = UInt(3, width = burstBits) // 4-beat incrementing burst
  def BURST_WRAP8  = UInt(4, width = burstBits) // 8-beat wrapping burst
  def BURST_INCR8  = UInt(5, width = burstBits) // 8-beat incrementing burst
  def BURST_WRAP16 = UInt(6, width = burstBits) // 16-beat wrapping burst
  def BURST_INCR16 = UInt(7, width = burstBits) // 16-beat incrementing burst

  val maxTransfer = 16

  def RESP_OKAY  = Bool(false)
  def RESP_ERROR = Bool(true)

  def PROT_DATA        = UInt(1, width = protBits)
  def PROT_PRIVILEDGED = UInt(2, width = protBits)
  def PROT_BUFFERABLE  = UInt(4, width = protBits)
  def PROT_CACHEABLE   = UInt(8, width = protBits)
  def PROT_DEFAULT = PROT_DATA | PROT_PRIVILEDGED
}
