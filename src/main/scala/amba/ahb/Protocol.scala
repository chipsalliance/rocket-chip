// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import chisel3._

object AHBParameters
{
  // These are all fixed by the AHB standard:
  val transBits = 2
  val burstBits = 3
  val protBits  = 4
  val sizeBits  = 3  // 8*2^s
  val userBits  = 3
  val hrespBits = 2  // AHB full

  def TRANS_IDLE   = 0.U(transBits.W) // No transfer requested, not in a burst
  def TRANS_BUSY   = 1.U(transBits.W) // No transfer requested, in a burst
  def TRANS_NONSEQ = 2.U(transBits.W) // First (potentially only) request in a burst
  def TRANS_SEQ    = 3.U(transBits.W) // Following requests in a burst

  def BURST_SINGLE = 0.U(burstBits.W) // Single access (no burst)
  def BURST_INCR   = 1.U(burstBits.W) // Incrementing burst of arbitrary length, not crossing 1KB
  def BURST_WRAP4  = 2.U(burstBits.W) // 4-beat wrapping burst
  def BURST_INCR4  = 3.U(burstBits.W) // 4-beat incrementing burst
  def BURST_WRAP8  = 4.U(burstBits.W) // 8-beat wrapping burst
  def BURST_INCR8  = 5.U(burstBits.W) // 8-beat incrementing burst
  def BURST_WRAP16 = 6.U(burstBits.W) // 16-beat wrapping burst
  def BURST_INCR16 = 7.U(burstBits.W) // 16-beat incrementing burst

  val maxTransfer = 16

  def RESP_OKAY  = 0.U(2.W)
  def RESP_ERROR = 1.U(2.W)
  // Only in AHB-Full:
  def RESP_RETRY = 2.U(2.W)
  def RESP_SPLIT = 3.U(2.W)

  def PROT_DATA        = 1.U(protBits.W)
  def PROT_PRIVILEGED = 2.U(protBits.W)
  def PROT_BUFFERABLE  = 4.U(protBits.W)
  def PROT_CACHEABLE   = 8.U(protBits.W)
  def PROT_DEFAULT = PROT_DATA | PROT_PRIVILEGED
}
