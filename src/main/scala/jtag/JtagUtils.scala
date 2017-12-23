// See LICENSE.jtag for license details.

package freechips.rocketchip.jtag

import Chisel._
//import chisel3._
import chisel3.util._

class JTAGIdcodeBundle extends Bundle {
  val version = UInt(4.W)
  val partNumber = UInt(16.W)
  val mfrId = UInt(11.W)
  val always1 = UInt(1.W)
}

object JtagIdcode {
  /** Generates a JTAG IDCODE as a 32-bit integer, using the format in 12.1.1d.
    */
  def apply(version: Int, partNumber: Int, mfrId: Int): BigInt = {
    require(version < (1 << 4), "version field must be 4 bits at most")
    require(partNumber < (1 << 16), "part number must be 16 bits at most")
    require(mfrId < (1 << 11), "manufacturer identity must be 11 bits at most")
    BigInt(version) << 28 | BigInt(partNumber) << 12 | BigInt(mfrId) << 1 | 1
  }

  /** A dummy manufacturer ID, not to be used per 12.2.1b since bus masters may shift this out to
    * determine the end of a bus.
    */
  def dummyMfrId: Int = 0x7f
}
