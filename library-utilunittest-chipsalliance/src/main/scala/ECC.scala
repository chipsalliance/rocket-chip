// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import Chisel._

/** Synthesizeable unit tests */
import freechips.rocketchip.unittest._

class ECCTest(k: Int, timeout: Int = 500000) extends UnitTest(timeout) {
  val code = new SECDEDCode
  val n = code.width(k)

  // Brute force the decode space
  val test = RegInit(UInt(0, width=n+1))
  val last = test(n)
  test := test + !last
  io.finished := RegNext(last, Bool(false))

  // Confirm the decoding matches the encoding
  val decoded = code.decode(test(n-1, 0))
  val recoded = code.encode(decoded.corrected)
  val distance = PopCount(recoded ^ test)

  // Count the cases
  val correct = RegInit(UInt(0, width=n))
  val correctable = RegInit(UInt(0, width=n))
  val uncorrectable = RegInit(UInt(0, width=n))

  when (!last) {
    when (decoded.uncorrectable) {
      assert (distance >= UInt(2)) // uncorrectable
      uncorrectable := uncorrectable + UInt(1)
    } .elsewhen (decoded.correctable) {
      assert (distance(0)) // correctable => odd bit errors
      correctable := correctable + UInt(1)
    } .otherwise {
      assert (distance === UInt(0)) // correct
      assert (decoded.uncorrected === decoded.corrected)
      correct := correct + UInt(1)
    }
  }

  // Expected number of each case
  val nCodes = BigInt(1) << n
  val nCorrect = BigInt(1) << k
  val nCorrectable = nCodes / 2
  val nUncorrectable = nCodes - nCorrectable - nCorrect

  when (last) {
    assert (correct === UInt(nCorrect))
    assert (correctable === UInt(nCorrectable))
    assert (uncorrectable === UInt(nUncorrectable))
  }
}
