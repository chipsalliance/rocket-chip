// See LICENSE.Berkeley for license details.

package freechips.rocketchip.unittest.hwtests

import chisel3._
import chisel3.util.PopCount

import org.chipsalliance.rocketutils.SECDEDCode

import freechips.rocketchip.unittest.UnitTest

class ECCTest(k: Int, timeout: Int = 500000) extends UnitTest(timeout) {
  val code = new SECDEDCode
  val n = code.width(k)

  // Brute force the decode space
  val test = RegInit(0.U((n+1).W))
  val last = test(n)
  test := test + !last
  io.finished := RegNext(last, false.B)

  // Confirm the decoding matches the encoding
  val decoded = code.decode(test(n-1, 0))
  val recoded = code.encode(decoded.corrected)
  val distance = PopCount(recoded ^ test)

  // Count the cases
  val correct = RegInit(0.U(n.W))
  val correctable = RegInit(0.U(n.W))
  val uncorrectable = RegInit(0.U(n.W))

  when (!last) {
    when (decoded.uncorrectable) {
      assert (distance >= 2.U) // uncorrectable
      uncorrectable := uncorrectable + 1.U
    } .elsewhen (decoded.correctable) {
      assert (distance(0)) // correctable => odd bit errors
      correctable := correctable + 1.U
    } .otherwise {
      assert (distance === 0.U) // correct
      assert (decoded.uncorrected === decoded.corrected)
      correct := correct + 1.U
    }
  }

  // Expected number of each case
  val nCodes = BigInt(1) << n
  val nCorrect = BigInt(1) << k
  val nCorrectable = nCodes / 2
  val nUncorrectable = nCodes - nCorrectable - nCorrect

  when (last) {
    assert (correct === nCorrect.U)
    assert (correctable === nCorrectable.U)
    assert (uncorrectable === nUncorrectable.U)
  }
}
