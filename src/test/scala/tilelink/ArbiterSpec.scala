// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import chisel3.iotesters

import freechips.rocketchip.system.DefaultConfig
import freechips.rocketchip.unittest.UnitTestTester

class ArbiterSpec extends AnyFlatSpec with Matchers {

  "TLArbiter with Round Robin policy" should "work" in {
    implicit val p = new DefaultConfig
    iotesters.Driver(() => new TestRobin) {
      c => new UnitTestTester(c)
    } should be (true)
  }
}

