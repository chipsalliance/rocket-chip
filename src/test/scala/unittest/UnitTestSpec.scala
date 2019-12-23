// See LICENSE.SiFive for license details.

package freechips.rocketchip.unittest

import chisel3.iotesters.PeekPokeTester

class UnitTestTester[T <: UnitTest](dut: T) extends PeekPokeTester(dut) {
  step(1)
  poke(dut.io.start, 1)
  step(1)
  poke(dut.io.start, 0)

  var done: BigInt = 0
  do {
    step(1)
    done = peek(dut.io.finished)
  } while (done == 0)
}

