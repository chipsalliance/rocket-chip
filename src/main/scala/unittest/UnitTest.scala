// See LICENSE.SiFive for license details.

package freechips.rocketchip.unittest

import Chisel._
import chisel3.experimental.MultiIOModule
import freechips.rocketchip.config._
import freechips.rocketchip.util._

trait UnitTestIO {
  val finished = Bool(OUTPUT)
  val start = Bool(INPUT)
}

trait HasUnitTestIO {
  val io: UnitTestIO
}

trait UnitTestLegacyModule extends HasUnitTestIO {
  val io = new Bundle with UnitTestIO
}

trait UnitTestModule extends MultiIOModule with HasUnitTestIO {
  val io = IO(new Bundle with UnitTestIO)
  ElaborationArtefacts.add("plusArgs", PlusArgArtefacts.serialize_cHeader)
}

abstract class UnitTest(val timeout: Int = 4096) extends Module with UnitTestLegacyModule {
  val testName = this.getClass.getSimpleName

  when (io.start) { printf(s"Started UnitTest $testName\n") }

  val timed_out = SimpleTimer(timeout, io.start, io.finished)
  assert(!timed_out, s"UnitTest $testName timed out")
}

case object UnitTests extends Field[Parameters => Seq[UnitTest]]

class UnitTestSuite(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val finished = Bool(OUTPUT)
  }

  val tests = p(UnitTests)(p)

  val s_idle :: s_start :: s_busy :: s_done :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)
  val tests_finished = Vec(tests.map(_.io.finished)).reduce(_&&_)

  tests.foreach { _.io.start := (state === s_start) }
  io.finished := (state === s_done)

  when (state === s_idle) { state := s_start }
  when (state === s_start) { state := s_busy }
  when (state === s_busy && tests_finished) { state := s_done }
}
