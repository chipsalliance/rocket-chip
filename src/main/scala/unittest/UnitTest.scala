// See LICENSE.SiFive for license details.

package freechips.rocketchip.unittest

import chisel3._
import chisel3.util._
import chisel3.experimental.{IO}
import freechips.rocketchip.config._
import freechips.rocketchip.util._

trait UnitTestIO {
  val finished = Output(Bool())
  val start = Input(Bool())
}

trait HasUnitTestIO {
  val io: UnitTestIO
}

trait UnitTestLegacyModule extends HasUnitTestIO {
  val io = IO(new Bundle with UnitTestIO)
}

trait UnitTestModule extends Module with HasUnitTestIO {
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
  val io = IO(new Bundle {
    val finished = Output(Bool())
  })

  val tests = p(UnitTests)(p)

  val s_idle :: s_start :: s_busy :: s_done :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val tests_finished = VecInit(tests.map(_.io.finished)).reduce(_&&_)

  tests.foreach { _.io.start := (state === s_start) }
  io.finished := (state === s_done)

  when (state === s_idle) { state := s_start }
  when (state === s_start) { state := s_busy }
  when (state === s_busy && tests_finished) { state := s_done }
}
