package groundtest.unittests

import Chisel._
import junctions._
import junctions.NastiConstants._
import uncore.tilelink._
import uncore.converters._
import uncore.constants._
import uncore.devices._
import groundtest.common._
import cde.{Field, Parameters}

abstract class UnitTest extends Module {
  val io = new Bundle {
    val finished = Bool(OUTPUT)
    val start = Bool(INPUT)
  }

  when (io.start) {
    printf(s"Started UnitTest ${this.getClass.getSimpleName}\n")
  }
}

case object UnitTests extends Field[Parameters => Seq[UnitTest]]

class UnitTestSuite(implicit p: Parameters) extends GroundTest()(p) {
  val tests = p(UnitTests)(p)

  val s_idle :: s_start :: s_wait :: s_done :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)
  val test_idx = Reg(init = UInt(0, log2Up(tests.size)))
  val test_finished = Vec(tests.map(_.io.finished))

  when (state === s_idle) { state := s_start }
  when (state === s_start) { state := s_wait }
  when (state === s_wait && test_finished(test_idx)) {
    state := s_start
    test_idx := test_idx + UInt(1)
    state := Mux(test_idx === UInt(tests.size - 1), s_done, s_start)
  }

  io.status.timeout.valid := Bool(false)
  tests.zipWithIndex.foreach { case (mod, i) =>
    mod.io.start := (state === s_start) && test_idx === UInt(i)
    val timeout = Timer(1000, mod.io.start, mod.io.finished)
    assert(!timeout, s"UnitTest ${mod.getClass.getSimpleName} timed out")
    when (timeout) {
      io.status.timeout.valid := Bool(true)
      io.status.timeout.bits := UInt(i)
    }
  }
  io.status.finished := (state === s_done)
  io.status.error.valid := Bool(false)

}
