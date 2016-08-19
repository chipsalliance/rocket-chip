package junctions.unittests

import Chisel._
import junctions._
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

class UnitTestSuite(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val finished = Bool(OUTPUT)
  }

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

  val timer = Module(new Timer(1000, tests.size))
  timer.io.start.valid := Bool(false)
  timer.io.stop.valid := Bool(false)

  tests.zipWithIndex.foreach { case (mod, i) =>
    mod.io.start := (state === s_start) && test_idx === UInt(i)
    when (test_idx === UInt(i)) {
      timer.io.start.valid := mod.io.start
      timer.io.start.bits := UInt(i)
      timer.io.stop.valid := mod.io.finished
      timer.io.stop.bits := UInt(i)
    }
  }
  io.finished := (state === s_done)

  assert(!timer.io.timeout.valid, "UnitTest timed out")
}

object JunctionsUnitTests {
  def apply(implicit p: Parameters): Seq[UnitTest] =
    Seq(
      Module(new MultiWidthFifoTest),
      Module(new AtosConverterTest),
      Module(new NastiMemoryDemuxTest),
      Module(new HastiTest))
}
