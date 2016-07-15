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
}

case object UnitTests extends Field[Parameters => Seq[UnitTest]]

class UnitTestSuite(implicit p: Parameters) extends GroundTest()(p) {
  val tests = p(UnitTests)(p)

  val s_idle :: s_start :: s_wait :: Nil = Enum(Bits(), 3)
  val state = Reg(init = s_idle)

  when (state === s_idle) { state := s_start }
  when (state === s_start) { state := s_wait }

  io.status.timeout.valid := Bool(false)
  tests.zipWithIndex.foreach { case (mod, i) =>
    mod.io.start := (state === s_start)
    val timeout = Timer(1000, mod.io.start, mod.io.finished)
    assert(!timeout, s"UnitTest $i timed out")
    when (timeout) {
      io.status.timeout.valid := Bool(true)
      io.status.timeout.bits := UInt(i)
    }
  }
  io.status.finished := tests.map(_.io.finished).reduce(_ && _)
  io.status.error.valid := Bool(false)
}
