package groundtest.unittests

import Chisel._
import junctions._
import cde.Parameters

class HastiTest(implicit p: Parameters) extends UnitTest {
  val sram = Module(new HastiTestSRAM(8))
  val bus = Module(new HastiBus(Seq(a => Bool(true))))
  val conv = Module(new HastiMasterIONastiIOConverter)
  val driver = Module(new MemoryTestDriver("HastiTest", 32, 8, 2))

  bus.io.slaves(0) <> sram.io
  bus.io.master <> conv.io.hasti
  conv.io.nasti <> driver.io.nasti
  io.finished := driver.io.finished
  driver.io.start := io.start
}


