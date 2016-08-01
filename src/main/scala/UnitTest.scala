package rocketchip

import Chisel._
import junctions.unittests.UnitTestSuite
import rocket.Tile
import cde.Parameters

class UnitTestTile(clockSignal: Clock = null, resetSignal: Bool = null)
    (implicit p: Parameters) extends Tile(clockSignal, resetSignal)(p) {

  require(io.cached.size == 0)
  require(io.uncached.size == 0)

  val tests = Module(new UnitTestSuite)
  when (tests.io.finished) { stop() }
}
