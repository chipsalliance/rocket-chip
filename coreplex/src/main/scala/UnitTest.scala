package coreplex

import Chisel._
import junctions.unittests.UnitTestSuite
import rocket.Tile
import uncore.tilelink.TLId
import cde.Parameters

class UnitTestCoreplex(topParams: Parameters) extends Coreplex()(topParams) {
  require(!exportMMIO)
  require(nBusPorts == 0)
  require(nMemChannels == 0)

  io.debug.req.ready := Bool(false)
  io.debug.resp.valid := Bool(false)

  val l1params = p.alterPartial({ case TLId => "L1toL2" })
  val tests = Module(new UnitTestSuite()(l1params))

  override def hasSuccessFlag = true
  io.success.get := tests.io.finished
}
