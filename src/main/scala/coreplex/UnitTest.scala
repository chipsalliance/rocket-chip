package coreplex

import Chisel._
import unittest.UnitTestSuite
import rocket.Tile
import uncore.tilelink.TLId
import cde.Parameters

class UnitTestCoreplex(tp: Parameters, tc: CoreplexConfig) extends Coreplex()(tp, tc) {
  require(tc.nSlaves == 0)
  require(tc.nMemChannels == 0)

  io.master.mmio.foreach { port =>
    port.acquire.valid := Bool(false)
    port.grant.ready := Bool(false)
  }

  io.debug.req.ready := Bool(false)
  io.debug.resp.valid := Bool(false)

  val l1params = p.alterPartial({ case TLId => "L1toL2" })
  val tests = Module(new UnitTestSuite()(l1params))

  override def hasSuccessFlag = true
  io.success.get := tests.io.finished
}
