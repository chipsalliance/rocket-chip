// See LICENSE for license details.

package rocketchip

import scala.collection.mutable.LinkedHashSet

import Chisel._
import cde.{Parameters, Config, Dump, Knob, CDEMatchError}
import util.{ParameterizedBundle}
import rocket._
import uncore.tilelink._
import uncore.tilelink2.{LazyModule, LazyModuleImp}
import coreplex._
import rocketchip._
import unittest._

class WithUnitTest extends Config(
  (pname, site, here) => pname match {
    case UnitTests => (testParams: Parameters) => {
      val groundtest = if (site(XLen) == 64)
        DefaultTestSuites.groundtest64
      else
        DefaultTestSuites.groundtest32
      TestGeneration.addSuite(groundtest("p"))
      TestGeneration.addSuite(DefaultTestSuites.emptyBmarks)
      JunctionsUnitTests(testParams) ++ UncoreUnitTests(testParams)
    }
    case RegressionTestNames => LinkedHashSet("rv64ui-p-simple")
    case SynTopName => None
    case _ => throw new CDEMatchError
  })

class UnitTestConfig extends Config(new WithUnitTest ++ new BaseConfig)

class UnitTestHarness(implicit val p: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }

  val l1params = p.alterPartial({
    case NCoreplexExtClients => 0
    case ConfigString => ""
    case TLId => "L1toL2" })
  val tests = Module(new UnitTestSuite()(l1params))

  io.success := tests.io.finished
}
