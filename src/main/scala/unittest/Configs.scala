// See LICENSE for license details.

package unittest

import scala.collection.mutable.LinkedHashSet

import Chisel._
import cde.{Parameters, Config, CDEMatchError}
import coreplex._
import rocketchip._

class WithUnitTest extends Config(
  (pname, site, here) => pname match {
    case UnitTests => (testParams: Parameters) => {
      TestGeneration.addSuite(DefaultTestSuites.groundtest64("p")) // TODO why
      TestGeneration.addSuite(DefaultTestSuites.emptyBmarks)
      JunctionsUnitTests(testParams) ++ UncoreUnitTests(testParams) // TODO refactor
    }
    case RegressionTestNames => LinkedHashSet("rv64ui-p-simple")
    case _ => throw new CDEMatchError
  })

class UnitTestConfig extends Config(new WithUnitTest ++ new BaseConfig)
