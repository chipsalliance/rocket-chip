// See LICENSE for license details.

package unittest

import scala.collection.mutable.LinkedHashSet

import Chisel._
import cde.{Parameters, Config, CDEMatchError}
import coreplex._
import rocketchip._

class WithJunctionsUnitTests extends Config(
  (pname, site, here) => pname match {
    case RegressionTestNames => LinkedHashSet("rv64ui-p-simple")
    case UnitTests => (p: Parameters) => {
      TestGeneration.addSuite(DefaultTestSuites.groundtest64("p")) // TODO why
      TestGeneration.addSuite(DefaultTestSuites.emptyBmarks)
      Seq(
        Module(new junctions.MultiWidthFifoTest),
        Module(new junctions.NastiMemoryDemuxTest()(p)),
        Module(new junctions.HastiTest()(p)))
    }
    case _ => throw new CDEMatchError
  })

class WithUncoreUnitTests extends Config(
  (pname, site, here) => pname match {
    case NCoreplexExtClients => 0
    case uncore.tilelink.TLId => "L1toL2"
    case RegressionTestNames => LinkedHashSet("rv64ui-p-simple")
    case UnitTests => (p: Parameters) => {
      TestGeneration.addSuite(DefaultTestSuites.groundtest64("p")) // TODO why
      TestGeneration.addSuite(DefaultTestSuites.emptyBmarks)
      Seq(
        Module(new uncore.devices.ROMSlaveTest()(p)),
        Module(new uncore.devices.TileLinkRAMTest()(p)),
        Module(new uncore.tilelink2.TLFuzzRAMTest))
    }
    case _ => throw new CDEMatchError
  })

class UnitTestConfig extends Config(new WithUncoreUnitTests ++ new WithJunctionsUnitTests ++ new BaseConfig)
