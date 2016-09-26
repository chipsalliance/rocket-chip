// See LICENSE for license details.

package unittest

import scala.collection.mutable.LinkedHashSet

import Chisel._
import cde.{Parameters, Config, CDEMatchError}
import coreplex._
import rocketchip._

class WithUnitTests extends Config(
  (pname, site, here) => pname match {
    case uncore.tilelink.TLId => "L1toL2"
    case NCoreplexExtClients => 0
    case RegressionTestNames => LinkedHashSet("rv64ui-p-simple")
    case UnitTests => (p: Parameters) => {
      TestGeneration.addSuite(DefaultTestSuites.groundtest64("p")) // TODO why
      TestGeneration.addSuite(DefaultTestSuites.emptyBmarks)
      Seq(
        Module(new junctions.MultiWidthFifoTest),
        Module(new junctions.NastiMemoryDemuxTest()(p)),
        Module(new junctions.HastiTest()(p)),
        Module(new uncore.devices.ROMSlaveTest()(p)),
        Module(new uncore.devices.TileLinkRAMTest()(p)),
        Module(new uncore.tilelink2.TLFuzzRAMTest))
    }
    case _ => throw new CDEMatchError
  })

class UnitTestConfig extends Config(new WithUnitTests ++ new BaseConfig)
