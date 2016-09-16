package unittest

import Chisel._
import cde.Parameters

object JunctionsUnitTests {
  def apply(implicit p: Parameters): Seq[UnitTest] =
    Seq(
      Module(new junctions.MultiWidthFifoTest),
      Module(new junctions.NastiMemoryDemuxTest),
      Module(new junctions.HastiTest))
}

object UncoreUnitTests {
  def apply(implicit p: Parameters): Seq[UnitTest] =
    Seq(
      Module(new uncore.devices.ROMSlaveTest),
      Module(new uncore.devices.TileLinkRAMTest),
      Module(new uncore.tilelink2.TLFuzzRAMTest))
}
