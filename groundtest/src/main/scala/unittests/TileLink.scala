package groundtest.unittests


import Chisel._
import junctions._
import uncore.devices._
import uncore.tilelink._
import uncore.converters._
import groundtest.common._
import cde.Parameters

class SmiConverterTest(implicit val p: Parameters) extends UnitTest
    with HasTileLinkParameters {
  val outermostParams = p.alterPartial({ case TLId => "Outermost" })

  val smiWidth = 32
  val smiDepth = 64
  val tlDepth = (smiWidth * smiDepth) / tlDataBits

  val smimem = Module(new SmiMem(smiWidth, smiDepth))
  val conv = Module(new SmiIOTileLinkIOConverter(
    smiWidth, log2Up(smiDepth))(outermostParams))
  val driver = Module(new DriverSet(
    (driverParams: Parameters) => {
      implicit val p = driverParams
      Seq(
        Module(new PutSweepDriver(tlDepth)),
        Module(new PutMaskDriver(smiWidth / 8)),
        Module(new PutBlockSweepDriver(tlDepth / tlDataBeats)))
    })(outermostParams))

  conv.io.tl <> driver.io.mem
  smimem.io <> conv.io.smi
  driver.io.start := io.start
  io.finished := driver.io.finished
}

class ROMSlaveTest(implicit p: Parameters) extends UnitTest {
  implicit val testName = "ROMSlaveTest"
  val romdata = Seq(
    BigInt("01234567deadbeef", 16),
    BigInt("ab32fee8d00dfeed", 16))
  val rombytes = romdata.map(_.toByteArray.reverse).flatten
  val rom = Module(new ROMSlave(rombytes))
  val driver = Module(new DriverSet(
    (driverParams: Parameters) => {
      implicit val p = driverParams
      Seq(
        Module(new GetMultiWidthDriver),
        Module(new GetSweepDriver(romdata)),
        Module(new GetBlockSweepDriver(romdata)))
    }))
  rom.io <> driver.io.mem
  driver.io.start := io.start
  io.finished := driver.io.finished
}

class TileLinkRAMTest(implicit val p: Parameters)
    extends UnitTest with HasTileLinkParameters {

  val depth = 2 * tlDataBeats
  val ram = Module(new TileLinkTestRAM(depth))
  val driver = Module(new DriverSet(
    (driverParams: Parameters) => {
      implicit val p = driverParams
      Seq(
        Module(new PutSweepDriver(depth)),
        Module(new PutMaskDriver),
        Module(new PutBlockSweepDriver(depth / tlDataBeats)))
    }))
  ram.io <> driver.io.mem
  driver.io.start := io.start
  io.finished := driver.io.finished
}
