package uncore.unittests

import Chisel._
import junctions._
import junctions.unittests._
import uncore.devices._
import uncore.tilelink._
import uncore.converters._
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
        Module(new PutBlockSweepDriver(tlDepth / tlDataBeats)),
        Module(new GetMultiWidthDriver))
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
        Module(new PutAtomicDriver),
        Module(new PutBlockSweepDriver(depth / tlDataBeats)),
        Module(new PrefetchDriver),
        Module(new GetMultiWidthDriver))
    }))
  ram.io <> driver.io.mem
  driver.io.start := io.start
  io.finished := driver.io.finished
}

class TileLinkSwitcherTest(implicit val p: Parameters)
    extends UnitTest with HasTileLinkParameters {

  def addrToRoute(addr: UInt): UInt = {
    val lsb = tlByteAddrBits + tlBeatAddrBits
    UIntToOH(addr(lsb, lsb))
  }

  val driver = Module(new DriverSet(
    (driverParams: Parameters) => {
      implicit val p = driverParams
      Seq(
        Module(new PutSweepDriver(depth)),
        Module(new PutMaskDriver),
        Module(new PutAtomicDriver),
        Module(new PutBlockSweepDriver(depth / tlDataBeats)),
        Module(new PrefetchDriver),
        Module(new GetMultiWidthDriver))
    }))
  driver.io.start := io.start
  io.finished := driver.io.finished

  val depth = 2 * tlDataBeats
  val testrams = Seq.fill(2) { Module(new TileLinkTestRAM(depth)) }
  val interconnect = Module(new TileLinkMemoryInterconnect(1, 2))
  val switcher = Module(new ClientUncachedTileLinkIOSwitcher(2, 2))
  val router = Module(new ClientUncachedTileLinkIORouter(2, addrToRoute _))
  router.io.in <> driver.io.mem
  switcher.io.in <> router.io.out
  interconnect.io.in <> switcher.io.out
  for ((ram, i) <- testrams.zipWithIndex) {
    ram.io <> interconnect.io.out(i)
  }
  // swapsies
  switcher.io.select(0) := UInt(1)
  switcher.io.select(1) := UInt(0)
}

class TileLinkSerdesTest(implicit val p: Parameters)
    extends UnitTest with HasTileLinkParameters {
  val serdesWidth = 8

  val driver = Module(new DriverSet(
    (driverParams: Parameters) => {
      implicit val p = driverParams
      Seq(
        Module(new PutSweepDriver(depth)),
        Module(new PutMaskDriver),
        Module(new PutAtomicDriver),
        Module(new PutBlockSweepDriver(depth / tlDataBeats)),
        Module(new PrefetchDriver),
        Module(new GetMultiWidthDriver))
    }))
  driver.io.start := io.start
  io.finished := driver.io.finished

  val depth = 2 * tlDataBeats
  val testram = Module(new TileLinkTestRAM(depth))

  val serdes = Module(new ClientTileLinkIOSerdes(serdesWidth))
  val desser = Module(new ClientTileLinkIODesser(serdesWidth))
  serdes.io.tl <> TileLinkIOWrapper(driver.io.mem)
  desser.io.serial.in <> serdes.io.serial.out
  serdes.io.serial.in <> desser.io.serial.out
  testram.io <> TileLinkIOUnwrapper(desser.io.tl)
}

object UncoreUnitTests {
  def apply(implicit p: Parameters): Seq[UnitTest] =
    Seq(
      Module(new SmiConverterTest),
      Module(new ROMSlaveTest),
      Module(new TileLinkRAMTest),
      Module(new TileLinkSwitcherTest),
      Module(new TileLinkSerdesTest))
}
