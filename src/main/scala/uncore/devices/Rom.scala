// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package uncore.devices

import Chisel._
import unittest.UnitTest
import junctions._
import diplomacy._
import uncore.tilelink._
import uncore.tilelink2._
import uncore.util._
import config._

class TLROM(val base: BigInt, val size: Int, contentsDelayed: => Seq[Byte], executable: Boolean = true, beatBytes: Int = 4)(implicit p: Parameters) extends LazyModule
{
  val device = new SimpleDevice("rom", Nil)

  val node = TLManagerNode(beatBytes, TLManagerParameters(
    address     = List(AddressSet(base, size-1)),
    resources   = device.reg,
    regionType  = RegionType.UNCACHED,
    executable  = executable,
    supportsGet = TransferSizes(1, beatBytes),
    fifoId      = Some(0)))

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
    }

    val contents = contentsDelayed
    require (contents.size <= size)

    val in = io.in(0)
    val edge = node.edgesIn(0)

    val words = (contents ++ Seq.fill(size-contents.size)(0.toByte)).grouped(beatBytes).toSeq
    val bigs = words.map(_.foldRight(BigInt(0)){ case (x,y) => (x.toInt & 0xff) | y << 8})
    val rom = Vec(bigs.map(x => UInt(x, width = 8*beatBytes)))

    in.d.valid := in.a.valid
    in.a.ready := in.d.ready

    val index = in.a.bits.address(log2Ceil(size)-1,log2Ceil(beatBytes))
    in.d.bits := edge.AccessAck(in.a.bits, UInt(0), rom(index))

    // Tie off unused channels
    in.b.valid := Bool(false)
    in.c.ready := Bool(true)
    in.e.ready := Bool(true)
  }
}

class ROMSlave(contents: Seq[Byte])(implicit val p: Parameters) extends Module
    with HasTileLinkParameters {
  val io = new ClientUncachedTileLinkIO().flip

  val acq = Queue(io.acquire, 1)
  val single_beat = acq.bits.isBuiltInType(Acquire.getType)
  val multi_beat = acq.bits.isBuiltInType(Acquire.getBlockType)
  assert(!acq.valid || single_beat || multi_beat, "unsupported ROMSlave operation")

  val addr_beat = Reg(UInt())
  when (io.grant.fire()) { addr_beat := addr_beat + UInt(1) }
  when (io.acquire.fire()) { addr_beat := io.acquire.bits.addr_beat }

  val byteWidth = tlDataBits / 8
  val rows = (contents.size + byteWidth - 1)/byteWidth
  val rom = Vec.tabulate(rows) { i =>
    val slice = contents.slice(i*byteWidth, (i+1)*byteWidth)
    UInt(slice.foldRight(BigInt(0)) { case (x,y) => (y << 8) + (x.toInt & 0xFF) }, byteWidth*8)
  }
  val raddr = Cat(acq.bits.addr_block, addr_beat)
  val rdata = rom(if (rows == 1) UInt(0) else raddr(log2Up(rom.size)-1,0))

  val last = !multi_beat || addr_beat === UInt(tlDataBeats-1)
  io.grant.valid := acq.valid
  acq.ready := io.grant.ready && last
  io.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = acq.bits.getBuiltInGrantType(),
    client_xact_id = acq.bits.client_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = addr_beat,
    data = rdata)
}

class ROMSlaveTest(implicit p: Parameters) extends UnitTest {
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

class NastiROM(contents: Seq[Byte])(implicit p: Parameters) extends Module {
  val io = new NastiIO().flip
  val ar = Queue(io.ar, 1)

  // This assumes ROMs are in read-only parts of the address map.
  // Reuse b_queue code from NastiErrorSlave if this assumption is bad.
  when (ar.valid) { assert(ar.bits.len === UInt(0), "Can't burst-read from NastiROM") }
  assert(!(io.aw.valid || io.w.valid), "Can't write to NastiROM")
  io.aw.ready := Bool(false)
  io.w.ready := Bool(false)
  io.b.valid := Bool(false)

  val byteWidth = io.r.bits.nastiXDataBits / 8
  val rows = (contents.size + byteWidth - 1)/byteWidth
  val rom = Vec.tabulate(rows) { i =>
    val slice = contents.slice(i*byteWidth, (i+1)*byteWidth)
    UInt(slice.foldRight(BigInt(0)) { case (x,y) => (y << 8) + (x.toInt & 0xFF) }, byteWidth*8)
  }
  val rdata = rom(if (rows == 1) UInt(0) else ar.bits.addr(log2Up(contents.size)-1,log2Up(byteWidth)))

  io.r <> ar
  io.r.bits := NastiReadDataChannel(ar.bits.id, rdata)
}
