package uncore

import Chisel._
import junctions._
import cde.{Parameters, Field}

class ROMSlave(contents: Seq[Byte])(implicit val p: Parameters) extends Module
    with HasTileLinkParameters
    with HasAddrMapParameters {
  val io = new ClientUncachedTileLinkIO().flip

  val acq = Queue(io.acquire, 1)
  val single_beat = acq.bits.isBuiltInType(Acquire.getType)
  val multi_beat = acq.bits.isBuiltInType(Acquire.getBlockType)
  assert(!acq.valid || single_beat || multi_beat, "unsupported ROMSlave operation")

  val addr_beat = Reg(UInt())
  when (io.grant.fire()) { addr_beat := addr_beat + UInt(1) }
  when (io.acquire.fire()) { addr_beat := io.acquire.bits.addr_beat }

  val byteWidth = tlDataBits / 8
  val rows = (contents.size + byteWidth - 1)/byteWidth + 1
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
