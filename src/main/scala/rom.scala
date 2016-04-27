package uncore

import Chisel._
import junctions._
import cde.{Parameters, Field}

class ROMSlave(contents: Seq[Byte])(implicit val p: Parameters) extends Module
    with HasTileLinkParameters
    with HasAddrMapParameters {
  val io = new ClientUncachedTileLinkIO().flip

  val acq = Queue(io.acquire, 1)
  assert(!acq.valid || acq.bits.a_type === Acquire.getType, "unsupported ROMSlave operation")

  val byteWidth = tlDataBits / 8
  val rows = (contents.size + byteWidth - 1)/byteWidth + 1
  val rom = Vec.tabulate(rows) { i =>
    val slice = contents.slice(i*byteWidth, (i+1)*byteWidth)
    UInt(slice.foldRight(BigInt(0)) { case (x,y) => (y << 8) + (x.toInt & 0xFF) }, byteWidth*8)
  }
  val rdata = rom(if (rows == 1) UInt(0) else acq.bits.full_addr()(log2Up(contents.size)-1,log2Up(byteWidth)))

  io.grant.valid := acq.valid
  acq.ready := io.grant.ready
  io.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = acq.bits.getBuiltInGrantType(),
    client_xact_id = acq.bits.client_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = UInt(0),
    data = rdata)
}
