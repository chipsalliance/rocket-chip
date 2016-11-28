// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package uncore.devices

import Chisel._
import config._
import unittest.UnitTest
import junctions._
import uncore.tilelink._
import uncore.util._
import util._
import HastiConstants._

class BRAMSlave(depth: Int)(implicit val p: Parameters) extends Module
  with HasTileLinkParameters {
  val io = new ClientUncachedTileLinkIO().flip

  // For TL2:
  // supportsAcquire = false
  // supportsMultibeat = false
  // supportsHint = false
  // supportsAtomic = false

  // Timing-wise, we assume the input is coming out of registers
  // since you probably needed a TileLinkFragmenter infront of us

  // Thus, only one pipeline stage: the grant result
  val g_valid = RegInit(Bool(false))
  val g_bits = Reg(new Grant)

  // Just pass the pipeline straight through
  io.grant.valid := g_valid
  io.grant.bits := g_bits
  io.acquire.ready := !g_valid || io.grant.ready

  val acq_get  = io.acquire.bits.isBuiltInType(Acquire.getType)
  val acq_put  = io.acquire.bits.isBuiltInType(Acquire.putType)
  val acq_addr = Cat(io.acquire.bits.addr_block, io.acquire.bits.addr_beat)

  val bram = Mem(depth, Bits(width = tlDataBits))

  val ren = acq_get && io.acquire.fire()
  val wen = acq_put && io.acquire.fire()

  when (io.grant.fire()) {
    g_valid := Bool(false)
  }

  when (io.acquire.fire()) {
    g_valid := Bool(true)
    g_bits := Grant(
      is_builtin_type = Bool(true),
      g_type = io.acquire.bits.getBuiltInGrantType(),
      client_xact_id = io.acquire.bits.client_xact_id,
      manager_xact_id = UInt(0),
      addr_beat = io.acquire.bits.addr_beat,
      data = UInt(0))
  }

  when (wen) {
    bram.write(acq_addr, io.acquire.bits.data)
    assert(io.acquire.bits.wmask().andR, "BRAMSlave: partial write masks not supported")
  }
  io.grant.bits.data := RegEnable(bram.read(acq_addr), ren)
}

class HastiRAM(depth: Int)(implicit p: Parameters) extends HastiModule()(p) {
  val io = new HastiSlaveIO

  val wdata = Vec.tabulate(hastiDataBytes)(i => io.hwdata(8*(i+1)-1,8*i))
  val waddr = Reg(UInt(width = hastiAddrBits))
  val wvalid = Reg(init = Bool(false))
  val wsize = Reg(UInt(width = SZ_HSIZE))
  val ram = SeqMem(depth, Vec(hastiDataBytes, Bits(width = 8)))

  val max_size = log2Ceil(hastiDataBytes)
  val wmask_lut = MuxLookup(wsize, SInt(-1, hastiDataBytes).asUInt,
    (0 until max_size).map(sz => (UInt(sz) -> UInt((1 << (1 << sz)) - 1))))
  val wmask = (wmask_lut << waddr(max_size - 1, 0))(hastiDataBytes - 1, 0)

  val is_trans = io.hsel && io.htrans.isOneOf(HTRANS_NONSEQ, HTRANS_SEQ)
  val raddr = io.haddr >> UInt(max_size)
  val ren = is_trans && !io.hwrite
  val bypass = Reg(init = Bool(false))

  when (is_trans && io.hwrite) {
    waddr := io.haddr
    wsize := io.hsize
    wvalid := Bool(true)
  } .otherwise { wvalid := Bool(false) }

  when (ren) { bypass := wvalid && (waddr >> UInt(max_size)) === raddr }

  when (wvalid) {
    ram.write(waddr >> UInt(max_size), wdata, wmask.toBools)
  }

  val rdata = ram.read(raddr, ren)
  io.hrdata := Cat(rdata.zip(wmask.toBools).zip(wdata).map {
    case ((rbyte, wsel), wbyte) => Mux(wsel && bypass, wbyte, rbyte)
  }.reverse)

  io.hready := Bool(true)
  io.hresp := HRESP_OKAY
}

/**
 * This RAM is not meant to be particularly performant.
 * It just supports the entire range of uncached TileLink operations in the
 * simplest way possible.
 */
class TileLinkTestRAM(depth: Int)(implicit val p: Parameters) extends Module
    with HasTileLinkParameters {
  val io = new ClientUncachedTileLinkIO().flip

  val ram = Mem(depth, UInt(width = tlDataBits))

  val responding = Reg(init = Bool(false))
  val acq = io.acquire.bits
  val r_acq = Reg(io.acquire.bits)
  val acq_addr = Cat(acq.addr_block, acq.addr_beat)
  val r_acq_addr = Cat(r_acq.addr_block, r_acq.addr_beat)

  when (io.acquire.fire() && io.acquire.bits.last()) {
    r_acq := io.acquire.bits
    responding := Bool(true)
  }

  when (io.grant.fire()) {
    val is_getblk = r_acq.isBuiltInType(Acquire.getBlockType)
    val last_beat = r_acq.addr_beat === UInt(tlDataBeats - 1)
    when (is_getblk && !last_beat) {
      r_acq.addr_beat := r_acq.addr_beat + UInt(1)
    } .otherwise { responding := Bool(false) }
  }

  val old_data = ram(acq_addr)
  val new_data = acq.data
  val r_old_data = RegEnable(old_data, io.acquire.fire())

  io.acquire.ready := !responding
  io.grant.valid := responding
  io.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = r_acq.getBuiltInGrantType(),
    client_xact_id = r_acq.client_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = r_acq.addr_beat,
    data = Mux(r_acq.isAtomic(), r_old_data, ram(r_acq_addr)))

  val amo_shift_bits = acq.amo_shift_bytes() << UInt(3)
  val amoalu = Module(new AMOALU(amoAluOperandBits, rhsIsAligned = true))
  amoalu.io.addr := Cat(acq.addr_block, acq.addr_beat, acq.addr_byte())
  amoalu.io.cmd := acq.op_code()
  amoalu.io.typ := acq.op_size()
  amoalu.io.lhs := old_data >> amo_shift_bits
  amoalu.io.rhs := new_data >> amo_shift_bits

  val result = Mux(acq.isAtomic(), amoalu.io.out << amo_shift_bits, new_data)
  val wmask = FillInterleaved(8, acq.wmask())

  when (io.acquire.fire() && acq.hasData()) {
    ram(acq_addr) := (old_data & ~wmask) | (result & wmask)
  }
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
