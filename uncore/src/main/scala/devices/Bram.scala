package uncore.devices

import Chisel._
import cde.{Parameters, Field}
import junctions._
import uncore.tilelink._
import uncore.util._
import HastiConstants._

class BRAMSlave(depth: Int)(implicit val p: Parameters) extends Module
  with HasTileLinkParameters {
  val io = new ClientUncachedTileLinkIO().flip

  val bram = SeqMem(depth, Bits(width = tlDataBits))

  val fire_acq = io.acquire.fire()
  val fire_gnt = io.grant.fire()

  val state_getblk = Reg(init = Bool(false))
  val state_putblk = Reg(init = Bool(false))
  val state_init = !(state_getblk || state_putblk)

  private def last(acq: AcquireMetadata) =
    (acq.addr_beat === UInt(tlDataBeats-1))

  val s0_acq = io.acquire.bits
  val s0_last = last(s0_acq)

  val s1_acq = RegEnable(s0_acq, fire_acq)
  val s1_last = last(s1_acq)

  val (is_get :: is_getblk :: is_put :: is_putblk :: Nil) =
    Seq(Acquire.getType, Acquire.getBlockType,
      Acquire.putType, Acquire.putBlockType).map(s0_acq.isBuiltInType _)

  val is_read = is_get || is_getblk
  val is_write = is_put || is_putblk
  val ren_getblk = state_getblk && !s1_last

  val s0_valid = (fire_acq && (!is_putblk || s0_last)) || ren_getblk
  val s1_valid = RegNext(s0_valid, Bool(false))

  val ren = (fire_acq && is_read) || ren_getblk
  val wen = (fire_acq && is_write)

  val s0_addr = Cat(s0_acq.addr_block, s0_acq.addr_beat)
  val s1_addr_beat = s1_acq.addr_beat + Mux(io.grant.ready, UInt(1), UInt(0))
  val s1_addr = Cat(s1_acq.addr_block, s1_addr_beat)

  val raddr = Mux(state_getblk, s1_addr, s0_addr)
  val waddr = s0_addr

  val rdata = bram.read(raddr, ren)
  val wdata = s0_acq.data
  val wmask = s0_acq.wmask()
  when (wen) {
    bram.write(waddr, wdata)
    assert(wmask.andR, "BRAMSlave: partial write masks not supported")
  }

  val stall = io.grant.valid && !io.grant.ready
  io.acquire.ready := state_init && !stall

  when (fire_acq) {
    state_getblk := is_getblk
    state_putblk := is_putblk && s0_last
  }

  when (state_getblk && fire_gnt) {
    s1_acq.addr_beat := s1_addr_beat
    state_getblk := !s1_last
  }

  when (state_putblk && fire_gnt) {
    state_putblk := Bool(false)
  }

  io.grant.valid := s1_valid
  io.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = s1_acq.getBuiltInGrantType(),
    client_xact_id = s1_acq.client_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = s1_acq.addr_beat,
    data = rdata)
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

  val is_trans = io.hsel && (io.htrans === HTRANS_NONSEQ || io.htrans === HTRANS_SEQ)
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

  io.acquire.ready := !responding
  io.grant.valid := responding
  io.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = r_acq.getBuiltInGrantType(),
    client_xact_id = r_acq.client_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = r_acq.addr_beat,
    data = ram(r_acq_addr))

  val old_data = ram(acq_addr)
  val new_data = acq.data

  val amo_shift_bits = acq.amo_shift_bytes() << UInt(3)
  val amoalu = Module(new AMOALU)
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
