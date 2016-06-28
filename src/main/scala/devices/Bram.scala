package uncore.devices

import Chisel._
import cde.{Parameters, Field}
import junctions._
import uncore.tilelink._
import HastiConstants._

class BRAMSlave(depth: Int)(implicit val p: Parameters) extends Module
  with HasTileLinkParameters {
  val io = new ClientUncachedTileLinkIO().flip

  val bram = SeqMem(depth, Vec(tlDataBytes, UInt(width = 8)))

  val s_idle :: s_getblk :: s_putblk :: s_resp :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  val acq = io.acquire.bits
  val r_acq = Reg(new AcquireMetadata)

  val (is_get :: is_getblk :: is_put :: is_putblk :: Nil) = Seq(
      Acquire.getType, Acquire.getBlockType, Acquire.putType, Acquire.putBlockType
    ).map(acq.isBuiltInType _)

  val beats = Reg(UInt(width = tlBeatAddrBits))

  when (io.acquire.fire()) {
    r_acq := acq
    when (is_get || is_put || acq.isPrefetch()) {
      state := s_resp
    }
    when (is_getblk) {
      beats := UInt(tlDataBeats - 1)
      state := s_getblk
    }
    /** Need to collect the rest of the beats.
     *  Beat 0 has already been accepted. */
    when (is_putblk) {
      beats := UInt(tlDataBeats - 2)
      state := s_putblk
    }
  }

  when (state === s_getblk && io.grant.ready) {
    r_acq.addr_beat := r_acq.addr_beat + UInt(1)
    beats := beats - UInt(1)
    when (beats === UInt(0)) { state := s_idle }
  }

  when (state === s_putblk && io.acquire.valid) {
    beats := beats - UInt(1)
    when (beats === UInt(0)) { state := s_resp }
  }

  when (state === s_resp && io.grant.ready) { state := s_idle }

  val acq_addr = Cat(acq.addr_block, acq.addr_beat)
  val r_acq_addr = Cat(r_acq.addr_block, r_acq.addr_beat)

  val ren = (io.acquire.fire() && (is_get || is_getblk)) ||
            (state === s_getblk && io.grant.ready)
  val raddr = Mux(state === s_idle, acq_addr,
    Mux(io.grant.ready, r_acq_addr + UInt(1), r_acq_addr))
  val rdata = bram.read(raddr, ren)

  val wen = (io.acquire.fire() && (is_put || is_putblk))
  val wdata = Vec.tabulate(tlDataBytes) { i => acq.data((i+1)*8-1, i*8) }
  val wmask = Vec.tabulate(tlWriteMaskBits) { i => acq.wmask()(i) }

  when (wen) { bram.write(acq_addr, wdata, wmask) }

  io.grant.valid := (state === s_resp) || (state === s_getblk)
  io.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = r_acq.getBuiltInGrantType(),
    client_xact_id = r_acq.client_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = r_acq.addr_beat,
    data = rdata.toBits)
  io.acquire.ready := (state === s_idle) || (state === s_putblk)
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
