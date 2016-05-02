package uncore

import Chisel._
import cde.{Parameters, Field}
import junctions._

class BRAMSlave(depth: Int)(implicit val p: Parameters) extends Module
  with HasTileLinkParameters {
  val io = new ClientUncachedTileLinkIO().flip

  val bram = SeqMem(depth, Bits(width = tlDataBits))

  val (s0_get :: s0_getblk :: s0_put :: s0_putblk :: Nil) = Seq(
      Acquire.getType, Acquire.getBlockType, Acquire.putType, Acquire.putBlockType
    ).map(io.acquire.bits.isBuiltInType _)

  val fire_acq = io.acquire.fire()
  val fire_gnt = io.grant.fire()

  val multibeat = Reg(init = Bool(false))
  when (fire_acq) {
    multibeat := s0_getblk
  }

  val s0_valid = io.acquire.valid || multibeat
  val s1_valid = Reg(next = s0_valid, init = Bool(false))
  val s1_acq = RegEnable(io.acquire.bits, fire_acq)

  val s0_addr = Cat(io.acquire.bits.addr_block, io.acquire.bits.addr_beat)
  val s1_beat = s1_acq.addr_beat + Mux(io.grant.ready, UInt(1), UInt(0))
  val s1_addr = Cat(s1_acq.addr_block, s1_beat)
  val raddr = Mux(multibeat, s1_addr, s0_addr)

  val last = (s1_acq.addr_beat === UInt(tlDataBeats-1))
  val ren = (io.acquire.valid && (s0_get || s0_getblk)) || (multibeat && !last)
  val wen = (io.acquire.valid && (s0_put || s0_putblk))

  val rdata = bram.read(raddr, ren)
  val wdata = io.acquire.bits.data
  val wmask = io.acquire.bits.wmask()
  assert(!io.acquire.valid || wmask === Fill(tlDataBytes, Bool(true)),
    "bram: subblock writes not supported")
  when (wen) {
    bram.write(s0_addr, wdata)
  }

  when (multibeat && fire_gnt) {
    s1_acq.addr_beat := s1_beat
    when (last) {
      multibeat := Bool(false)
    }
  }

  io.grant.valid := s1_valid
  io.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = s1_acq.getBuiltInGrantType(),
    client_xact_id = s1_acq.client_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = s1_acq.addr_beat,
    data = rdata)

  val stall = multibeat || (io.grant.valid && !io.grant.ready)
  io.acquire.ready := !stall
}

class HastiRAM(depth: Int)(implicit p: Parameters) extends HastiModule()(p) {
  val io = new HastiSlaveIO

  val hastiDataBytes = hastiDataBits/8

  val wdata = Vec.tabulate(hastiDataBytes)(i => io.hwdata(8*(i+1)-1,8*i))
  val waddr = Reg(UInt(width = hastiAddrBits))
  val wvalid = Reg(init = Bool(false))
  val wsize = Reg(UInt(width = SZ_HSIZE))
  val ram = SeqMem(depth, Vec(hastiDataBytes, Bits(width = 8)))

  val max_wsize = log2Ceil(hastiDataBytes)
  val wmask_lut = MuxLookup(wsize, SInt(-1, hastiDataBytes).asUInt,
    (0 until max_wsize).map(1 << _).map(sz => (UInt(sz) -> UInt((1 << sz << sz) - 1))))
  val wmask = (wmask_lut << waddr(max_wsize - 1, 0))(hastiDataBytes - 1, 0)

  val is_trans = io.hsel && (io.htrans === HTRANS_NONSEQ || io.htrans === HTRANS_SEQ)
  val raddr = io.haddr >> UInt(2)
  val ren = is_trans && !io.hwrite
  val bypass = Reg(init = Bool(false))
  val last_wdata = Reg(next = wdata)
  val last_wmask = Reg(next = wmask)

  when (is_trans && io.hwrite) {
    waddr := io.haddr
    wsize := io.hsize
    wvalid := Bool(true)
  } .otherwise { wvalid := Bool(false) }

  when (ren) { bypass := wvalid && (waddr >> UInt(2)) === raddr }

  when (wvalid) {
    ram.write(waddr >> UInt(2), wdata, wmask.toBools)
  }

  val rdata = ram.read(raddr, ren)
  io.hrdata := Cat(rdata.zip(wmask.toBools).zip(wdata).map {
    case ((rbyte, wsel), wbyte) => Mux(wsel && bypass, wbyte, rbyte)
  }.reverse)

  io.hreadyout := Bool(true)
  io.hresp := HRESP_OKAY
}
