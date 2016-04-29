package uncore

import Chisel._
import junctions._
import cde.{Parameters, Field}

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
  val rows = (contents.size + byteWidth - 1)/byteWidth + 1
  val rom = Vec.tabulate(rows) { i =>
    val slice = contents.slice(i*byteWidth, (i+1)*byteWidth)
    UInt(slice.foldRight(BigInt(0)) { case (x,y) => (y << 8) + (x.toInt & 0xFF) }, byteWidth*8)
  }
  val rdata_word = rom(if (rows == 1) UInt(0) else ar.bits.addr(log2Up(contents.size)-1,log2Up(byteWidth)))
  val rdata = new LoadGen(Cat(UInt(1), ar.bits.size), ar.bits.addr, rdata_word, Bool(false), byteWidth).data

  io.r <> ar
  io.r.bits := NastiReadDataChannel(ar.bits.id, rdata)
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
