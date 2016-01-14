package uncore

import Chisel._
import junctions.{SmiIO, MMIOBase}
import cde.Parameters

class SCRIO(implicit p: Parameters) extends HtifBundle()(p) {
  val rdata = Vec(nSCR, Bits(INPUT, scrDataBits))
  val wen = Bool(OUTPUT)
  val waddr = UInt(OUTPUT, log2Up(nSCR))
  val wdata = Bits(OUTPUT, scrDataBits)
}

class SCRFile(implicit p: Parameters) extends HtifModule()(p) {
  val io = new Bundle {
    val smi = new SmiIO(scrDataBits, scrAddrBits).flip
    val scr = new SCRIO
  }

  val scr_rdata = Wire(Vec(io.scr.rdata.size, Bits(width=scrDataBits)))
  for (i <- 0 until scr_rdata.size)
    scr_rdata(i) := io.scr.rdata(i)
  scr_rdata(0) := UInt(nCores)
  scr_rdata(1) := UInt(p(MMIOBase) >> 20)

  val read_addr = Reg(init = UInt(0, scrAddrBits))
  val resp_valid = Reg(init = Bool(false))

  io.smi.req.ready := !resp_valid
  io.smi.resp.valid := resp_valid
  io.smi.resp.bits := scr_rdata(read_addr)

  io.scr.wen := io.smi.req.fire() && io.smi.req.bits.rw
  io.scr.wdata := io.smi.req.bits.data
  io.scr.waddr := io.smi.req.bits.addr

  when (io.smi.req.fire()) {
    read_addr := io.smi.req.bits.addr
    resp_valid := Bool(true)
  }
  when (io.smi.resp.fire()) { resp_valid := Bool(false) }
}
