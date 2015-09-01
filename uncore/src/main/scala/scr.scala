package uncore

import Chisel._
import junctions.{SMIIO, MMIOBase}

class SCRIO extends HTIFBundle {
  val rdata = Vec(Bits(INPUT, 64), nSCR)
  val wen = Bool(OUTPUT)
  val waddr = UInt(OUTPUT, log2Up(nSCR))
  val wdata = Bits(OUTPUT, 64)
}

class SCRFile extends Module with HTIFParameters {
  val io = new Bundle {
    val smi = new SMIIO(64, scrAddrBits).flip
    val scr = new SCRIO
  }

  val scr_rdata = Wire(Vec(Bits(width=64), io.scr.rdata.size))
  for (i <- 0 until scr_rdata.size)
    scr_rdata(i) := io.scr.rdata(i)
  scr_rdata(0) := UInt(nCores)
  scr_rdata(1) := UInt(params(MMIOBase) >> 20)

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
