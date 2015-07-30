// See LICENSE for license details.

package rocketchip

import Chisel._
import junctions._
import uncore._

class MemDessert extends Module {
  val io = new MemDesserIO(params(HTIFWidth))
  val x = Module(new MemDesser(params(HTIFWidth)))
  io.narrow <> x.io.narrow
  io.wide <> x.io.wide
}

object VLSIUtils {
  def doOuterMemorySystemSerdes(
      llcs: Seq[MemIO],
      mems: Seq[MemIO],
      backup: MemSerializedIO,
      en: Bool,
      nMemChannels: Int,
      htifWidth: Int) {
    val arb = Module(new MemIOArbiter(nMemChannels))
    val mem_serdes = Module(new MemSerdes(htifWidth))
    mem_serdes.io.wide <> arb.io.outer
    mem_serdes.io.narrow <> backup

    llcs zip mems zip arb.io.inner foreach { case ((llc, mem), wide) =>
      llc.req_cmd.ready := Mux(en, wide.req_cmd.ready, mem.req_cmd.ready)
      mem.req_cmd.valid := llc.req_cmd.valid && !en
      mem.req_cmd.bits := llc.req_cmd.bits
      wide.req_cmd.valid := llc.req_cmd.valid && en
      wide.req_cmd.bits := llc.req_cmd.bits

      llc.req_data.ready := Mux(en, wide.req_data.ready, mem.req_data.ready)
      mem.req_data.valid := llc.req_data.valid && !en
      mem.req_data.bits := llc.req_data.bits
      wide.req_data.valid := llc.req_data.valid && en
      wide.req_data.bits := llc.req_data.bits

      llc.resp.valid := Mux(en, wide.resp.valid, mem.resp.valid)
      llc.resp.bits := Mux(en, wide.resp.bits, mem.resp.bits)
      mem.resp.ready  := llc.resp.ready && !en
      wide.resp.ready := llc.resp.ready && en
    }
  }

  def padOutHTIFWithDividedClock(
      htif: HTIFModuleIO,
      child: MemSerializedIO, 
      parent: MemBackupCtrlIO,
      host: HostIO,
      htifW: Int) {
    val hio = Module((new SlowIO(512)) { Bits(width = htifW+1) })
    hio.io.set_divisor.valid := htif.scr.wen && (htif.scr.waddr === UInt(63))
    hio.io.set_divisor.bits := htif.scr.wdata
    htif.scr.rdata(63) := hio.io.divisor

    hio.io.out_fast.valid := htif.host.out.valid || child.req.valid
    hio.io.out_fast.bits := Cat(htif.host.out.valid, Mux(htif.host.out.valid, htif.host.out.bits, child.req.bits))
    htif.host.out.ready := hio.io.out_fast.ready
    child.req.ready := hio.io.out_fast.ready && !htif.host.out.valid
    host.out.valid := hio.io.out_slow.valid && hio.io.out_slow.bits(htifW)
    host.out.bits := hio.io.out_slow.bits
    parent.out_valid := hio.io.out_slow.valid && !hio.io.out_slow.bits(htifW)
    hio.io.out_slow.ready := Mux(hio.io.out_slow.bits(htifW), host.out.ready, parent.out_ready)

    val mem_backup_resp_valid = parent.en && parent.in_valid
    hio.io.in_slow.valid := mem_backup_resp_valid || host.in.valid
    hio.io.in_slow.bits := Cat(mem_backup_resp_valid, host.in.bits)
    host.in.ready := hio.io.in_slow.ready
    child.resp.valid := hio.io.in_fast.valid && hio.io.in_fast.bits(htifW)
    child.resp.bits := hio.io.in_fast.bits
    htif.host.in.valid := hio.io.in_fast.valid && !hio.io.in_fast.bits(htifW)
    htif.host.in.bits := hio.io.in_fast.bits
    hio.io.in_fast.ready := Mux(hio.io.in_fast.bits(htifW), Bool(true), htif.host.in.ready)
    host.clk := hio.io.clk_slow
    host.clk_edge := Reg(next=host.clk && !Reg(next=host.clk))
  }
}
