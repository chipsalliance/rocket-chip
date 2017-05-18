// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package junctions

import Chisel._
import config._

class PociIO(implicit p: Parameters) extends HastiBundle()(p)
{
  val paddr = UInt(OUTPUT, hastiAddrBits)
  val pwrite = Bool(OUTPUT)
  val psel = Bool(OUTPUT)
  val penable = Bool(OUTPUT)
  val pwdata = UInt(OUTPUT, hastiDataBits)
  val prdata = UInt(INPUT, hastiDataBits)
  val pready = Bool(INPUT)
  val pslverr = Bool(INPUT)
}

class HastiToPociBridge(implicit p: Parameters) extends HastiModule()(p) {
  val io = new Bundle {
    val in = new HastiSlaveIO
    val out = new PociIO
  }

  val s_idle :: s_setup :: s_access :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_idle)
  val transfer = io.in.hsel & io.in.htrans(1)

  switch (state) {
    is (s_idle) {
      when (transfer) { state := s_setup }
    }
    is (s_setup) {
      state := s_access
    }
    is (s_access) {
      when (io.out.pready & ~transfer) { state := s_idle   }
      when (io.out.pready & transfer)  { state := s_setup  }
      when (~io.out.pready)            { state := s_access }
    }
  }

  val haddr_reg = Reg(UInt(width = hastiAddrBits))
  val hwrite_reg = Reg(UInt(width = 1))
  when (transfer) {
    haddr_reg  := io.in.haddr
    hwrite_reg := io.in.hwrite
  }

  io.out.paddr := haddr_reg
  io.out.pwrite := hwrite_reg(0)
  io.out.psel := (state =/= s_idle)
  io.out.penable := (state === s_access)
  io.out.pwdata := io.in.hwdata
  io.in.hrdata := io.out.prdata
  io.in.hready := ((state === s_access) & io.out.pready) | (state === s_idle)
  io.in.hresp := io.out.pslverr
}

class PociBus(amap: Seq[UInt=>Bool])(implicit p: Parameters) extends HastiModule()(p)
{
  val io = new Bundle {
    val master = new PociIO().flip
    val slaves = Vec(amap.size, new PociIO)
  }

  val psels = PriorityEncoderOH(
    (io.slaves zip amap) map { case (s, afn) => {
      s.paddr := io.master.paddr
      s.pwrite := io.master.pwrite
      s.pwdata := io.master.pwdata
      afn(io.master.paddr) && io.master.psel
  }})

  (io.slaves zip psels) foreach { case (s, psel) => {
    s.psel := psel
    s.penable := io.master.penable && psel
  } }

  io.master.prdata := Mux1H(psels, io.slaves.map(_.prdata))
  io.master.pready := Mux1H(psels, io.slaves.map(_.pready))
  io.master.pslverr := Mux1H(psels, io.slaves.map(_.pslverr))
}
