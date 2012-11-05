package rocket

import Chisel._
import Node._
import Constants._
import hwacha._
import hwacha.Constants._

class ioMultiplier extends Bundle {
  val req = new io_imul_req().flip
  val req_tag = UFix(INPUT, 5)
  val req_kill = Bool(INPUT)
  val resp_val = Bool(OUTPUT)
  val resp_rdy = Bool(INPUT)
  val resp_tag = UFix(OUTPUT, 5)
  val resp_bits = Bits(OUTPUT, SZ_XLEN)
}

class rocketVUMultiplier(nwbq: Int) extends Component {
  val io = new Bundle {
    val cpu = new ioMultiplier
    val vu = new Bundle {
      val req = new io_imul_req
      val resp = Bits(INPUT, SZ_XLEN)
    }
  }

  val valid = Reg(resetVal = Bits(0, IMUL_STAGES))
  val wbq_cnt = Reg(resetVal = Bits(0, log2Up(nwbq+1)))
  val tag = Vec(IMUL_STAGES) { Reg() { Bits() } }

  val fire = io.cpu.req.valid && io.cpu.req.ready

  valid := Cat(fire, valid(IMUL_STAGES-1) && !io.cpu.req_kill, valid(IMUL_STAGES-2,1))
  when (fire) {
    tag(IMUL_STAGES-1) := io.cpu.req_tag
  }
  for (i <- 0 until IMUL_STAGES-1) {
    tag(i) := tag(i+1)
  }
  when (valid(0) != (io.cpu.resp_val && io.cpu.resp_rdy)) {
    wbq_cnt := Mux(valid(0), wbq_cnt + UFix(1), wbq_cnt - UFix(1))
  }

  var inflight_cnt = valid(0)
  for (i <- 1 until IMUL_STAGES)
    inflight_cnt = inflight_cnt + valid(i)
  inflight_cnt = inflight_cnt + wbq_cnt
  val wbq_rdy = inflight_cnt < UFix(nwbq)

  val wbq = (new Queue(nwbq)) { Bits(width = io.cpu.resp_bits.width + io.cpu.resp_tag.width) }
  wbq.io.enq.valid := valid(0)
  wbq.io.enq.bits := Cat(io.vu.resp, tag(0))
  wbq.io.deq.ready := io.cpu.resp_rdy

  io.cpu.req.ready := io.vu.req.ready && wbq_rdy
  io.cpu.resp_val := wbq.io.deq.valid
  io.cpu.resp_bits := wbq.io.deq.bits >> UFix(io.cpu.resp_tag.width)
  io.cpu.resp_tag := wbq.io.deq.bits(io.cpu.resp_tag.width-1,0).toUFix

  io.vu.req <> io.cpu.req
}

class rocketMultiplier(unroll: Int = 1, earlyOut: Boolean = false) extends Component {
  val io = new ioMultiplier

  val w0 = io.req.bits.in0.getWidth
  val w = (w0+1+unroll-1)/unroll*unroll
  val cycles = w/unroll
  
  val r_val = Reg(resetVal = Bool(false));
  val r_dw  = Reg { Bits() }
  val r_fn  = Reg { Bits() }
  val r_tag = Reg { UFix() }
  val r_lhs = Reg { Bits() }
  val r_prod= Reg { Bits(width = w*2) }
  val r_lsb = Reg { Bits() }
  val r_cnt = Reg { UFix(width = log2Up(cycles+1)) }

  val dw = io.req.bits.fn(io.req.bits.fn.width-1)
  val fn = io.req.bits.fn(io.req.bits.fn.width-2,0)

  val lhs_msb = Mux(dw === DW_64, io.req.bits.in0(w0-1), io.req.bits.in0(w0/2-1)).toBool
  val lhs_sign = ((fn === MUL_H) || (fn === MUL_HSU)) && lhs_msb
  val lhs_hi = Mux(dw === DW_64, io.req.bits.in0(w0-1,w0/2), Fill(w0/2, lhs_sign))
  val lhs_in = Cat(lhs_sign, lhs_hi, io.req.bits.in0(w0/2-1,0))

  val rhs_msb = Mux(dw === DW_64, io.req.bits.in1(w0-1), io.req.bits.in1(w0/2-1)).toBool
  val rhs_sign = (fn === MUL_H) && rhs_msb
  val rhs_hi = Mux(dw === DW_64, io.req.bits.in1(w0-1,w0/2), Fill(w0/2, rhs_sign))
  val rhs_in = Cat(Fill(w-w0, rhs_sign), rhs_hi, io.req.bits.in1(w0/2-1,0))
  
  when (io.req.valid && io.req.ready) {
    r_val := Bool(true)
    r_cnt := UFix(0, log2Up(cycles+1))
    r_dw  := dw
    r_fn  := fn
    r_tag := io.req_tag
    r_lhs := lhs_in
    r_prod:= rhs_in
    r_lsb := Bool(false)
  }
  .elsewhen (io.resp_val && io.resp_rdy || io.req_kill) {
    r_val := Bool(false)
  }

  val eOutDist = (UFix(cycles)-r_cnt)*UFix(unroll)
  val outShift = Mux(r_fn === MUL_LO, UFix(0), Mux(r_dw === DW_64, UFix(64), UFix(32)))
  val shiftDist = Mux(r_cnt === UFix(cycles), outShift, eOutDist)
  val eOutMask = (UFix(1) << eOutDist) - UFix(1)
  val eOut = r_cnt != UFix(0) && Bool(earlyOut) && !((r_prod(w-1,0) ^ r_lsb.toFix) & eOutMask).orR
  val shift = r_prod.toFix >> shiftDist

  val sum = r_prod(2*w-1,w).toFix + r_prod(unroll-1,0).toFix * r_lhs.toFix + Mux(r_lsb, r_lhs.toFix, Fix(0))
  when (r_val && (r_cnt != UFix(cycles))) {
    r_lsb  := r_prod(unroll-1)
    r_prod := Cat(sum, r_prod(w-1,unroll)).toFix
    r_cnt := r_cnt + UFix(1)
    when (eOut) {
      r_prod := shift
      r_cnt := UFix(cycles)
    }
  }

  val out32 = Cat(Fill(w0/2, shift(w0/2-1)), shift(w0/2-1,0))
  val out64 = shift(w0-1,0)
 
  io.req.ready := !r_val
  io.resp_bits := Mux(r_dw === DW_64, out64, out32)
  io.resp_tag := r_tag;
  io.resp_val := r_val && (r_cnt === UFix(cycles))
}
