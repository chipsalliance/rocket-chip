package Top

import Chisel._
import Node._
import Constants._
import hwacha._
import hwacha.Constants._

class ioMultiplier extends Bundle {
  val req = new io_imul_req().flip()
  val req_tag = Bits(5, INPUT)
  val req_kill = Bool(INPUT)
  val resp_val = Bool(OUTPUT)
  val resp_rdy = Bool(INPUT)
  val resp_tag = Bits(5, OUTPUT)
  val resp_bits = Bits(SZ_XLEN, OUTPUT)
}

class rocketVUMultiplier(nwbq: Int) extends Component {
  val io = new Bundle {
    val cpu = new ioMultiplier
    val vu = new Bundle {
      val req = new io_imul_req
      val resp = Bits(SZ_XLEN, INPUT)
    }
  }

  val valid = Reg(resetVal = Bits(0, IMUL_STAGES))
  val wbq_cnt = Reg(resetVal = Bits(0, log2up(nwbq+1)))
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

  val wbq = (new queue(nwbq)) { Bits(width = io.cpu.resp_bits.width + io.cpu.resp_tag.width) }
  wbq.io.enq.valid := valid(0)
  wbq.io.enq.bits := Cat(io.vu.resp, tag(0))
  wbq.io.deq.ready := io.cpu.resp_rdy

  io.cpu.req.ready := io.vu.req.ready && wbq_rdy
  io.cpu.resp_val := wbq.io.deq.valid
  io.cpu.resp_bits := wbq.io.deq.bits >> UFix(io.cpu.resp_tag.width)
  io.cpu.resp_tag := wbq.io.deq.bits(io.cpu.resp_tag.width-1,0)

  io.vu.req <> io.cpu.req
}

class rocketMultiplier extends Component {
  val io = new ioMultiplier
  // w must be even (booth).
  // we need an extra bit to handle signed vs. unsigned,
  // so we need to add a second to keep w even.
  val w = 64 + 2
  val unroll = 3

  require(w % 2 == 0 && (w/2) % unroll == 0)

  val cycles = w/unroll/2
  
  val r_val = Reg(resetVal = Bool(false));
  val r_dw  = Reg { Bits() }
  val r_fn  = Reg { Bits() }
  val r_tag = Reg { Bits() }
  val r_lhs = Reg { Bits() }
  val r_prod= Reg { Bits(width = w*2) }
  val r_lsb = Reg { Bits() }
  val r_cnt = Reg { UFix(width = log2up(cycles+1)) }

  val dw = io.req.bits.fn(io.req.bits.fn.width-1)
  val fn = io.req.bits.fn(io.req.bits.fn.width-2,0)

  val lhs_msb = Mux(dw === DW_64, io.req.bits.in0(63), io.req.bits.in0(31)).toBool
  val lhs_sign = ((fn === MUL_H) || (fn === MUL_HSU)) && lhs_msb
  val lhs_hi = Mux(dw === DW_64, io.req.bits.in0(63,32), Fill(32, lhs_sign))
  val lhs_in = Cat(lhs_sign, lhs_hi, io.req.bits.in0(31,0))

  val rhs_msb = Mux(dw === DW_64, io.req.bits.in1(63), io.req.bits.in1(31)).toBool
  val rhs_sign = (fn === MUL_H) && rhs_msb
  val rhs_hi = Mux(dw === DW_64, io.req.bits.in1(63,32), Fill(32, rhs_sign))
  val rhs_in = Cat(rhs_sign, rhs_sign, rhs_hi, io.req.bits.in1(31,0))

  val do_kill = io.req_kill && r_cnt === UFix(0) // can only kill on 1st cycle
  
  when (io.req.valid && io.req.ready) {
    r_val := Bool(true)
    r_cnt := UFix(0, log2up(cycles+1))
    r_dw  := dw
    r_fn  := fn
    r_tag := io.req_tag
    r_lhs := lhs_in
    r_prod:= rhs_in
    r_lsb := Bool(false)
  }
  .elsewhen (io.resp_val && io.resp_rdy || do_kill) { // can only kill on first cycle
    r_val := Bool(false)
  }

  val lhs_sext = Cat(r_lhs(w-2), r_lhs(w-2), r_lhs).toUFix
  val lhs_twice = Cat(r_lhs(w-2), r_lhs, Bits(0,1)).toUFix

  var prod = r_prod
  var lsb = r_lsb

  for (i <- 0 until unroll) {
    val addend = Mux(prod(0) != lsb,     lhs_sext,
                 Mux(prod(0) != prod(1), lhs_twice,
                                             UFix(0)));
    val sub = prod(1)
    val adder_lhs = Cat(prod(w*2-1), prod(w*2-1,w)).toUFix
    val adder_rhs = Mux(sub, ~addend, addend)
    val adder_out = (adder_lhs + adder_rhs + sub.toUFix)(w,0)

    lsb = prod(1)
    prod = Cat(adder_out(w), adder_out, prod(w-1,2))
  }

  when (r_val && (r_cnt != UFix(cycles))) {
    r_lsb  := lsb
    r_prod := prod
    r_cnt := r_cnt + UFix(1)
  }

  val mul_output64 = Mux(r_fn === MUL_LO, r_prod(63,0), r_prod(127,64))
  val mul_output32 = Mux(r_fn === MUL_LO, r_prod(31,0), r_prod(63,32))
  val mul_output32_ext = Cat(Fill(32, mul_output32(31)), mul_output32)
  
  val mul_output = Mux(r_dw === DW_64, mul_output64, mul_output32_ext)
 
  io.req.ready := !r_val
  io.resp_bits := mul_output;
  io.resp_tag := r_tag;
  io.resp_val := r_val && (r_cnt === UFix(cycles))
}
