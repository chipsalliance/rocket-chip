package rocket

import Chisel._
import ALU._

class MultiplierReq(implicit conf: RocketConfiguration) extends Bundle {
  val fn = Bits(width = SZ_ALU_FN)
  val dw = Bits(width = SZ_DW)
  val in1 = Bits(width = conf.xprlen)
  val in2 = Bits(width = conf.xprlen)
  val tag = UInt(width = conf.nxprbits)

  override def clone = new MultiplierReq().asInstanceOf[this.type]
}

class MultiplierResp(implicit conf: RocketConfiguration) extends Bundle {
  val data = Bits(width = conf.xprlen)
  val tag = UInt(width = conf.nxprbits)

  override def clone = new MultiplierResp().asInstanceOf[this.type]
}

class MultiplierIO(implicit conf: RocketConfiguration) extends Bundle {
  val req = Decoupled(new MultiplierReq).flip
  val kill = Bool(INPUT)
  val resp = Decoupled(new MultiplierResp)
}

class Multiplier(unroll: Int = 1, earlyOut: Boolean = false)(implicit conf: RocketConfiguration) extends Module {
  val io = new MultiplierIO

  val w0 = io.req.bits.in1.getWidth
  val w = (w0+1+unroll-1)/unroll*unroll
  val cycles = w/unroll
  
  val r_val = Reg(init=Bool(false))
  val r_prod = Reg(Bits(width = w*2))
  val r_lsb = Reg(Bits())
  val r_cnt = Reg(UInt(width = log2Up(cycles+1)))
  val r_req = Reg(new MultiplierReq)
  val r_lhs = Reg(Bits(width = w0+1))

  val dw = io.req.bits.dw
  val fn = io.req.bits.fn

  val lhs_msb = Mux(dw === DW_64, io.req.bits.in1(w0-1), io.req.bits.in1(w0/2-1)).toBool
  val lhs_sign = (isMulFN(fn, FN_MULH) || isMulFN(fn, FN_MULHSU)) && lhs_msb
  val lhs_hi = Mux(dw === DW_64, io.req.bits.in1(w0-1,w0/2), Fill(w0/2, lhs_sign))
  val lhs_in = Cat(lhs_sign, lhs_hi, io.req.bits.in1(w0/2-1,0))

  val rhs_msb = Mux(dw === DW_64, io.req.bits.in2(w0-1), io.req.bits.in2(w0/2-1)).toBool
  val rhs_sign = isMulFN(fn, FN_MULH) && rhs_msb
  val rhs_hi = Mux(dw === DW_64, io.req.bits.in2(w0-1,w0/2), Fill(w0/2, rhs_sign))
  val rhs_in = Cat(Fill(w-w0, rhs_sign), rhs_hi, io.req.bits.in2(w0/2-1,0))
  
  when (io.req.fire()) {
    r_val := Bool(true)
    r_cnt := UInt(0, log2Up(cycles+1))
    r_req := io.req.bits
    r_lhs := lhs_in
    r_prod:= rhs_in
    r_lsb := Bool(false)
  }
  .elsewhen (io.resp.fire() || io.kill) {
    r_val := Bool(false)
  }

  val eOutDist = (UInt(cycles)-r_cnt)*UInt(unroll)
  val outShift = Mux(isMulFN(r_req.fn, FN_MUL), UInt(0), Mux(r_req.dw === DW_64, UInt(64), UInt(32)))
  val shiftDist = Mux(r_cnt === UInt(cycles), outShift, eOutDist)
  val eOutMask = (UInt(1) << eOutDist) - UInt(1)
  val eOut = r_cnt != UInt(0) && Bool(earlyOut) && !((r_prod(w-1,0) ^ r_lsb.toSInt) & eOutMask).orR
  val shift = r_prod.toSInt >> shiftDist

  val sum = r_prod(2*w-1,w).toSInt + r_prod(unroll-1,0).toSInt * r_lhs.toSInt + Mux(r_lsb.toBool, r_lhs.toSInt, SInt(0))
  when (r_val && (r_cnt != UInt(cycles))) {
    r_lsb  := r_prod(unroll-1)
    r_prod := Cat(sum, r_prod(w-1,unroll)).toSInt
    r_cnt := r_cnt + UInt(1)
    when (eOut) {
      r_prod := shift
      r_cnt := UInt(cycles)
    }
  }

  val out32 = Cat(Fill(w0/2, shift(w0/2-1)), shift(w0/2-1,0))
  val out64 = shift(w0-1,0)
 
  io.req.ready := !r_val
  io.resp.bits := r_req
  io.resp.bits.data := Mux(r_req.dw === DW_64, out64, out32)
  io.resp.valid := r_val && (r_cnt === UInt(cycles))
}
