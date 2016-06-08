// See LICENSE for license details.

package junctions
import Chisel._

class SlowIO[T <: Data](val divisor_max: Int)(data: => T) extends Module
{
  val io = new Bundle {
    val out_fast = Decoupled(data).flip
    val out_slow = Decoupled(data)
    val in_fast = Decoupled(data)
    val in_slow = Decoupled(data).flip
    val clk_slow = Bool(OUTPUT)
    val set_divisor = Valid(Bits(width = 32)).flip
    val divisor = Bits(OUTPUT, 32)
  }

  require(divisor_max >= 8 && divisor_max <= 65536 && isPow2(divisor_max))
  val divisor = Reg(init=UInt(divisor_max-1))
  val d_shadow = Reg(init=UInt(divisor_max-1))
  val hold = Reg(init=UInt(divisor_max/4-1))
  val h_shadow = Reg(init=UInt(divisor_max/4-1))
  when (io.set_divisor.valid) {
    d_shadow := io.set_divisor.bits(log2Up(divisor_max)-1, 0).toUInt
    h_shadow := io.set_divisor.bits(log2Up(divisor_max)-1+16, 16).toUInt
  }
  io.divisor := (hold << 16) | divisor

  val count = Reg{UInt(width = log2Up(divisor_max))}
  val myclock = Reg{Bool()}
  count := count + UInt(1)

  val rising = count === (divisor >> 1)
  val falling = count === divisor
  val held = count === (divisor >> 1) + hold

  when (falling) {
    divisor := d_shadow
    hold := h_shadow
    count := UInt(0)
    myclock := Bool(false)
  }
  when (rising) {
    myclock := Bool(true)
  }

  val in_slow_rdy = Reg(init=Bool(false))
  val out_slow_val = Reg(init=Bool(false))
  val out_slow_bits = Reg(data)

  val fromhost_q = Module(new Queue(data,1))
  fromhost_q.io.enq.valid := rising && (io.in_slow.valid && in_slow_rdy || this.reset)
  fromhost_q.io.enq.bits := io.in_slow.bits
  io.in_fast <> fromhost_q.io.deq

  val tohost_q = Module(new Queue(data,1))
  tohost_q.io.enq <> io.out_fast
  tohost_q.io.deq.ready := rising && io.out_slow.ready && out_slow_val

  when (held) {
    in_slow_rdy := fromhost_q.io.enq.ready
    out_slow_val := tohost_q.io.deq.valid
    out_slow_bits := Mux(this.reset, fromhost_q.io.deq.bits, tohost_q.io.deq.bits)
  }

  io.in_slow.ready := in_slow_rdy
  io.out_slow.valid := out_slow_val
  io.out_slow.bits := out_slow_bits
  io.clk_slow := myclock
}
