package rocket

import Chisel._
import Constants._

class slowIO[T <: Data](divisor: Int, hold_cycles: Int)(data: => T) extends Component
{
  val io = new Bundle {
    val out_fast = new ioDecoupled()(data).flip
    val out_slow = new ioDecoupled()(data)

    val in_fast = new ioDecoupled()(data)
    val in_slow = new ioDecoupled()(data).flip

    val clk_slow = Bool(OUTPUT)
  }

  require((divisor & (divisor-1)) == 0)
  require(hold_cycles < divisor/2 && hold_cycles >= 2)

  val cnt = Reg() { UFix(width = log2up(divisor)) }
  cnt := cnt + UFix(1)
  val out_en = cnt === UFix(divisor/2+hold_cycles-1) // rising edge + hold time
  val in_en = cnt === UFix(divisor/2-1) // rising edge

  val in_slow_rdy = Reg(resetVal = Bool(false))
  val out_slow_val = Reg(resetVal = Bool(false))
  val out_slow_bits = Reg() { data }

  val fromhost_q = new queue(1)(data)
  fromhost_q.io.enq.valid := in_en && (io.in_slow.valid && in_slow_rdy || reset)
  fromhost_q.io.enq.bits := io.in_slow.bits
  fromhost_q.io.deq <> io.in_fast

  val tohost_q = new queue(1)(data)
  tohost_q.io.enq <> io.out_fast
  tohost_q.io.deq.ready := in_en && io.out_slow.ready && out_slow_val

  when (out_en) {
    in_slow_rdy := fromhost_q.io.enq.ready
    out_slow_val := tohost_q.io.deq.valid
    out_slow_bits := Mux(reset, fromhost_q.io.deq.bits, tohost_q.io.deq.bits)
  }

  io.in_slow.ready := in_slow_rdy
  io.out_slow.valid := out_slow_val
  io.out_slow.bits := out_slow_bits
  io.clk_slow := cnt(log2up(divisor)-1).toBool
}
