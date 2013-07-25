package uncore
import Chisel._

class SlowIO[T <: Data](val divisor_max: Int)(data: => T) extends Component
{
  val io = new Bundle {
    val out_fast = new FIFOIO()(data).flip
    val out_slow = new FIFOIO()(data)

    val in_fast = new FIFOIO()(data)
    val in_slow = new FIFOIO()(data).flip

    val clk_slow = Bool(OUTPUT)

    val set_divisor = new PipeIO()(Bits(width = 32)).flip
    val divisor = Bits(OUTPUT, 32)
  }

  require(divisor_max >= 8 && divisor_max <= 65536 && isPow2(divisor_max))
  val divisor = Reg(resetVal = UFix(divisor_max-1))
  val d_shadow = Reg(resetVal = UFix(divisor_max-1))
  val hold = Reg(resetVal = UFix(divisor_max/4-1))
  val h_shadow = Reg(resetVal = UFix(divisor_max/4-1))
  when (io.set_divisor.valid) {
    d_shadow := io.set_divisor.bits(log2Up(divisor_max)-1, 0).toUFix
    h_shadow := io.set_divisor.bits(log2Up(divisor_max)-1+16, 16).toUFix
  }
  io.divisor := hold << UFix(16) | divisor

  val count = Reg{UFix(width = log2Up(divisor_max))}
  val clock = Reg{Bool()}
  count := count + UFix(1)

  val rising = count === (divisor >> UFix(1))
  val falling = count === divisor
  val held = count === (divisor >> UFix(1)) + hold

  when (falling) {
    divisor := d_shadow
    hold := h_shadow
    count := UFix(0)
    clock := Bool(false)
  }
  when (rising) {
    clock := Bool(true)
  }

  val in_slow_rdy = Reg(resetVal = Bool(false))
  val out_slow_val = Reg(resetVal = Bool(false))
  val out_slow_bits = Reg() { data }

  val fromhost_q = new Queue(1)(data)
  fromhost_q.io.enq.valid := rising && (io.in_slow.valid && in_slow_rdy || reset)
  fromhost_q.io.enq.bits := io.in_slow.bits
  fromhost_q.io.deq <> io.in_fast

  val tohost_q = new Queue(1)(data)
  tohost_q.io.enq <> io.out_fast
  tohost_q.io.deq.ready := rising && io.out_slow.ready && out_slow_val

  when (held) {
    in_slow_rdy := fromhost_q.io.enq.ready
    out_slow_val := tohost_q.io.deq.valid
    out_slow_bits := Mux(reset, fromhost_q.io.deq.bits, tohost_q.io.deq.bits)
  }

  io.in_slow.ready := in_slow_rdy
  io.out_slow.valid := out_slow_val
  io.out_slow.bits := out_slow_bits
  io.clk_slow := clock
}
