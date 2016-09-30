// See LICENSE for license details.

package junctions
import Chisel._

class SlowIOClockDivider(val divisor_max: Int) extends Module {
  val io = new Bundle {
    val divisor = UInt(INPUT, log2Up(divisor_max))
    val slow_clk = Bool(OUTPUT)
    val slow_clk_rising = Bool(OUTPUT)
    val slow_clk_falling = Bool(OUTPUT)
  }

  val count = Reg(init = UInt(0, log2Up(divisor_max)))
  val divisor = Reg(init = UInt(divisor_max-1))

  val rising = count === (divisor >> UInt(1))
  val falling = count === divisor

  val slow_clock = Reg(init = Bool(false))

  when (rising) { slow_clock := Bool(true) }
  when (falling) {
    slow_clock := Bool(false)
    count := UInt(0)
    divisor := io.divisor
  } .otherwise {
    count := count + UInt(1)
  }
  io.slow_clk := slow_clock
  io.slow_clk_rising := rising
  io.slow_clk_falling := falling
}

class SlowIOTransmitter[T <: Data](val divisor_max: Int, data: T) extends Module {
  val io = new Bundle {
    val slow = new Bundle {
      val clk = Bool(OUTPUT)
      val out = Decoupled(data)
      val in  = Decoupled(data).flip
    }
    val fast = new Bundle {
      val out = Decoupled(data).flip
      val in  = Decoupled(data)
    }
    val divisor = UInt(INPUT, log2Up(divisor_max))
  }

  val divider = Module(new SlowIOClockDivider(divisor_max))
  divider.io.divisor := io.divisor

  val rising = divider.io.slow_clk_rising
  val falling = divider.io.slow_clk_falling

  io.slow.clk := divider.io.slow_clk

  val slow_out_valid = Reg(init = Bool(false))
  val slow_out_bits  = Reg(data)
  val slow_in_ready  = Reg(init = Bool(false))

  // Only take inputs on the rising edge
  val outq = Module(new Queue(data, 1))
  outq.io.enq <> io.fast.out
  outq.io.deq.ready := io.slow.out.ready && slow_out_valid && rising

  val inq = Module(new Queue(data, 1))
  io.fast.in <> inq.io.deq
  inq.io.enq.valid := io.slow.in.valid && slow_in_ready && rising
  inq.io.enq.bits  := io.slow.in.bits

  // Only change outputs on the falling edge
  when (falling) {
    slow_out_valid := outq.io.deq.valid
    slow_out_bits  := outq.io.deq.bits
    slow_in_ready  := inq.io.enq.ready
  }

  io.slow.out.valid := slow_out_valid
  io.slow.out.bits  := slow_out_bits
  io.slow.in.ready  := slow_in_ready
}

class SlowIOReceiver[T <: Data](data: T) extends Module {
  val io = new Bundle {
    val slow = new Bundle {
      val clk = Bool(INPUT)
      val out = Decoupled(data)
      val in  = Decoupled(data).flip
    }
    val fast = new Bundle {
      val out = Decoupled(data).flip
      val in  = Decoupled(data)
    }
  }

  val last_clk = Reg(next = io.slow.clk)
  val rising = io.slow.clk && !last_clk
  val falling = !io.slow.clk && last_clk

  val slow_in_ready = Reg(init = Bool(false))
  val slow_out_valid = Reg(init = Bool(false))
  val slow_out_bits = Reg(data)

  // Only take inputs on the rising edge
  val inq = Module(new Queue(data, 1))
  inq.io.enq.valid := io.slow.in.valid && slow_in_ready && rising
  inq.io.enq.bits  := io.slow.in.bits
  io.fast.in <> inq.io.deq

  val outq = Module(new Queue(data, 1))
  outq.io.deq.ready := io.slow.out.ready && slow_out_valid && rising
  outq.io.enq <> io.fast.out

  // Only change outputs on the falling edge
  when (falling) {
    slow_in_ready := inq.io.enq.ready
    slow_out_valid := outq.io.deq.valid
    slow_out_bits := outq.io.deq.bits
  }

  io.slow.in.ready := slow_in_ready
  io.slow.out.valid := slow_out_valid
  io.slow.out.bits := slow_out_bits
}

class SlowIOUnitTest extends unittest.UnitTest {
  val w = 32
  val tx = Module(new SlowIOTransmitter(64, UInt(width = w)))
  val rx = Module(new SlowIOReceiver(UInt(width = w)))

  tx.io.divisor := UInt(31)

  rx.io.slow.in <> tx.io.slow.out
  tx.io.slow.in <> rx.io.slow.out
  rx.io.slow.clk := tx.io.slow.clk

  // Some random delays
  val delay_rand = LFSR16()
  val recv_delay = Reg(UInt(width = 8))
  val send_delay = Reg(UInt(width = 8))

  // Echo everything back with random delay
  rx.io.fast.out.valid := rx.io.fast.in.valid && recv_delay === UInt(0)
  rx.io.fast.out.bits := rx.io.fast.in.bits
  rx.io.fast.in.ready := rx.io.fast.out.ready && recv_delay === UInt(0)

  when (rx.io.fast.out.fire()) {
    recv_delay := delay_rand
  } .elsewhen (recv_delay =/= UInt(0)) {
    recv_delay := recv_delay - UInt(1)
  }

  val nTests = 16
  val testData = Vec(Seq.tabulate(nTests)(i => UInt(i * 5, width = w)))

  val started = Reg(init = Bool(false))
  val sending = Reg(init = Bool(true))
  val receiving = Reg(init = Bool(true))

  val (sendIdx, sendDone) = Counter(tx.io.fast.out.fire(), nTests)
  val (recvIdx, recvDone) = Counter(tx.io.fast.in.fire(), nTests)

  tx.io.fast.out.valid := started && sending && send_delay === UInt(0)
  tx.io.fast.out.bits := testData(sendIdx)
  tx.io.fast.in.ready := started && receiving

  when (tx.io.fast.out.fire()) {
    send_delay := delay_rand
  } .elsewhen (send_delay =/= UInt(0)) {
    send_delay := send_delay - UInt(1)
  }

  when (!started && io.start) { started := Bool(true) }
  when (sendDone) { sending := Bool(false) }
  when (recvDone) { receiving := Bool(false) }

  assert(!tx.io.fast.in.valid || tx.io.fast.in.bits === testData(recvIdx),
    "SlowIOUnitTest: data does not match")

  io.finished := !sending && !receiving
}
