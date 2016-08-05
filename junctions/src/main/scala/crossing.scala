package junctions
import Chisel._

class Crossing[T <: Data](gen: T, enq_sync: Boolean, deq_sync: Boolean) extends Bundle {
    val enq = Decoupled(gen).flip()
    val deq = Decoupled(gen)
    val enq_clock = if (enq_sync) Some(Clock(INPUT)) else None
    val deq_clock = if (deq_sync) Some(Clock(INPUT)) else None
    val enq_reset = if (enq_sync) Some(Bool(INPUT))  else None
    val deq_reset = if (deq_sync) Some(Bool(INPUT))  else None
}

// Output is 1 for one cycle after any edge of 'in'
object AsyncHandshakePulse {
  def apply(in: Bool, sync: Int): Bool = {
    val syncv = RegInit(Vec.fill(sync+1){Bool(false)})
    syncv.last := in
    (syncv.init zip syncv.tail).foreach { case (sink, source) => sink := source }
    syncv(0) =/= syncv(1)
  }
}

class AsyncHandshakeSource[T <: Data](gen: T, sync: Int, clock: Clock, reset: Bool)
    extends Module(_clock = clock, _reset = reset) {
  val io = new Bundle {
    // These come from the source clock domain
    val enq  = Decoupled(gen).flip()
    // These cross to the sink clock domain
    val bits = gen.cloneType.asOutput
    val push = Bool(OUTPUT)
    val pop  = Bool(INPUT)
  }

  val ready = RegInit(Bool(true))
  val bits = Reg(gen)
  val push = RegInit(Bool(false))

  io.enq.ready := ready
  io.bits := bits
  io.push := push

  val pop = AsyncHandshakePulse(io.pop, sync)
  assert (!pop || !ready)

  when (pop) {
    ready := Bool(true)
  }

  when (io.enq.fire()) {
    ready := Bool(false)
    bits := io.enq.bits
    push := !push
  }
}

class AsyncHandshakeSink[T <: Data](gen: T, sync: Int, clock: Clock, reset: Bool) 
    extends Module(_clock = clock, _reset = reset) {
  val io = new Bundle {
    // These cross to the source clock domain
    val bits = gen.cloneType.asInput
    val push = Bool(INPUT)
    val pop  = Bool(OUTPUT)
    // These go to the sink clock domain
    val deq = Decoupled(gen)
  }

  val valid = RegInit(Bool(false))
  val bits  = Reg(gen)
  val pop   = RegInit(Bool(false))

  io.deq.valid := valid
  io.deq.bits  := bits
  io.pop := pop

  val push = AsyncHandshakePulse(io.push, sync)
  assert (!push || !valid)

  when (push) {
    valid := Bool(true)
    bits  := io.bits
  }

  when (io.deq.fire()) {
    valid := Bool(false)
    pop := !pop
  }
}

class AsyncHandshake[T <: Data](gen: T, sync: Int = 2) extends Module {
  val io = new Crossing(gen, true, true)
  require (sync >= 2)

  val source = Module(new AsyncHandshakeSource(gen, sync, io.enq_clock.get, io.enq_reset.get))
  val sink   = Module(new AsyncHandshakeSink  (gen, sync, io.deq_clock.get, io.deq_reset.get))

  source.io.enq <> io.enq
  io.deq <> sink.io.deq

  sink.io.bits := source.io.bits
  sink.io.push := source.io.push
  source.io.pop := sink.io.pop
}

class AsyncDecoupledTo[T <: Data](gen: T, depth: Int = 0, sync: Int = 2) extends Module {
  val io = new Crossing(gen, false, true)

  // !!! if depth == 0 { use Handshake } else { use AsyncFIFO }
  val crossing = Module(new AsyncHandshake(gen, sync)).io
  crossing.enq_clock.get := clock
  crossing.enq_reset.get := reset
  crossing.enq <> io.enq
  crossing.deq_clock.get := io.deq_clock.get
  crossing.deq_reset.get := io.deq_reset.get
  io.deq <> crossing.deq
}

object AsyncDecoupledTo {
  // source is in our clock domain, output is in the 'to' clock domain
  def apply[T <: Data](to_clock: Clock, to_reset: Bool, source: DecoupledIO[T], depth: Int = 0, sync: Int = 2): DecoupledIO[T] = {
    val to = Module(new AsyncDecoupledTo(source.bits, depth, sync))
    to.io.deq_clock.get := to_clock
    to.io.deq_reset.get := to_reset
    to.io.enq <> source
    to.io.deq
  }
}

class AsyncDecoupledFrom[T <: Data](gen: T, depth: Int = 0, sync: Int = 2) extends Module {
  val io = new Crossing(gen, true, false)

  // !!! if depth == 0 { use Handshake } else { use AsyncFIFO }
  val crossing = Module(new AsyncHandshake(gen, sync)).io
  crossing.enq_clock.get := io.enq_clock.get
  crossing.enq_reset.get := io.enq_reset.get
  crossing.enq <> io.enq
  crossing.deq_clock.get := clock
  crossing.deq_reset.get := reset
  io.deq <> crossing.deq
}

object AsyncDecoupledFrom {
  // source is in the 'from' clock domain, output is in our clock domain
  def apply[T <: Data](from_clock: Clock, from_reset: Bool, source: DecoupledIO[T], depth: Int = 0, sync: Int = 2): DecoupledIO[T] = {
    val from = Module(new AsyncDecoupledFrom(source.bits, depth, sync))
    from.io.enq_clock.get := from_clock
    from.io.enq_reset.get := from_reset
    from.io.enq <> source
    from.io.deq
  }
}
