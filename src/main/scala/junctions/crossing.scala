package junctions
import Chisel._

class Crossing[T <: Data](gen: T) extends Bundle {
    val enq = Decoupled(gen).flip()
    val deq = Decoupled(gen)
    val enq_clock = Clock(INPUT)
    val deq_clock = Clock(INPUT)
    val enq_reset = Bool(INPUT)
    val deq_reset = Bool(INPUT)
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
  val io = new Crossing(gen)
  require (sync >= 2)

  val source = Module(new AsyncHandshakeSource(gen, sync, io.enq_clock, io.enq_reset))
  val sink   = Module(new AsyncHandshakeSink  (gen, sync, io.deq_clock, io.deq_reset))

  source.io.enq <> io.enq
  io.deq <> sink.io.deq

  sink.io.bits := source.io.bits
  sink.io.push := source.io.push
  source.io.pop := sink.io.pop
}

class AsyncScope extends Module { val io = new Bundle }
object AsyncScope { def apply() = Module(new AsyncScope) }

object AsyncDecoupledCrossing
{
  // takes from_source from the 'from' clock domain and puts it into the 'to' clock domain
  def apply[T <: Data](from_clock: Clock, from_reset: Bool, from_source: DecoupledIO[T], to_clock: Clock, to_reset: Bool, depth: Int = 3, sync: Int = 2): DecoupledIO[T] = {
    // !!! if depth == 0 { use Handshake } else { use AsyncFIFO }
    val crossing = Module(new AsyncHandshake(from_source.bits, sync)).io
    crossing.enq_clock := from_clock
    crossing.enq_reset := from_reset
    crossing.enq       <> from_source
    crossing.deq_clock := to_clock
    crossing.deq_reset := to_reset
    crossing.deq
  }
}

object AsyncDecoupledTo
{
  // takes source from your clock domain and puts it into the 'to' clock domain
  def apply[T <: Data](to_clock: Clock, to_reset: Bool, source: DecoupledIO[T], depth: Int = 3, sync: Int = 2): DecoupledIO[T] = {
    val scope = AsyncScope()
    AsyncDecoupledCrossing(scope.clock, scope.reset, source, to_clock, to_reset, depth, sync)
  }
}

object AsyncDecoupledFrom
{
  // takes from_source from the 'from' clock domain and puts it into your clock domain
  def apply[T <: Data](from_clock: Clock, from_reset: Bool, from_source: DecoupledIO[T], depth: Int = 3, sync: Int = 2): DecoupledIO[T] = {
    val scope = AsyncScope()
    AsyncDecoupledCrossing(from_clock, from_reset, from_source, scope.clock, scope.reset, depth, sync)
  }
}
