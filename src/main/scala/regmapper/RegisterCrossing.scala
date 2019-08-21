// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import Chisel._
import chisel3.util.{Irrevocable}

import freechips.rocketchip.util.{AsyncQueue,AsyncQueueParams,AsyncResetRegVec}

// A very simple flow control state machine, run in the specified clock domain
class BusyRegisterCrossing extends Module {
  val io = new Bundle {
    val bypass                  = Bool(INPUT)
    val master_request_valid    = Bool(INPUT)
    val master_request_ready    = Bool(OUTPUT)
    val master_response_valid   = Bool(OUTPUT)
    val master_response_ready   = Bool(INPUT)
    val crossing_request_valid  = Bool(OUTPUT)
    val crossing_request_ready  = Bool(INPUT)
    // ... no crossing_response_ready; we are always ready
  }

  val busy   = RegInit(Bool(false))
  val bypass = Reg(Bool())

  when (io.crossing_request_ready || Mux(busy, bypass, io.bypass)) {
    busy := Mux(busy, !io.master_response_ready, io.master_request_valid)
  }

  when (io.master_request_valid && io.master_request_ready) {
    bypass := io.bypass
  }

  io.crossing_request_valid := io.master_request_valid && !io.bypass && !busy
  io.master_request_ready   := (io.bypass || io.crossing_request_ready) && !busy
  io.master_response_valid  := (bypass    || io.crossing_request_ready) &&  busy
}

class RegisterCrossingAssertion extends Module {
  val io = new Bundle {
    val master_bypass = Bool(INPUT)
    val slave_reset = Bool(INPUT)
  }

  val up = RegInit(Bool(false))
  up := !io.slave_reset

  assert (io.master_bypass || !up || !io.slave_reset)
}

// RegField should support connecting to one of these
class RegisterWriteIO[T <: Data](gen: T) extends Bundle {
  val request  = Decoupled(gen).flip
  val response = Irrevocable(Bool()) // ignore .bits

  override def cloneType = new RegisterWriteIO(gen).asInstanceOf[this.type]
}

// To turn off=>on a domain:
//  A. To turn disable the master domain
//    1. wait for all inflight traffic to resolve
//    2. assert master reset
//    3. (optional) stop the master clock
//    --- YOU MAY NOT TURN OFF POWER ---
//    4. re-enable the clock
//    5. deassert reset
//  B. To turn off the slave domain
//    1. assert bypass
//    2. wait for inflight traffic to resolve
//    3. assert slave reset
//    4. (optional) stop the slave clock
//    --- YOU MAY NOT TURN OFF POWER ---
//    5. re-enable the clock
//    6. deassert reset
//    7. deassert bypass
//
// If you need to cut power, use something that support isolation gates.

class RegisterWriteCrossingIO[T <: Data](gen: T) extends Bundle {
  // Master clock domain
  val master_clock   = Clock(INPUT)
  val master_reset   = Bool(INPUT)
  val master_port    = new RegisterWriteIO(gen)
  // Bypass requests from the master to be noops
  val master_bypass  = Bool(INPUT)
  // Slave clock domain
  val slave_clock    = Clock(INPUT)
  val slave_reset    = Bool(INPUT)
  val slave_register = gen.asOutput
  val slave_valid    = Bool(OUTPUT) // is high on 1st cycle slave_register has a new value
}

class RegisterWriteCrossing[T <: Data](gen: T, sync: Int = 3) extends Module {
  val io = new RegisterWriteCrossingIO(gen)
  // The crossing must only allow one item inflight at a time
  val control = Module(new BusyRegisterCrossing)
  val crossing = Module(new AsyncQueue(gen, AsyncQueueParams.singleton(sync)))

  control.clock := io.master_clock
  control.reset := io.master_reset
  crossing.io.enq_clock := io.master_clock
  crossing.io.enq_reset := io.master_reset
  crossing.io.deq_clock := io.slave_clock
  crossing.io.deq_reset := io.slave_reset

  control.io.bypass := io.master_bypass
  control.io.master_request_valid  := io.master_port.request.valid
  control.io.master_response_ready := io.master_port.response.ready
  io.master_port.request.ready  := control.io.master_request_ready
  io.master_port.response.valid := control.io.master_response_valid

  control.io.crossing_request_ready := crossing.io.enq.ready
  crossing.io.enq.valid := control.io.crossing_request_valid
  crossing.io.enq.bits := io.master_port.request.bits

  crossing.io.deq.ready := Bool(true)
  io.slave_valid := crossing.io.deq.valid
  io.slave_register := crossing.io.deq.bits

  val assertion = Module(new RegisterCrossingAssertion)
  assertion.clock := io.master_clock
  assertion.reset := io.master_reset
  assertion.io.master_bypass := io.master_bypass
  assertion.io.slave_reset := io.slave_reset
}

// RegField should support connecting to one of these
class RegisterReadIO[T <: Data](gen: T) extends Bundle {
  val request  = Decoupled(Bool()).flip // ignore .bits
  val response = Irrevocable(gen)

  override def cloneType = new RegisterReadIO(gen).asInstanceOf[this.type]
}

class RegisterReadCrossingIO[T <: Data](gen: T) extends Bundle {
  // Master clock domain
  val master_clock   = Clock(INPUT)
  val master_reset   = Bool(INPUT)
  val master_port    = new RegisterReadIO(gen)
  // Bypass requests from the master to be noops
  val master_bypass  = Bool(INPUT)
  // Slave clock domain
  val slave_clock    = Clock(INPUT)
  val slave_reset    = Bool(INPUT)
  val slave_register = gen.asInput
}

class RegisterReadCrossing[T <: Data](gen: T, sync: Int = 3) extends Module {
  val io = new RegisterReadCrossingIO(gen)
  // The crossing must only allow one item inflight at a time
  val control = Module(new BusyRegisterCrossing)
  val crossing = Module(new AsyncQueue(gen, AsyncQueueParams.singleton(sync)))

  control.clock := io.master_clock
  control.reset := io.master_reset
  crossing.io.deq_clock := io.master_clock
  crossing.io.deq_reset := io.master_reset
  crossing.io.enq_clock := io.slave_clock
  crossing.io.enq_reset := io.slave_reset

  control.io.bypass := io.master_bypass
  control.io.master_request_valid  := io.master_port.request.valid
  control.io.master_response_ready := io.master_port.response.ready
  io.master_port.request.ready  := control.io.master_request_ready
  io.master_port.response.valid := control.io.master_response_valid

  control.io.crossing_request_ready := crossing.io.deq.valid
  crossing.io.deq.ready := control.io.crossing_request_valid
  io.master_port.response.bits := crossing.io.deq.bits

  crossing.io.enq.valid := Bool(true)
  crossing.io.enq.bits := io.slave_register

  val assertion = Module(new RegisterCrossingAssertion)
  assertion.clock := io.master_clock
  assertion.reset := io.master_reset
  assertion.io.master_bypass := io.master_bypass
  assertion.io.slave_reset := io.slave_reset
}

/** Wrapper to create an
  *  asynchronously reset slave register which can be 
  *  both read and written 
  *  using crossing FIFOs.
  *  The reset and allow assertion & de-assertion 
  *  should be synchronous to their respective 
  *  domains. 
  */

object AsyncRWSlaveRegField {

  def apply(
    master_clock: Clock,
    master_reset: Bool,
    slave_clock: Clock,
    slave_reset: Bool,
    width:  Int,
    init: Int,
    name: Option[String] = None,
    master_bypass: Bool = Bool(true),
    desc: Option[RegFieldDesc] = None
  ): (UInt, RegField) = {

    val async_slave_reg = Module(new AsyncResetRegVec(width, init))
    name.foreach(async_slave_reg.suggestName(_))
    async_slave_reg.reset := slave_reset
    async_slave_reg.clock := slave_clock

    val wr_crossing = Module (new RegisterWriteCrossing(UInt(width = width)))
    name.foreach(n => wr_crossing.suggestName(s"${n}_wcrossing"))

    wr_crossing.io.master_clock  := master_clock
    wr_crossing.io.master_reset  := master_reset
    wr_crossing.io.master_bypass := master_bypass
    wr_crossing.io.slave_clock   := slave_clock
    wr_crossing.io.slave_reset   := slave_reset

    async_slave_reg.io.en := wr_crossing.io.slave_valid
    async_slave_reg.io.d  := wr_crossing.io.slave_register

    val rd_crossing = Module (new RegisterReadCrossing(UInt(width = width )))
    name.foreach(n => rd_crossing.suggestName(s"${n}_rcrossing"))

    rd_crossing.io.master_clock  := master_clock
    rd_crossing.io.master_reset  := master_reset
    rd_crossing.io.master_bypass := master_bypass
    rd_crossing.io.slave_clock   := slave_clock
    rd_crossing.io.slave_reset   := slave_reset

    rd_crossing.io.slave_register := async_slave_reg.io.q

    (async_slave_reg.io.q, RegField(width, rd_crossing.io.master_port, wr_crossing.io.master_port, desc))
  }
}
