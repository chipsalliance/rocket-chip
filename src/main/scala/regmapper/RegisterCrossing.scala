// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import chisel3._
import chisel3.util.{Decoupled, Irrevocable}

import freechips.rocketchip.util.{AsyncQueue,AsyncQueueParams,AsyncResetRegVec}

// A very simple flow control state machine, run in the specified clock domain
class BusyRegisterCrossing extends Module {
  val io = IO(new Bundle {
    val bypass                  = Input(Bool())
    val client_request_valid    = Input(Bool())
    val client_request_ready    = Output(Bool())
    val client_response_valid   = Output(Bool())
    val client_response_ready   = Input(Bool())
    val crossing_request_valid  = Output(Bool())
    val crossing_request_ready  = Input(Bool())
    // ... no crossing_response_ready; we are always ready
  })

  val busy   = RegInit(false.B)
  val bypass = Reg(Bool())

  when (io.crossing_request_ready || Mux(busy, bypass, io.bypass)) {
    busy := Mux(busy, !io.client_response_ready, io.client_request_valid)
  }

  when (io.client_request_valid && io.client_request_ready) {
    bypass := io.bypass
  }

  io.crossing_request_valid := io.client_request_valid && !io.bypass && !busy
  io.client_request_ready   := (io.bypass || io.crossing_request_ready) && !busy
  io.client_response_valid  := (bypass    || io.crossing_request_ready) &&  busy
}

class RegisterCrossingAssertion extends Module {
  val io = IO(new Bundle {
    val client_bypass = Input(Bool())
    val manager_reset = Input(Bool())
  })

  val up = RegInit(false.B)
  up := !io.manager_reset

  assert (io.client_bypass || !up || !io.manager_reset)
}

// RegField should support connecting to one of these
class RegisterWriteIO[T <: Data](gen: T) extends Bundle {
  val request  = Flipped(Decoupled(gen))
  val response = Irrevocable(Bool()) // ignore .bits
}

// To turn off=>on a domain:
//  A. To turn disable the client domain
//    1. wait for all inflight traffic to resolve
//    2. assert client reset
//    3. (optional) stop the client clock
//    --- YOU MAY NOT TURN OFF POWER ---
//    4. re-enable the clock
//    5. deassert reset
//  B. To turn off the manager domain
//    1. assert bypass
//    2. wait for inflight traffic to resolve
//    3. assert manager reset
//    4. (optional) stop the manager clock
//    --- YOU MAY NOT TURN OFF POWER ---
//    5. re-enable the clock
//    6. deassert reset
//    7. deassert bypass
//
// If you need to cut power, use something that support isolation gates.

class RegisterWriteCrossingIO[T <: Data](gen: T) extends Bundle {
  // client clock domain
  val client_clock   = Input(Clock())
  val client_reset   = Input(Bool())
  val client_port    = new RegisterWriteIO(gen)
  // Bypass requests from the client to be noops
  val client_bypass  = Input(Bool())
  // manager clock domain
  val manager_clock    = Input(Clock())
  val manager_reset    = Input(Bool())
  val manager_register = Output(gen)
  val manager_valid    = Output(Bool()) // is high on 1st cycle manager_register has a new value
}

class RegisterWriteCrossing[T <: Data](gen: T, sync: Int = 3) extends Module {
  val io = IO(new RegisterWriteCrossingIO(gen))
  // The crossing must only allow one item inflight at a time
  val control = Module(new BusyRegisterCrossing)
  val crossing = Module(new AsyncQueue(gen, AsyncQueueParams.singleton(sync)))

  control.clock := io.client_clock
  control.reset := io.client_reset
  crossing.io.enq_clock := io.client_clock
  crossing.io.enq_reset := io.client_reset
  crossing.io.deq_clock := io.manager_clock
  crossing.io.deq_reset := io.manager_reset

  control.io.bypass := io.client_bypass
  control.io.client_request_valid  := io.client_port.request.valid
  control.io.client_response_ready := io.client_port.response.ready
  io.client_port.request.ready  := control.io.client_request_ready
  io.client_port.response.valid := control.io.client_response_valid
  io.client_port.response.bits  := DontCare

  control.io.crossing_request_ready := crossing.io.enq.ready
  crossing.io.enq.valid := control.io.crossing_request_valid
  crossing.io.enq.bits := io.client_port.request.bits

  crossing.io.deq.ready := true.B
  io.manager_valid := crossing.io.deq.valid
  io.manager_register := crossing.io.deq.bits

  val assertion = Module(new RegisterCrossingAssertion)
  assertion.clock := io.client_clock
  assertion.reset := io.client_reset
  assertion.io.client_bypass := io.client_bypass
  assertion.io.manager_reset := io.manager_reset
}

// RegField should support connecting to one of these
class RegisterReadIO[T <: Data](gen: T) extends Bundle {
  val request  = Flipped(Decoupled(Bool())) // ignore .bits
  val response = Irrevocable(gen)

}

class RegisterReadCrossingIO[T <: Data](gen: T) extends Bundle {
  // client clock domain
  val client_clock   = Input(Clock())
  val client_reset   = Input(Bool())
  val client_port    = new RegisterReadIO(gen)
  // Bypass requests from the client to be noops
  val client_bypass  = Input(Bool())
  // manager clock domain
  val manager_clock    = Input(Clock())
  val manager_reset    = Input(Bool())
  val manager_register = Input(gen)
}

class RegisterReadCrossing[T <: Data](gen: T, sync: Int = 3) extends Module {
  val io = IO(new RegisterReadCrossingIO(gen))
  // The crossing must only allow one item inflight at a time
  val control = Module(new BusyRegisterCrossing)
  val crossing = Module(new AsyncQueue(gen, AsyncQueueParams.singleton(sync)))

  control.clock := io.client_clock
  control.reset := io.client_reset
  crossing.io.deq_clock := io.client_clock
  crossing.io.deq_reset := io.client_reset
  crossing.io.enq_clock := io.manager_clock
  crossing.io.enq_reset := io.manager_reset

  control.io.bypass := io.client_bypass
  control.io.client_request_valid  := io.client_port.request.valid
  control.io.client_response_ready := io.client_port.response.ready
  io.client_port.request.ready  := control.io.client_request_ready
  io.client_port.response.valid := control.io.client_response_valid

  control.io.crossing_request_ready := crossing.io.deq.valid
  crossing.io.deq.ready := control.io.crossing_request_valid
  io.client_port.response.bits := crossing.io.deq.bits

  crossing.io.enq.valid := true.B
  crossing.io.enq.bits := io.manager_register

  val assertion = Module(new RegisterCrossingAssertion)
  assertion.clock := io.client_clock
  assertion.reset := io.client_reset
  assertion.io.client_bypass := io.client_bypass
  assertion.io.manager_reset := io.manager_reset
}

/** Wrapper to create an
  *  asynchronously reset manager register which can be 
  *  both read and written 
  *  using crossing FIFOs.
  *  The reset and allow assertion & de-assertion 
  *  should be synchronous to their respective 
  *  domains. 
  */

object AsyncRWManagerRegField {

  def apply(
    client_clock: Clock,
    client_reset: Bool,
    manager_clock: Clock,
    manager_reset: Bool,
    width:  Int,
    init: Int,
    name: Option[String] = None,
    client_bypass: Bool = true.B,
    desc: Option[RegFieldDesc] = None
  ): (UInt, RegField) = {

    val async_manager_reg = Module(new AsyncResetRegVec(width, init))
    name.foreach(async_manager_reg.suggestName(_))
    async_manager_reg.reset := manager_reset
    async_manager_reg.clock := manager_clock

    val wr_crossing = Module (new RegisterWriteCrossing(UInt(width.W)))
    name.foreach(n => wr_crossing.suggestName(s"${n}_wcrossing"))

    wr_crossing.io.client_clock  := client_clock
    wr_crossing.io.client_reset  := client_reset
    wr_crossing.io.client_bypass := client_bypass
    wr_crossing.io.manager_clock   := manager_clock
    wr_crossing.io.manager_reset   := manager_reset

    async_manager_reg.io.en := wr_crossing.io.manager_valid
    async_manager_reg.io.d  := wr_crossing.io.manager_register

    val rd_crossing = Module (new RegisterReadCrossing(UInt(width.W)))
    name.foreach(n => rd_crossing.suggestName(s"${n}_rcrossing"))

    rd_crossing.io.client_clock  := client_clock
    rd_crossing.io.client_reset  := client_reset
    rd_crossing.io.client_bypass := client_bypass
    rd_crossing.io.manager_clock   := manager_clock
    rd_crossing.io.manager_reset   := manager_reset

    rd_crossing.io.manager_register := async_manager_reg.io.q

    (async_manager_reg.io.q, RegField(width, rd_crossing.io.client_port, wr_crossing.io.client_port, desc))
  }
}
