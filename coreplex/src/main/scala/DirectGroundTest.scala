package coreplex

import Chisel._
import cde.{Parameters, Field}
import groundtest._
import uncore.tilelink._
import uncore.agents._

case object ExportGroundTestStatus extends Field[Boolean]

class DirectGroundTestCoreplex(topParams: Parameters) extends Coreplex()(topParams) {

  // Not using the debug 
  io.debug.req.ready := Bool(false)
  io.debug.resp.valid := Bool(false)

  require(!exportMMIO)
  require(!exportBus)
  require(nMemChannels == 1)
  require(nTiles == 1)

  val test = p(BuildGroundTest)(outermostParams.alterPartial({
    case GroundTestId => 0
    case CacheName => "L1D"
  }))
  require(test.io.cache.size == 0)
  require(test.io.mem.size == nBanksPerMemChannel)
  require(test.io.ptw.size == 0)

  when (test.io.status.finished) { stop() }

  val mem_ic = Module(new TileLinkMemoryInterconnect(
    nBanksPerMemChannel, nMemChannels)(outermostParams))

  mem_ic.io.in <> test.io.mem
  io.mem <> mem_ic.io.out

  if (p(ExportGroundTestStatus)) {
    val status = io.extra.asInstanceOf[GroundTestStatus]

    val s_running :: s_finished :: s_errored :: s_timeout :: Nil = Enum(Bits(), 4)
    val state = Reg(init = s_running)
    val error_code = Reg(status.error.bits)
    val timeout_code = Reg(status.timeout.bits)
    when (state === s_running) {
      when (test.io.status.finished) { state := s_finished }
      when (test.io.status.error.valid) {
        state := s_errored
        error_code := test.io.status.error.bits
      }
      when (test.io.status.timeout.valid) {
        state := s_timeout
        timeout_code := test.io.status.timeout.bits
      }
    }
    status.finished := (state === s_finished)
    status.error.valid := (state === s_errored)
    status.error.bits := error_code
    status.timeout.valid := (state === s_timeout)
    status.timeout.bits := timeout_code
  }
}
