package freechips.rocketchip.trace

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.prci._
import freechips.rocketchip.subsystem._

class WithTraceSink(targetId: Int = 0) extends Config((site, here, up) => {  
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => {
      assert(tp.tileParams.ltrace.isDefined, "TraceSink must be used with a tile that has a trace encoder")
      assert(!tp.tileParams.ltrace.get.sinks.contains(targetId), "This targetId is already in use")
      tp.copy(tileParams = tp.tileParams.copy(
      ltrace = Some(tp.tileParams.ltrace.get.copy(sinks = tp.tileParams.ltrace.get.sinks :+ targetId))))
    }
    case other => other
  }
})

abstract class LazyTraceSink()(implicit p: Parameters) extends LazyModule {
  val module: LazyTraceSinkModuleImp
}

class LazyTraceSinkModuleImp(outer: LazyTraceSink) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val trace_in = Flipped(Decoupled(UInt(8.W)))
  })
  io := DontCare
}