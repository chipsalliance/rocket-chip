package freechips.rocketchip.trace

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import org.chipsalliance.cde.config.{Parameters, Config, Field}
import freechips.rocketchip.tile._
import freechips.rocketchip.subsystem._
case object TraceSinkAlwaysKey extends Field[Option[Int]](None)

class TraceSinkAlways()(implicit p: Parameters) extends LazyTraceSink {
  override lazy val module = new TraceSinkAlwaysImpl(this)
  class TraceSinkAlwaysImpl(outer: TraceSinkAlways) extends LazyTraceSinkModuleImp(outer) {
    io.trace_in.ready := true.B
  }
}

class WithTraceSinkAlways(targetId: Int = 0) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: RocketTileAttachParams => {
      tp.copy(tileParams = tp.tileParams.copy(
        traceParams = Some(tp.tileParams.traceParams.get.copy(buildSinks = 
          tp.tileParams.traceParams.get.buildSinks :+ (p => (LazyModule(new TraceSinkAlways()(p)), targetId)))))
      )
    }
    case other => other
  }
})