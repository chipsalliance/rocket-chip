package freechips.rocketchip.trace

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import org.chipsalliance.cde.config.{Parameters, Config, Field}
import freechips.rocketchip.tile._
import freechips.rocketchip.subsystem._
case object TraceSinkAlwaysKey extends Field[Option[Int]](None)

class TraceSinkAlways()(implicit p: Parameters) extends LazyModule {
  println(s"TraceSinkAlways Lazy Init}")
  override lazy val module = new TraceSinkAlwaysImpl(this)
  class TraceSinkAlwaysImpl(outer: TraceSinkAlways) extends LazyModuleImp(outer) {
    println(s"TraceSinkAlways Impl Init")
    val io = IO(new Bundle {
      val trace_in = Flipped(Decoupled(UInt(8.W)))
    })
    withClockAndReset(clock, reset) {
      io.trace_in.ready := true.B
    }
  }
}

class WithTraceSinkAlways(targetId: Int = 0) extends Config(
  (new WithTraceSink(targetId)).alter((site, here, up) => {
    case TraceSinkAlwaysKey => Some(targetId)
  })
)

trait CanHaveTraceSinkAlways {this: BaseSubsystem with InstantiatesHierarchicalElements =>
  val traceSinkAlways = p(TraceSinkAlwaysKey) match {
    case Some(targetId) => {
      val arbs = totalTiles.values.map { t => t match {
        case r: RocketTile => List((t, r.trace_sink_arbiter.get))
        case _ => Nil
      }}.flatten

      arbs.map { case (t, arb) =>
        t { // in the implicit clock domain of tile
          val traceSinkAlways = LazyModule(new TraceSinkAlways()(p))
          val index = t.asInstanceOf[RocketTile].rocketParams.ltrace.get.sinks.indexOf(targetId)
          println(s"What? $index")
          InModuleBody {
            println(s"TraceSinkAlways In Module Body")
            traceSinkAlways.module.io.trace_in <> arb.module.io.out(index)
          }
        }
      }
    }
    case _ => None
  }
}