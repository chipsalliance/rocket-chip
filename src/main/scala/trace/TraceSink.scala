package freechips.rocketchip.trace

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.prci._
import freechips.rocketchip.subsystem._

abstract class LazyTraceSink()(implicit p: Parameters) extends LazyModule {
  val module: LazyTraceSinkModuleImp
}

class LazyTraceSinkModuleImp(outer: LazyTraceSink) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val trace_in = Flipped(Decoupled(UInt(8.W)))
  })
  io := DontCare
}