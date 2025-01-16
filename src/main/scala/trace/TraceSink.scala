package freechips.rocketchip.trace

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.prci._
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc}

trait HasTraceSinkIO {
  val io = IO(new Bundle {
    val trace_in = Flipped(Decoupled(UInt(8.W)))
  })
}
