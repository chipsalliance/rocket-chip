package uncore.test

//import Chisel._
import chisel3._
import chisel3.util._
import junctions._
import uncore.test._
import diplomacy._

object RandomClockGenerator {
//  val io = new Bundle {
//    val clock_out = Clock(OUTPUT)
//  }

  def apply(reset:Bool) : Clock = {
    val clock_transaction = NoiseMaker(true, 1, Bool(true)).toBool
    val clock_reg = Reg(Bool())
    when (clock_transaction || reset) {
      clock_reg := ~clock_reg
    }  
    clock_reg.asClock
  }
}

class AsyncQueueFormalTest[T <: Data](gen: T, depth: Int=1, sync: Int=2) extends Module {
  val io = new Bundle {}
  
  val in_reset = Wire(Bool(INPUT), reset)
  val in_clock = Wire(Clock(), RandomClockGenerator(in_reset))
  val in_data = Wire(new IrrevocableIO(gen).flip)
  val out_reset = Wire(Bool(INPUT), reset)
  val out_clock = Wire(Clock(), RandomClockGenerator(out_reset))
  val out_data = Wire(new IrrevocableIO(gen))

  val scoreboard = new FormalScoreboard(gen)

  class SourceScoreboard(clock_in:Clock) extends Module(_clock=clock_in) {
    val io = new Bundle {
      val out_data = new IrrevocableIO(gen)
      val nd_value1 = scoreboard.nd_value1.asInput
      val nd_value2 = scoreboard.nd_value2.asInput
    }
    io.out_data.valid := NoiseMaker(true, 1, Bool(true)).toBool
    io.out_data.bits := scoreboard.generator(io.out_data.fire(), io.nd_value1, io.nd_value2)
  }

  val data_source = Module(new SourceScoreboard(in_clock))
  in_data <> data_source.io.out_data
  data_source.io.nd_value1 := scoreboard.nd_value1
  data_source.io.nd_value2 := scoreboard.nd_value2
  
//  out_data <> AsyncIrrevocableCrossing(in_clock, in_reset, in_data, out_clock, out_reset, depth, sync)
  
  class SinkScoreboard (clock_in:Clock) extends Module(_clock=clock_in) {
    val io = new Bundle {
      val in_data = new IrrevocableIO(gen).flip
      val nd_value1 = scoreboard.nd_value1.asInput
      val nd_value2 = scoreboard.nd_value2.asInput
    }

    io.in_data.ready := NoiseMaker(true, 1, Bool(true)).toBool
    scoreboard.checker(io.in_data.bits, io.in_data.fire(), io.nd_value1, io.nd_value2)
  }
  
  val data_sink = Module(new SinkScoreboard(out_clock))
  data_sink.io.in_data <> out_data
  data_sink.io.nd_value1 := scoreboard.nd_value1
  data_sink.io.nd_value2 := scoreboard.nd_value2
}

object AsyncQueueFormalTest extends App {
  import chisel3.internal.firrtl.Circuit
  import java.io.File
  
  val circuit = Driver.elaborate(()=>new AsyncQueueFormalTest(UInt(width=2), depth=4))
  Driver.dumpFirrtl(circuit, Some(new File("FormalTester.fir")))

}