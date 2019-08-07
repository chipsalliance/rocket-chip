// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

import freechips.rocketchip.unittest._

class GatherTest(size: Int, timeout: Int = 500000) extends UnitTest(timeout) {
  val bits = log2Ceil(size+1)
  val mask = RegInit(0.U(size.W))
  io.finished := mask.andR
  mask := mask + !io.finished

  // Put 0, 1, 2, 3 ... at all of the lanes with mask=1
  val sum = RipplePrefixSum(0.U(bits.W) +: mask.asBools.map { x => WireInit(UInt(bits.W), x) })(_+_)
  val input = Wire(Vec(size, Valid(UInt(bits.W))))
  for (i <- 0 until size) {
    input(i).valid := mask(i)
    input(i).bits := Mux(mask(i), sum(i), sum.last)
  }

  val output = Gather(input)
  val total = PopCount(mask)
  for (i <- 0 until size) assert (i.U >= total || output(i) === i.U)
}

class ScatterTest(size: Int, timeout: Int = 500000) extends UnitTest(timeout) {
  val bits = log2Ceil(size+1)
  val mask = RegInit(0.U(size.W))
  io.finished := mask.andR
  mask := mask + !io.finished

  val input = Wire(Vec(size, Valid(UInt(bits.W))))
  for (i <- 0 until size) {
    input(i).valid := mask(i)
    input(i).bits := i.U
  }

  val output = Scatter(input)
  val sum = RipplePrefixSum(0.U(bits.W) +: mask.asBools.map { x => WireInit(UInt(bits.W), x) })(_+_)
  for (i <- 0 until size) assert (!mask(i) || output(i) === sum(i))
}
