// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

class IDPool(numIds: Int, lateValid: Boolean = false, revocableSelect: Boolean = false) extends Module {
  require (numIds > 0)
  val idWidth = log2Up(numIds)

  val io = IO(new Bundle {
    val free = Flipped(Valid(UInt(idWidth.W)))
    val alloc = if (revocableSelect) Decoupled(UInt(idWidth.W)) else Irrevocable(UInt(idWidth.W))
  })

  // True indicates that the id is available
  val bitmap = RegInit(UInt(numIds.W), -1.S(numIds.W).asUInt)
  val select = RegInit(0.U(idWidth.W))
  val valid  = RegInit(true.B)

  io.alloc.valid := (if (lateValid)       bitmap.orR              else valid)
  io.alloc.bits  := (if (revocableSelect) PriorityEncoder(bitmap) else select)

  val taken  = Mux(io.alloc.ready, UIntToOH(io.alloc.bits, numIds), 0.U)
  val allocated = Mux(io.free .valid, UIntToOH(io.free .bits, numIds), 0.U)
  val bitmap1 = (bitmap & ~taken) | allocated
  val select1 = PriorityEncoder(bitmap1)
  val valid1  = (  (bitmap.orR && !((PopCount(bitmap) === 1.U) && io.alloc.ready))  // bitmap not zero, and not allocating last bit
                || io.free.valid)

  // Clock gate the bitmap
  when (io.alloc.ready || io.free.valid) {
    bitmap := bitmap1
    valid  := valid1
  }

  // Make select irrevocable
  when (io.alloc.ready || (!io.alloc.valid && io.free.valid)) {
    select := select1
  }

  // No double freeing
  assert (!io.free.valid || !(bitmap & ~taken)(io.free.bits))

  // pre-calculations for timing
  if (!lateValid) {
    assert (valid === bitmap.orR)
  }
  if (!revocableSelect) {
    when (io.alloc.valid && RegNext(io.alloc.ready || (!io.alloc.valid && io.free.valid))) {
      assert (select === PriorityEncoder(bitmap))
    }
  }
}
