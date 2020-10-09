// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

class IDPool(numIds: Int) extends Module {
  require (numIds > 0)
  val idWidth = log2Up(numIds)

  val io = IO(new Bundle {
    val free = Flipped(Valid(UInt(idWidth.W)))
    val alloc = Irrevocable(UInt(idWidth.W))
  })

  // True indicates that the id is available
  val bitmap = RegInit(-1.S(numIds.W).asUInt)
  val select = RegInit(0.U(idWidth.W))
  val valid  = RegInit(true.B)

  io.alloc.valid := valid
  io.alloc.bits  := select

  val taken  = Mux(io.alloc.ready, (1.U << io.alloc.bits)(numIds-1, 0), 0.U)
  val given  = Mux(io.free .valid, (1.U << io.free .bits)(numIds-1, 0), 0.U)
  val bitmap1 = (bitmap & ~taken) | given
  val select1 = OHToUInt(~(leftOR(bitmap1, numIds) << 1) & bitmap1, numIds)
  val valid1  = bitmap1.orR

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
}
