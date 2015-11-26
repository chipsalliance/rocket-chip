package uncore

import Chisel._
import junctions._
import cde.{Parameters, Field}

class NastiROM(contents: Seq[Byte])(implicit p: Parameters) extends Module {
  val io = new NastiIO().flip
  val ar = Queue(io.ar, 1)

  // This assumes ROMs are in read-only parts of the address map.
  // Reuse b_queue code from NastiErrorSlave if this assumption is bad.
  when (ar.valid) { assert(ar.bits.len === UInt(0), "Can't burst-read from NastiROM") }
  assert(!(io.aw.valid || io.w.valid), "Can't write to NastiROM")
  io.aw.ready := Bool(false)
  io.w.ready := Bool(false)
  io.b.valid := Bool(false)

  val byteWidth = io.r.bits.nastiXDataBits / 8
  val rows = (contents.size + byteWidth - 1)/byteWidth + 1
  val rom = Vec.tabulate(rows) { i =>
    val slice = contents.slice(i*byteWidth, (i+1)*byteWidth)
    UInt(slice.foldRight(BigInt(0)) { case (x,y) => (y << 8) + (x.toInt & 0xFF) })
  }
  val rdata_word = rom(if (rows == 1) UInt(0) else ar.bits.addr(log2Up(contents.size)-1,log2Up(byteWidth)))
  val rdata = new LoadGen(Cat(UInt(1), ar.bits.size), ar.bits.addr, rdata_word, Bool(false), byteWidth).data

  io.r <> ar
  io.r.bits := NastiReadDataChannel(ar.bits.id, rdata)
}
