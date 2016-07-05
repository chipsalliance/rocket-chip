// Angie modified

package uncore

import Chisel._
import junctions.SmiIO
import cde.Parameters
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class SmiExample(implicit p: Parameters) extends HtifModule()(p) {
  // SMI boilerplate
  val io = new Bundle {
    val smi = new SmiIO(64, 10).flip
    val iobit = Bool(OUTPUT)
  }
  
  val resp_valid = Reg(init = Bool(false))
  when (io.smi.resp.fire()) {
    resp_valid := Bool(false)
  }

  val read_addr = Reg(init = UInt(width=10))
  when (io.smi.req.fire()) {
    read_addr := io.smi.req.bits.addr
    resp_valid := Bool(true)
  }

  io.smi.req.ready := !resp_valid
  io.smi.resp.valid := resp_valid

  // Whatever you want to do in SMI
  val mem = Reg(Vec(1024, UInt(width = 64)))
  when (io.smi.req.fire() && io.smi.req.bits.rw) {
    mem(io.smi.req.bits.addr) := ~io.smi.req.bits.data
  }
  io.smi.resp.bits := mem(read_addr)

  printf("mem %d", mem(0))
  io.iobit := mem(0)(0)
}
