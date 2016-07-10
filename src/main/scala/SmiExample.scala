// Public Domain Code, from RISC-V Chisel Tutorial (July 2016)

package rocketchip

import Chisel._
import junctions.SmiIO
import cde.Parameters
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class SmiExample(implicit p: Parameters) extends Module {
  // SMI boilerplate
  val io = new Bundle {
    val smi = new SmiIO(64, p(SmiExampleAddrBits)).flip
    val iobit = Bool(OUTPUT)
  }
  
  val resp_valid = Reg(init = Bool(false))
  when (io.smi.resp.fire()) {
    resp_valid := Bool(false)
  }

  val read_addr = Reg(UInt(width=10))
  when (io.smi.req.fire()) {
    read_addr := io.smi.req.bits.addr
    resp_valid := Bool(true)
  }

  io.smi.req.ready := !resp_valid
  io.smi.resp.valid := resp_valid

  // Whatever you want to do in SMI
  val enabled = Reg(init = Bool(false))
  val period  = Reg(UInt(width = 64))
  val duty    = Reg(UInt(width = 64))

  when (io.smi.req.fire() && io.smi.req.bits.rw) {
    when (io.smi.req.bits.addr === UInt(0)) {
      enabled := io.smi.req.bits.data
    }
    when (io.smi.req.bits.addr === UInt(1)) {
      period := io.smi.req.bits.data
    }
    when (io.smi.req.bits.addr === UInt(2)) {
      duty := io.smi.req.bits.data
    }
  }
  when(io.smi.req.fire() && !io.smi.req.bits.rw) {
    when (io.smi.req.bits.addr === UInt(0)) {
      io.smi.resp.bits := enabled
    }
    when (io.smi.req.bits.addr === UInt(1)) {
      io.smi.resp.bits := period
    }
    when (io.smi.req.bits.addr === UInt(2)) {
      io.smi.resp.bits := duty
    }
  }

  // A simple PWM controller
  val counter = Reg(UInt(width = 64))
  counter := counter + UInt(1)
  when (counter > period) {
    counter := UInt(0)
  }
  io.iobit := enabled && (counter > duty)
}
