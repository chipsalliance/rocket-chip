package rocket

import Chisel._
import Node._
import uncore._

class RoCCInstruction extends Bundle
{
  val rd = Bits(width = 5)
  val rs1 = Bits(width = 5)
  val rs2 = Bits(width = 5)
  val funct = Bits(width = 7)
  val xd = Bool()
  val xs1 = Bool()
  val xs2 = Bool()
  val opcode = Bits(width = 7)
}

class RoCCCommand(implicit conf: RocketConfiguration) extends Bundle
{
  val inst = new RoCCInstruction
  val rs1 = Bits(width = conf.xprlen)
  val rs2 = Bits(width = conf.xprlen)

  override def clone = new RoCCCommand().asInstanceOf[this.type]
}

class RoCCResponse(implicit conf: RocketConfiguration) extends Bundle
{
  val rd = Bits(width = 5)
  val data = Bits(width = conf.xprlen)

  override def clone = new RoCCResponse().asInstanceOf[this.type]
}

class RoCCInterface(implicit conf: RocketConfiguration) extends Bundle
{
  val cmd = Decoupled(new RoCCCommand).flip
  val resp = Decoupled(new RoCCResponse)
  val mem = new HellaCacheIO()(conf.dcache)
  val busy = Bool(OUTPUT)
  val interrupt = Bool(OUTPUT)

  override def clone = new RoCCInterface().asInstanceOf[this.type]
}

abstract class RoCC(conf: RocketConfiguration) extends Module
{
  val io = new RoCCInterface()(conf)
}

class AccumulatorExample(conf: RocketConfiguration) extends RoCC(conf)
{
  val n = 4
  val regfile = Mem(UInt(width = conf.xprlen), n)
  val busy = Vec.fill(n){Reg(init=Bool(false))}

  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct
  val addr = cmd.bits.inst.rs2(log2Up(n)-1,0)
  val doWrite = funct === UInt(0)
  val doRead = funct === UInt(1)
  val doLoad = funct === UInt(2)
  val doAccum = funct === UInt(3)
  val memRespTag = io.mem.resp.bits.tag(log2Up(n)-1,0)

  // datapath
  val addend = cmd.bits.rs1
  val accum = regfile(addr)
  val wdata = Mux(doWrite, addend, accum + addend)

  when (cmd.fire() && (doWrite || doAccum)) {
    regfile(addr) := wdata
  }

  when (io.mem.resp.valid) {
    regfile(memRespTag) := io.mem.resp.bits.data
  }

  // control
  when (io.mem.req.fire()) {
    busy(addr) := Bool(true)
  }

  when (io.mem.resp.valid) {
    busy(memRespTag) := Bool(false)
  }

  val doResp = cmd.bits.inst.xd
  val stallReg = busy(addr)
  val stallLoad = doLoad && !io.mem.req.ready
  val stallResp = doResp && !io.resp.ready

  cmd.ready := !stallReg && !stallLoad && !stallResp

  io.resp.valid := cmd.valid && doResp && !stallReg && !stallLoad
  io.resp.bits.rd := cmd.bits.inst.rd
  io.resp.bits.data := accum

  io.busy := Bool(false)
  io.interrupt := Bool(false)

  io.mem.req.valid := cmd.valid && doLoad && !stallReg && !stallResp
  io.mem.req.bits.addr := addend
  io.mem.req.bits.cmd := M_XRD // perform a load (M_XWR for stores)
  io.mem.req.bits.typ := MT_D // D = 8 bytes, W = 4, H = 2, B = 1
  io.mem.req.bits.data := Bits(0) // we're not performing any stores...
  io.mem.req.bits.phys := Bool(true) // don't perform address translation
}
