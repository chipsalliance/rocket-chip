package rocket

import Chisel._
import Node._
import uncore._

class RoCCInstruction extends Bundle
{
  val funct = Bits(width = 7)
  val rs2 = Bits(width = 5)
  val rs1 = Bits(width = 5)
  val xd = Bool()
  val xs1 = Bool()
  val xs2 = Bool()
  val rd = Bits(width = 5)
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
  implicit val as = conf.as
  val cmd = Decoupled(new RoCCCommand).flip
  val resp = Decoupled(new RoCCResponse)
  val mem = new HellaCacheIO()(conf.dcache)
  val busy = Bool(OUTPUT)
  val s = Bool(INPUT)
  val interrupt = Bool(OUTPUT)
  
  // These should be handled differently, eventually
  val imem = new UncachedTileLinkIO()(conf.tl)
  val iptw = new TLBPTWIO
  val dptw = new TLBPTWIO
  val pptw = new TLBPTWIO
  val exception = Bool(INPUT)

  override def clone = new RoCCInterface().asInstanceOf[this.type]
}

abstract class RoCC(conf: RocketConfiguration) extends Module
{
  val io = new RoCCInterface()(conf)

  io.mem.req.bits.phys := Bool(true) // don't perform address translation
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
    // command resolved if no stalls AND not issuing a load that will need a request

  // PROC RESPONSE INTERFACE
  io.resp.valid := cmd.valid && doResp && !stallReg && !stallLoad
    // valid response if valid command, need a response, and no stalls
  io.resp.bits.rd := cmd.bits.inst.rd
    // Must respond with the appropriate tag or undefined behavior
  io.resp.bits.data := accum
    // Semantics is to always send out prior accumulator register value

  io.busy := cmd.valid || busy.reduce(_||_)
    // Be busy when have pending memory requests or committed possibility of pending requests
  io.interrupt := Bool(false)
    // Set this true to trigger an interrupt on the processor (please refer to supervisor documentation)

  // MEMORY REQUEST INTERFACE
  io.mem.req.valid := cmd.valid && doLoad && !stallReg && !stallResp
  io.mem.req.bits.addr := addend
  io.mem.req.bits.tag := addr
  io.mem.req.bits.cmd := M_XRD // perform a load (M_XWR for stores)
  io.mem.req.bits.typ := MT_D // D = 8 bytes, W = 4, H = 2, B = 1
  io.mem.req.bits.data := Bits(0) // we're not performing any stores...
}
