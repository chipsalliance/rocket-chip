package rocket

import Chisel._
import Node._

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
  val busy = Bool(OUTPUT)
  val interrupt = Bool(OUTPUT)

  override def clone = new RoCCInterface().asInstanceOf[this.type]
}

abstract class RoCC(implicit conf: RocketConfiguration) extends Module
{
  val io = new RoCCInterface
}

class AccumulatorExample(implicit conf: RocketConfiguration) extends RoCC
{
  val regfile = Mem(UInt(width = conf.xprlen), 4)

  val funct = io.cmd.bits.inst.funct
  val addr = io.cmd.bits.inst.rs2
  val addend = io.cmd.bits.rs1
  val accum = regfile(addr)
  val wdata = Mux(funct === UInt(0), addend, accum + addend)

  when (io.cmd.fire() && (funct === UInt(1) || funct === UInt(3))) {
    regfile(addr) := wdata
  }

  io.cmd.ready := io.resp.ready
  io.resp.valid := io.cmd.valid && io.cmd.bits.inst.xd
  io.resp.bits.rd := io.cmd.bits.inst.rd
  io.resp.bits.data := accum
  io.busy := Bool(false)
  io.interrupt := Bool(false)
}
