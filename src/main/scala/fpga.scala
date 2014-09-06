package rocketchip

import Chisel._

abstract class AXISlave extends Module {
  val aw = 5
  val dw = 32
  val io = new Bundle {
    val in = Decoupled(Bits(width = dw)).flip
    val out = Decoupled(Bits(width = dw))
    val addr = Bits(INPUT, aw)
  }
}

class Slave extends AXISlave
{
  val top = Module(new Top)

  val memw = top.io.mem.resp.bits.data.getWidth
  val htifw = top.io.host.in.bits.getWidth
  
  val n = 4
  def wen(i: Int) = io.in.valid && io.addr(log2Up(n)-1,0) === UInt(i)
  def ren(i: Int) = io.out.ready && io.addr(log2Up(n)-1,0) === UInt(i)
  val rdata = Vec.fill(n){Bits(width = dw)}
  val rvalid = Vec.fill(n){Bool()}
  val wready = Vec.fill(n){Bool()}

  io.in.ready := wready(io.addr)
  io.out.valid := rvalid(io.addr)
  io.out.bits := rdata(io.addr)

  // write r0 -> htif.in (blocking)
  wready(0) := top.io.host.in.ready
  top.io.host.in.valid := wen(0)
  top.io.host.in.bits := io.in.bits

  // read cr0 -> htif.out (nonblocking)
  rdata(0) := Cat(top.io.host.out.bits, top.io.host.out.valid)
  rvalid(0) := Bool(true)
  top.io.host.out.ready := ren(0)
  require(dw >= htifw + 1)

  // read cr1 -> mem.req_cmd (nonblocking)
  // the memory system is FIFO from hereon out, so just remember the tags here
  val tagq = Module(new Queue(top.io.mem.req_cmd.bits.tag, 4))
  tagq.io.enq.bits := top.io.mem.req_cmd.bits.tag
  tagq.io.enq.valid := ren(1) && top.io.mem.req_cmd.valid && !top.io.mem.req_cmd.bits.rw
  top.io.mem.req_cmd.ready := ren(1)
  rdata(1) := Cat(top.io.mem.req_cmd.bits.addr, top.io.mem.req_cmd.bits.rw, top.io.mem.req_cmd.valid && (tagq.io.enq.ready || top.io.mem.req_cmd.bits.rw))
  rvalid(1) := Bool(true)
  require(dw >= top.io.mem.req_cmd.bits.addr.getWidth + 1 + 1)

  // write cr1 -> mem.resp (nonblocking)
  val in_count = Reg(init=UInt(0, log2Up(memw/dw)))
  val rf_count = Reg(init=UInt(0, log2Up(params(CacheBlockBytes)*8/memw)))
  require(memw % dw == 0 && isPow2(memw/dw))
  val in_reg = Reg(top.io.mem.resp.bits.data)
  top.io.mem.resp.bits.data := Cat(io.in.bits, in_reg(in_reg.getWidth-1,dw))
  top.io.mem.resp.bits.tag := tagq.io.deq.bits
  top.io.mem.resp.valid := wen(1) && in_count.andR
  tagq.io.deq.ready := top.io.mem.resp.fire() && rf_count.andR
  wready(1) := top.io.mem.resp.ready
  when (wen(1) && wready(1)) {
    in_count := in_count + UInt(1)
    in_reg := top.io.mem.resp.bits.data
  }
  when (top.io.mem.resp.fire()) {
    rf_count := rf_count + UInt(1)
  }

  // read cr2 -> mem.req_data (blocking)
  val out_count = Reg(init=UInt(0, log2Up(memw/dw)))
  top.io.mem.req_data.ready := ren(2) && out_count.andR
  rdata(2) := top.io.mem.req_data.bits.data >> (out_count * UInt(dw))
  rvalid(2) := top.io.mem.req_data.valid
  when (ren(2) && rvalid(2)) { out_count := out_count + UInt(1) }

  // read cr3 -> debug signals (nonblocking)
  rdata(3) := Cat(top.io.mem.req_cmd.valid, tagq.io.enq.ready)
  rvalid(3) := Bool(true)

  // writes to cr2, cr3 ignored
  wready(2) := Bool(true)
  wready(3) := Bool(true)
}
