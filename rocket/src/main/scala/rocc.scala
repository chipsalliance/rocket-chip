// See LICENSE for license details.

package rocket

import Chisel._
import uncore.tilelink._
import uncore.constants._
import uncore.agents.CacheName
import Util._
import cde.{Parameters, Field}

case object RoccMaxTaggedMemXacts extends Field[Int]
case object RoccNMemChannels extends Field[Int]
case object RoccNPTWPorts extends Field[Int]
case object RoccNCSRs extends Field[Int]

class RoCCCSRs(implicit p: Parameters) extends CoreBundle()(p) {
  val rdata = Vec(nRoccCsrs, UInt(INPUT, xLen))
  val waddr = UInt(OUTPUT, CSR.ADDRSZ)
  val wdata = UInt(OUTPUT, xLen)
  val wen = Bool(OUTPUT)
}

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

class RoCCCommand(implicit p: Parameters) extends CoreBundle()(p) {
  val inst = new RoCCInstruction
  val rs1 = Bits(width = xLen)
  val rs2 = Bits(width = xLen)
  val status = new MStatus
}

class RoCCResponse(implicit p: Parameters) extends CoreBundle()(p) {
  val rd = Bits(width = 5)
  val data = Bits(width = xLen)
}

class RoCCInterface(implicit p: Parameters) extends CoreBundle()(p) {
  val cmd = Decoupled(new RoCCCommand).flip
  val resp = Decoupled(new RoCCResponse)
  val mem = new HellaCacheIO()(p.alterPartial({ case CacheName => "L1D" }))
  val busy = Bool(OUTPUT)
  val interrupt = Bool(OUTPUT)
  
  // These should be handled differently, eventually
  val autl = new ClientUncachedTileLinkIO
  val utl = Vec(p(RoccNMemChannels), new ClientUncachedTileLinkIO)
  val ptw = Vec(p(RoccNPTWPorts), new TLBPTWIO)
  val fpu_req = Decoupled(new FPInput)
  val fpu_resp = Decoupled(new FPResult).flip
  val exception = Bool(INPUT)
  val csr = (new RoCCCSRs).flip
  val host_id = UInt(INPUT, log2Up(nCores))

  override def cloneType = new RoCCInterface().asInstanceOf[this.type]
}

abstract class RoCC(implicit p: Parameters) extends CoreModule()(p) {
  val io = new RoCCInterface
  io.mem.req.bits.phys := Bool(true) // don't perform address translation
}

class AccumulatorExample(n: Int = 4)(implicit p: Parameters) extends RoCC()(p) {
  val regfile = Mem(n, UInt(width = xLen))
  val busy = Reg(init = Vec.fill(n){Bool(false)})

  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct
  val addr = cmd.bits.rs2(log2Up(n)-1,0)
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
  io.mem.invalidate_lr := false

  io.autl.acquire.valid := false
  io.autl.grant.ready := false
}

class TranslatorExample(implicit p: Parameters) extends RoCC()(p) {
  val req_addr = Reg(UInt(width = coreMaxAddrBits))
  val req_rd = Reg(io.resp.bits.rd)
  val req_offset = req_addr(pgIdxBits - 1, 0)
  val req_vpn = req_addr(coreMaxAddrBits - 1, pgIdxBits)
  val pte = Reg(new PTE)

  val s_idle :: s_ptw_req :: s_ptw_resp :: s_resp :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  io.cmd.ready := (state === s_idle)

  when (io.cmd.fire()) {
    req_rd := io.cmd.bits.inst.rd
    req_addr := io.cmd.bits.rs1
    state := s_ptw_req
  }

  private val ptw = io.ptw(0)

  when (ptw.req.fire()) { state := s_ptw_resp }

  when (state === s_ptw_resp && ptw.resp.valid) {
    pte := ptw.resp.bits.pte
    state := s_resp
  }

  when (io.resp.fire()) { state := s_idle }

  ptw.req.valid := (state === s_ptw_req)
  ptw.req.bits.addr := req_vpn
  ptw.req.bits.store := Bool(false)
  ptw.req.bits.fetch := Bool(false)

  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := req_rd
  io.resp.bits.data := Mux(pte.leaf(), Cat(pte.ppn, req_offset), ~UInt(0, xLen))

  io.busy := (state =/= s_idle)
  io.interrupt := Bool(false)
  io.mem.req.valid := Bool(false)
  io.mem.invalidate_lr := Bool(false)
  io.autl.acquire.valid := Bool(false)
  io.autl.grant.ready := Bool(false)
}

class CharacterCountExample(implicit p: Parameters) extends RoCC()(p)
    with HasTileLinkParameters {

  private val blockOffset = tlBeatAddrBits + tlByteAddrBits

  val needle = Reg(UInt(width = 8))
  val addr = Reg(UInt(width = coreMaxAddrBits))
  val count = Reg(UInt(width = xLen))
  val resp_rd = Reg(io.resp.bits.rd)

  val addr_block = addr(coreMaxAddrBits - 1, blockOffset)
  val offset = addr(blockOffset - 1, 0)
  val next_addr = (addr_block + UInt(1)) << UInt(blockOffset)

  val s_idle :: s_acq :: s_gnt :: s_check :: s_resp :: Nil = Enum(Bits(), 5)
  val state = Reg(init = s_idle)

  val gnt = io.autl.grant.bits
  val recv_data = Reg(UInt(width = tlDataBits))
  val recv_beat = Reg(UInt(width = tlBeatAddrBits))

  val data_bytes = Vec.tabulate(tlDataBytes) { i => recv_data(8 * (i + 1) - 1, 8 * i) }
  val zero_match = data_bytes.map(_ === UInt(0))
  val needle_match = data_bytes.map(_ === needle)
  val first_zero = PriorityEncoder(zero_match)

  val chars_found = PopCount(needle_match.zipWithIndex.map {
    case (matches, i) =>
      val idx = Cat(recv_beat, UInt(i, tlByteAddrBits))
      matches && idx >= offset && UInt(i) <= first_zero
  })
  val zero_found = zero_match.reduce(_ || _)
  val finished = Reg(Bool())

  io.cmd.ready := (state === s_idle)
  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := resp_rd
  io.resp.bits.data := count
  io.autl.acquire.valid := (state === s_acq)
  io.autl.acquire.bits := GetBlock(addr_block = addr_block)
  io.autl.grant.ready := (state === s_gnt)

  when (io.cmd.fire()) {
    addr := io.cmd.bits.rs1
    needle := io.cmd.bits.rs2
    resp_rd := io.cmd.bits.inst.rd
    count := UInt(0)
    finished := Bool(false)
    state := s_acq
  }

  when (io.autl.acquire.fire()) { state := s_gnt }

  when (io.autl.grant.fire()) {
    recv_beat := gnt.addr_beat
    recv_data := gnt.data
    state := s_check
  }

  when (state === s_check) {
    when (!finished) {
      count := count + chars_found
    }
    when (zero_found) { finished := Bool(true) }
    when (recv_beat === UInt(tlDataBeats - 1)) {
      addr := next_addr
      state := Mux(zero_found || finished, s_resp, s_acq)
    } .otherwise {
      state := s_gnt
    }
  }

  when (io.resp.fire()) { state := s_idle }

  io.busy := (state =/= s_idle)
  io.interrupt := Bool(false)
  io.mem.req.valid := Bool(false)
  io.mem.invalidate_lr := Bool(false)
}

class OpcodeSet(val opcodes: Seq[UInt]) {
  def |(set: OpcodeSet) =
    new OpcodeSet(this.opcodes ++ set.opcodes)

  def matches(oc: UInt) = opcodes.map(_ === oc).reduce(_ || _)
}

object OpcodeSet {
  val custom0 = new OpcodeSet(Seq(Bits("b0001011")))
  val custom1 = new OpcodeSet(Seq(Bits("b0101011")))
  val custom2 = new OpcodeSet(Seq(Bits("b1011011")))
  val custom3 = new OpcodeSet(Seq(Bits("b1111011")))
  val all = custom0 | custom1 | custom2 | custom3
}

class RoccCommandRouter(opcodes: Seq[OpcodeSet])(implicit p: Parameters)
    extends CoreModule()(p) {
  val io = new Bundle {
    val in = Decoupled(new RoCCCommand).flip
    val out = Vec(opcodes.size, Decoupled(new RoCCCommand))
    val busy = Bool(OUTPUT)
  }

  val cmd = Queue(io.in)
  val cmdReadys = io.out.zip(opcodes).map { case (out, opcode) =>
    val me = opcode.matches(cmd.bits.inst.opcode)
    out.valid := cmd.valid && me
    out.bits := cmd.bits
    out.ready && me
  }
  cmd.ready := cmdReadys.reduce(_ || _)
  io.busy := cmd.valid

  assert(PopCount(cmdReadys) <= UInt(1),
    "Custom opcode matched for more than one accelerator")
}
