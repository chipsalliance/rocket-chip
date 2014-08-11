package rocket

import Chisel._
import uncore._
import Util._

case object InstBytes extends Field[Int]
case object CoreBTBParams extends Field[PF]

class FrontendReq extends Bundle {
  val pc = UInt(width = params(VAddrBits)+1)
}

class FrontendResp extends Bundle {
  val pc = UInt(width = params(VAddrBits)+1)  // ID stage PC
  val data = Bits(width = params(InstBytes)*8)
  val xcpt_ma = Bool()
  val xcpt_if = Bool()
}

class CPUFrontendIO extends Bundle {
  params.alter(params(CoreBTBParams))
  val req = Valid(new FrontendReq)
  val resp = Decoupled(new FrontendResp).flip
  val btb_resp = Valid(new BTBResp).flip
  val btb_update = Valid(new BTBUpdate)
  val ptw = new TLBPTWIO().flip
  val invalidate = Bool(OUTPUT)
}

class Frontend extends Module
{
  val io = new Bundle {
    val cpu = new CPUFrontendIO().flip
    val mem = new UncachedTileLinkIO
  }
  
  val btb = Module(new BTB, params(CoreBTBParams))
  val icache = Module(new ICache)
  val tlb = Module(new TLB(params(NTLBEntries)))

  val s1_pc_ = Reg(UInt())
  val s1_pc = s1_pc_ & SInt(-2) // discard LSB of PC (throughout the pipeline)
  val s1_same_block = Reg(Bool())
  val s2_valid = Reg(init=Bool(true))
  val s2_pc = Reg(init=UInt(START_ADDR))
  val s2_btb_resp_valid = Reg(init=Bool(false))
  val s2_btb_resp_bits = Reg(btb.io.resp.bits.clone)
  val s2_xcpt_if = Reg(init=Bool(false))

  val msb = params(VAddrBits)-1
  val btbTarget = Cat(btb.io.resp.bits.target(msb), btb.io.resp.bits.target)
  val pcp4_0 = s1_pc + UInt(params(InstBytes))
  val pcp4 = Cat(s1_pc(msb) & pcp4_0(msb), pcp4_0(msb,0))
  val icmiss = s2_valid && !icache.io.resp.valid
  val predicted_npc = Mux(btb.io.resp.bits.taken, btbTarget, pcp4)
  val npc = Mux(icmiss, s2_pc, predicted_npc).toUInt
  val s0_same_block = !icmiss && !io.cpu.req.valid && !btb.io.resp.bits.taken && ((pcp4 & params(RowBytes)) === (s1_pc & params(RowBytes)))

  val stall = io.cpu.resp.valid && !io.cpu.resp.ready
  when (!stall) {
    s1_same_block := s0_same_block && !tlb.io.resp.miss
    s1_pc_ := npc
    s2_valid := !icmiss
    when (!icmiss) {
      s2_pc := s1_pc
      s2_btb_resp_valid := btb.io.resp.valid
      when (btb.io.resp.valid) { s2_btb_resp_bits := btb.io.resp.bits }
      s2_xcpt_if := tlb.io.resp.xcpt_if
    }
  }
  when (io.cpu.req.valid) {
    s1_same_block := Bool(false)
    s1_pc_ := io.cpu.req.bits.pc
    s2_valid := Bool(false)
  }

  btb.io.req := s1_pc & SInt(-params(InstBytes))
  btb.io.update := io.cpu.btb_update
  btb.io.invalidate := io.cpu.invalidate || io.cpu.ptw.invalidate

  tlb.io.ptw <> io.cpu.ptw
  tlb.io.req.valid := !stall && !icmiss
  tlb.io.req.bits.vpn := s1_pc >> UInt(params(PgIdxBits))
  tlb.io.req.bits.asid := UInt(0)
  tlb.io.req.bits.passthrough := Bool(false)
  tlb.io.req.bits.instruction := Bool(true)

  icache.io.mem <> io.mem
  icache.io.req.valid := !stall && !s0_same_block
  icache.io.req.bits.idx := Mux(io.cpu.req.valid, io.cpu.req.bits.pc, npc)
  icache.io.invalidate := io.cpu.invalidate
  icache.io.req.bits.ppn := tlb.io.resp.ppn
  icache.io.req.bits.kill := io.cpu.req.valid || tlb.io.resp.miss || icmiss
  icache.io.resp.ready := !stall && !s1_same_block

  io.cpu.resp.valid := s2_valid && (s2_xcpt_if || icache.io.resp.valid)
  io.cpu.resp.bits.pc := s2_pc & SInt(-params(InstBytes)) // discard PC LSBs
  io.cpu.resp.bits.data := icache.io.resp.bits.datablock >> (s2_pc(log2Up(params(RowBytes))-1,log2Up(params(InstBytes))) << log2Up(params(InstBytes)*8))
  io.cpu.resp.bits.xcpt_ma := s2_pc(log2Up(params(InstBytes))-1,0) != UInt(0)
  io.cpu.resp.bits.xcpt_if := s2_xcpt_if

  io.cpu.btb_resp.valid := s2_btb_resp_valid
  io.cpu.btb_resp.bits := s2_btb_resp_bits
}

class ICacheReq extends Bundle {
  val idx = UInt(width = params(PgIdxBits))
  val ppn = UInt(width = params(PPNBits)) // delayed one cycle
  val kill = Bool() // delayed one cycle
}

class ICacheResp extends Bundle {
  val data = Bits(width = params(InstBytes)*8)
  val datablock = Bits(width = params(RowBits))
}

class ICache extends Module
{
  val (nSets, nWays, co, ecc) = (params(NSets), params(NWays), params(TLCoherence), params(ECCCode))
  val io = new Bundle {
    val req = Valid(new ICacheReq).flip
    val resp = Decoupled(new ICacheResp)
    val invalidate = Bool(INPUT)
    val mem = new UncachedTileLinkIO
  }
  require(isPow2(nSets) && isPow2(nWays))
  require(isPow2(params(InstBytes)))
  require(params(PgIdxBits) >= params(UntagBits))

  val s_ready :: s_request :: s_refill_wait :: s_refill :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  val invalidated = Reg(Bool())
  val stall = !io.resp.ready
  val rdy = Bool()

  val s2_valid = Reg(init=Bool(false))
  val s2_addr = Reg(UInt(width = params(PAddrBits)))
  val s2_any_tag_hit = Bool()

  val s1_valid = Reg(init=Bool(false))
  val s1_pgoff = Reg(UInt(width = params(PgIdxBits)))
  val s1_addr = Cat(io.req.bits.ppn, s1_pgoff).toUInt
  val s1_tag = s1_addr(params(TagBits)+params(UntagBits)-1,params(UntagBits))

  val s0_valid = io.req.valid || s1_valid && stall
  val s0_pgoff = Mux(s1_valid && stall, s1_pgoff, io.req.bits.idx)

  s1_valid := io.req.valid && rdy || s1_valid && stall && !io.req.bits.kill
  when (io.req.valid && rdy) {
    s1_pgoff := io.req.bits.idx
  }

  s2_valid := s1_valid && rdy && !io.req.bits.kill || io.resp.valid && stall
  when (s1_valid && rdy && !stall) {
    s2_addr := s1_addr
  }

  val s2_tag = s2_addr(params(TagBits)+params(UntagBits)-1,params(UntagBits))
  val s2_idx = s2_addr(params(UntagBits)-1,params(OffBits))
  val s2_offset = s2_addr(params(OffBits)-1,0)
  val s2_hit = s2_valid && s2_any_tag_hit
  val s2_miss = s2_valid && !s2_any_tag_hit
  rdy := state === s_ready && !s2_miss

  var refill_cnt = UInt(0)
  var refill_done = state === s_refill 
  var refill_valid = io.mem.grant.valid
  var refill_bits = io.mem.grant.bits
  def doRefill(g: Grant): Bool = Bool(true)
  if(params(RefillCycles) > 1) {
    val ser = Module(new FlowThroughSerializer(io.mem.grant.bits, params(RefillCycles), doRefill))
    ser.io.in <> io.mem.grant
    refill_cnt = ser.io.cnt
    refill_done = ser.io.done
    refill_valid = ser.io.out.valid
    refill_bits = ser.io.out.bits
    ser.io.out.ready := Bool(true)
  } else {
    io.mem.grant.ready := Bool(true)
  }
  //assert(!c.tlco.isVoluntary(refill_bits.payload) || !refill_valid, "UncachedRequestors shouldn't get voluntary grants.")

  val repl_way = if (params(IsDM)) UInt(0) else LFSR16(s2_miss)(log2Up(nWays)-1,0)
  val entagbits = ecc.width(params(TagBits))
  val tag_array = Mem(Bits(width = entagbits*nWays), nSets, seqRead = true)
  val tag_raddr = Reg(UInt())
  when (refill_done) {
    val wmask = FillInterleaved(entagbits, if (params(IsDM)) Bits(1) else UIntToOH(repl_way))
    val tag = ecc.encode(s2_tag).toUInt
    tag_array.write(s2_idx, Fill(nWays, tag), wmask)
  }
//  /*.else*/when (s0_valid) { // uncomment ".else" to infer 6T SRAM
  .elsewhen (s0_valid) {
    tag_raddr := s0_pgoff(params(UntagBits)-1,params(OffBits))
  }

  val vb_array = Reg(init=Bits(0, nSets*nWays))
  when (refill_done && !invalidated) {
    vb_array := vb_array.bitSet(Cat(repl_way, s2_idx), Bool(true))
  }
  when (io.invalidate) {
    vb_array := Bits(0)
    invalidated := Bool(true)
  }
  val s2_disparity = Vec.fill(nWays){Bool()}
  for (i <- 0 until nWays)
    when (s2_valid && s2_disparity(i)) { vb_array := vb_array.bitSet(Cat(UInt(i), s2_idx), Bool(false)) }

  val s1_tag_match = Vec.fill(nWays){Bool()}
  val s2_tag_hit = Vec.fill(nWays){Bool()}
  val s2_dout = Vec.fill(nWays){Reg(Bits())}

  for (i <- 0 until nWays) {
    val s1_vb = vb_array(Cat(UInt(i), s1_pgoff(params(UntagBits)-1,params(OffBits)))).toBool
    val s2_vb = Reg(Bool())
    val s2_tag_disparity = Reg(Bool())
    val s2_tag_match = Reg(Bool())
    val tag_out = tag_array(tag_raddr)(entagbits*(i+1)-1, entagbits*i)
    when (s1_valid && rdy && !stall) {
      s2_vb := s1_vb
      s2_tag_disparity := ecc.decode(tag_out).error
      s2_tag_match := s1_tag_match(i)
    }
    s1_tag_match(i) := tag_out(params(TagBits)-1,0) === s1_tag
    s2_tag_hit(i) := s2_vb && s2_tag_match
    s2_disparity(i) := s2_vb && (s2_tag_disparity || ecc.decode(s2_dout(i)).error)
  }
  s2_any_tag_hit := s2_tag_hit.reduceLeft(_||_) && !s2_disparity.reduceLeft(_||_)

  for (i <- 0 until nWays) {
    val data_array = Mem(Bits(width = ecc.width(params(RowBits))), nSets*params(RefillCycles), seqRead = true)
    val s1_raddr = Reg(UInt())
    when (refill_valid && repl_way === UInt(i)) {
      val e_d = ecc.encode(refill_bits.payload.data)
      if(params(RefillCycles) > 1) data_array(Cat(s2_idx,refill_cnt)) := e_d
      else                   data_array(s2_idx) := e_d
    }
//    /*.else*/when (s0_valid) { // uncomment ".else" to infer 6T SRAM
    .elsewhen (s0_valid) {
      s1_raddr := s0_pgoff(params(UntagBits)-1,params(OffBits)-(if(params(RefillCycles) > 1) refill_cnt.getWidth else 0))
    }
    // if s1_tag_match is critical, replace with partial tag check
    when (s1_valid && rdy && !stall && (Bool(params(IsDM)) || s1_tag_match(i))) { s2_dout(i) := data_array(s1_raddr) }
  }
  val s2_dout_word = s2_dout.map(x => (x >> (s2_offset(log2Up(params(RowBytes))-1,log2Up(params(InstBytes))) << log2Up(params(InstBytes)*8)))(params(InstBytes)*8-1,0))
  io.resp.bits.data := Mux1H(s2_tag_hit, s2_dout_word)
  io.resp.bits.datablock := Mux1H(s2_tag_hit, s2_dout)

  val ack_q = Module(new Queue(new LogicalNetworkIO(new Finish), 1))
  ack_q.io.enq.valid := refill_done && co.requiresAckForGrant(refill_bits.payload.g_type)
  ack_q.io.enq.bits.payload.master_xact_id := refill_bits.payload.master_xact_id
  ack_q.io.enq.bits.header.dst := refill_bits.header.src

  // output signals
  io.resp.valid := s2_hit
  io.mem.acquire.valid := (state === s_request) && ack_q.io.enq.ready
  io.mem.acquire.bits.payload := Acquire(co.getUncachedReadAcquireType, s2_addr >> UInt(params(OffBits)), UInt(0))
  io.mem.finish <> ack_q.io.deq

  // control state machine
  switch (state) {
    is (s_ready) {
      when (s2_miss) { state := s_request }
      invalidated := Bool(false)
    }
    is (s_request) {
      when (io.mem.acquire.ready && ack_q.io.enq.ready) { state := s_refill_wait }
    }
    is (s_refill_wait) {
      when (io.mem.grant.valid) { state := s_refill }
    }
    is (s_refill) {
      when (refill_done) { state := s_ready }
    }
  }
}
