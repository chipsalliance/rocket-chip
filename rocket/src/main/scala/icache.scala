package rocket

import Chisel._
import uncore._
import Util._

case class ICacheConfig(sets: Int, assoc: Int,
                        ibytes: Int = 4, rowbytes: Int = 16,
                        ntlb: Int = 8, 
                        tl: TileLinkConfiguration,
                        as: AddressSpaceConfiguration,
                        btb: BTBConfig,
                        code: Code = new IdentityCode)
{
  val w = 1

  val dm = assoc == 1
  val lines = sets * assoc
  val idxbits = log2Up(sets)
  val offbits = log2Up(tl.dataBits/8)
  val rowbits = rowbytes*8
  val rowoffbits = log2Up(rowbytes)
  val untagbits = idxbits + offbits
  val tagbits = as.paddrBits - untagbits
  val refillcycles = tl.dataBits/rowbits

  require(isPow2(sets) && isPow2(assoc))
  require(isPow2(w) && isPow2(ibytes))
  require(as.pgIdxBits >= untagbits)
}

class FrontendReq()(implicit conf: ICacheConfig) extends Bundle {
  val pc = UInt(width = conf.as.vaddrBits+1)
  override def clone = new FrontendReq().asInstanceOf[this.type]
}

class FrontendResp(implicit conf: ICacheConfig) extends Bundle {
  val pc = UInt(width = conf.as.vaddrBits+1)  // ID stage PC
  val data = Bits(width = conf.ibytes*8)
  val xcpt_ma = Bool()
  val xcpt_if = Bool()

  override def clone = new FrontendResp().asInstanceOf[this.type]
}

class CPUFrontendIO(implicit conf: ICacheConfig) extends Bundle {
  val req = Valid(new FrontendReq)
  val resp = Decoupled(new FrontendResp).flip
  val btb_resp = Valid(new BTBResp()(conf.btb)).flip
  val btb_update = Valid(new BTBUpdate()(conf.btb))
  val ptw = new TLBPTWIO()(conf.as).flip
  val invalidate = Bool(OUTPUT)
}

class Frontend(implicit c: ICacheConfig) extends Module
{
  implicit val (tl, as) = (c.tl, c.as)
  val io = new Bundle {
    val cpu = new CPUFrontendIO()(c).flip
    val mem = new UncachedTileLinkIO
  }
  
  val btb = Module(new BTB()(c.btb))
  val icache = Module(new ICache)
  val tlb = Module(new TLB(c.ntlb))

  val s1_pc_ = Reg(UInt())
  val s1_pc = s1_pc_ & SInt(-2) // discard LSB of PC (throughout the pipeline)
  val s1_same_block = Reg(Bool())
  val s2_valid = Reg(init=Bool(true))
  val s2_pc = Reg(init=UInt(START_ADDR))
  val s2_btb_resp_valid = Reg(init=Bool(false))
  val s2_btb_resp_bits = Reg(btb.io.resp.bits.clone)
  val s2_xcpt_if = Reg(init=Bool(false))

  val msb = c.as.vaddrBits-1
  val btbTarget = Cat(btb.io.resp.bits.target(msb), btb.io.resp.bits.target)
  val pcp4_0 = s1_pc + UInt(c.ibytes)
  val pcp4 = Cat(s1_pc(msb) & pcp4_0(msb), pcp4_0(msb,0))
  val icmiss = s2_valid && !icache.io.resp.valid
  val predicted_npc = Mux(btb.io.resp.bits.taken, btbTarget, pcp4)
  val npc = Mux(icmiss, s2_pc, predicted_npc).toUInt
  val s0_same_block = !icmiss && !io.cpu.req.valid && !btb.io.resp.bits.taken && ((pcp4 & c.rowbytes) === (s1_pc & c.rowbytes))

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

  btb.io.req := s1_pc & SInt(-c.ibytes)
  btb.io.update := io.cpu.btb_update
  btb.io.invalidate := io.cpu.invalidate || io.cpu.ptw.invalidate

  tlb.io.ptw <> io.cpu.ptw
  tlb.io.req.valid := !stall && !icmiss
  tlb.io.req.bits.vpn := s1_pc >> UInt(c.as.pgIdxBits)
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
  io.cpu.resp.bits.pc := s2_pc & SInt(-c.ibytes) // discard PC LSBs
  io.cpu.resp.bits.data := icache.io.resp.bits.datablock >> (s2_pc(log2Up(c.rowbytes)-1,log2Up(c.ibytes)) << log2Up(c.ibytes*8))
  io.cpu.resp.bits.xcpt_ma := s2_pc(log2Up(c.ibytes)-1,0) != UInt(0)
  io.cpu.resp.bits.xcpt_if := s2_xcpt_if

  io.cpu.btb_resp.valid := s2_btb_resp_valid
  io.cpu.btb_resp.bits := s2_btb_resp_bits
}

class ICacheReq(implicit c: ICacheConfig)  extends Bundle {
  val idx = UInt(width = c.as.pgIdxBits)
  val ppn = UInt(width = c.as.ppnBits) // delayed one cycle
  val kill = Bool() // delayed one cycle
  override def clone = new ICacheReq().asInstanceOf[this.type]
}

class ICacheResp(implicit c: ICacheConfig) extends Bundle {
  val data = Bits(width = c.ibytes*8)
  val datablock = Bits(width = c.rowbits)
  override def clone = new ICacheResp().asInstanceOf[this.type]
}

class ICache(implicit c: ICacheConfig) extends Module
{
  implicit val (tl, ln) = (c.tl, c.tl.ln)
  val io = new Bundle {
    val req = Valid(new ICacheReq).flip
    val resp = Decoupled(new ICacheResp)
    val invalidate = Bool(INPUT)
    val mem = new UncachedTileLinkIO
  }

  val s_ready :: s_request :: s_refill_wait :: s_refill :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  val invalidated = Reg(Bool())
  val stall = !io.resp.ready
  val rdy = Bool()

  val s2_valid = Reg(init=Bool(false))
  val s2_addr = Reg(UInt(width = c.as.paddrBits))
  val s2_any_tag_hit = Bool()

  val s1_valid = Reg(init=Bool(false))
  val s1_pgoff = Reg(UInt(width = c.as.pgIdxBits))
  val s1_addr = Cat(io.req.bits.ppn, s1_pgoff).toUInt
  val s1_tag = s1_addr(c.tagbits+c.untagbits-1,c.untagbits)

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

  val s2_tag = s2_addr(c.tagbits+c.untagbits-1,c.untagbits)
  val s2_idx = s2_addr(c.untagbits-1,c.offbits)
  val s2_offset = s2_addr(c.offbits-1,0)
  val s2_hit = s2_valid && s2_any_tag_hit
  val s2_miss = s2_valid && !s2_any_tag_hit
  rdy := state === s_ready && !s2_miss

  var refill_cnt = UInt(0)
  var refill_done = state === s_refill 
  var refill_valid = io.mem.grant.valid
  var refill_bits = io.mem.grant.bits
  def doRefill(g: Grant): Bool = Bool(true)
  if(c.refillcycles > 1) {
    val ser = Module(new FlowThroughSerializer(io.mem.grant.bits, c.refillcycles, doRefill))
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

  val repl_way = if (c.dm) UInt(0) else LFSR16(s2_miss)(log2Up(c.assoc)-1,0)
  val enc_tagbits = c.code.width(c.tagbits)
  val tag_array = Mem(Bits(width = enc_tagbits*c.assoc), c.sets, seqRead = true)
  val tag_raddr = Reg(UInt())
  when (refill_done) {
    val wmask = FillInterleaved(enc_tagbits, if (c.dm) Bits(1) else UIntToOH(repl_way))
    val tag = c.code.encode(s2_tag).toUInt
    tag_array.write(s2_idx, Fill(c.assoc, tag), wmask)
  }
//  /*.else*/when (s0_valid) { // uncomment ".else" to infer 6T SRAM
  .elsewhen (s0_valid) {
    tag_raddr := s0_pgoff(c.untagbits-1,c.offbits)
  }

  val vb_array = Reg(init=Bits(0, c.lines))
  when (refill_done && !invalidated) {
    vb_array := vb_array.bitSet(Cat(repl_way, s2_idx), Bool(true))
  }
  when (io.invalidate) {
    vb_array := Bits(0)
    invalidated := Bool(true)
  }
  val s2_disparity = Vec.fill(c.assoc){Bool()}
  for (i <- 0 until c.assoc)
    when (s2_valid && s2_disparity(i)) { vb_array := vb_array.bitSet(Cat(UInt(i), s2_idx), Bool(false)) }

  val s1_tag_match = Vec.fill(c.assoc){Bool()}
  val s2_tag_hit = Vec.fill(c.assoc){Bool()}
  val s2_dout = Vec.fill(c.assoc){Reg(Bits())}

  for (i <- 0 until c.assoc) {
    val s1_vb = vb_array(Cat(UInt(i), s1_pgoff(c.untagbits-1,c.offbits))).toBool
    val s2_vb = Reg(Bool())
    val s2_tag_disparity = Reg(Bool())
    val s2_tag_match = Reg(Bool())
    val tag_out = tag_array(tag_raddr)(enc_tagbits*(i+1)-1, enc_tagbits*i)
    when (s1_valid && rdy && !stall) {
      s2_vb := s1_vb
      s2_tag_disparity := c.code.decode(tag_out).error
      s2_tag_match := s1_tag_match(i)
    }
    s1_tag_match(i) := tag_out(c.tagbits-1,0) === s1_tag
    s2_tag_hit(i) := s2_vb && s2_tag_match
    s2_disparity(i) := s2_vb && (s2_tag_disparity || c.code.decode(s2_dout(i)).error)
  }
  s2_any_tag_hit := s2_tag_hit.reduceLeft(_||_) && !s2_disparity.reduceLeft(_||_)

  for (i <- 0 until c.assoc) {
    val data_array = Mem(Bits(width = c.code.width(c.rowbits)), c.sets*c.refillcycles, seqRead = true)
    val s1_raddr = Reg(UInt())
    when (refill_valid && repl_way === UInt(i)) {
      val e_d = c.code.encode(refill_bits.payload.data)
      if(c.refillcycles > 1) data_array(Cat(s2_idx,refill_cnt)) := e_d
      else                   data_array(s2_idx) := e_d
    }
//    /*.else*/when (s0_valid) { // uncomment ".else" to infer 6T SRAM
    .elsewhen (s0_valid) {
      s1_raddr := s0_pgoff(c.untagbits-1,c.offbits-(if(c.refillcycles > 1) refill_cnt.getWidth else 0))
    }
    // if s1_tag_match is critical, replace with partial tag check
    when (s1_valid && rdy && !stall && (Bool(c.dm) || s1_tag_match(i))) { s2_dout(i) := data_array(s1_raddr) }
  }
  val s2_dout_word = s2_dout.map(x => (x >> (s2_offset(log2Up(c.rowbytes)-1,log2Up(c.ibytes)) << log2Up(c.ibytes*8)))(c.ibytes*8-1,0))
  io.resp.bits.data := Mux1H(s2_tag_hit, s2_dout_word)
  io.resp.bits.datablock := Mux1H(s2_tag_hit, s2_dout)

  val ack_q = Module(new Queue(new LogicalNetworkIO(new GrantAck), 1))
  ack_q.io.enq.valid := refill_done && tl.co.requiresAckForGrant(refill_bits.payload.g_type)
  ack_q.io.enq.bits.payload.master_xact_id := refill_bits.payload.master_xact_id
  ack_q.io.enq.bits.header.dst := refill_bits.header.src

  // output signals
  io.resp.valid := s2_hit
  io.mem.acquire.valid := (state === s_request) && ack_q.io.enq.ready
  io.mem.acquire.bits.payload := Acquire(tl.co.getUncachedReadAcquireType, s2_addr >> UInt(c.offbits), UInt(0))
  io.mem.grant_ack <> ack_q.io.deq

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
