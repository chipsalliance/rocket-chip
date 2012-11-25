package rocket

import Chisel._
import Node._
import Constants._
import uncore._
import Util._

case class ICacheConfig(sets: Int, assoc: Int, co: CoherencePolicyWithUncached,
                        code: Code = new IdentityCode)
{
  val w = 1
  val ibytes = 4

  val dm = assoc == 1
  val lines = sets * assoc
  val databits = MEM_DATA_BITS
  val idxbits = log2Up(sets)
  val offbits = OFFSET_BITS
  val untagbits = idxbits + offbits
  val tagbits = PADDR_BITS - untagbits

  require(isPow2(sets) && isPow2(assoc))
  require(isPow2(w) && isPow2(ibytes))
  require(PGIDX_BITS >= untagbits)
}

class FrontendReq extends Bundle {
  val pc = UFix(width = VADDR_BITS+1)
  val mispredict = Bool()
  val taken = Bool()
  val currentpc = UFix(width = VADDR_BITS+1)
}

class FrontendResp(implicit conf: ICacheConfig) extends Bundle {
  val pc = UFix(width = VADDR_BITS+1)  // ID stage PC
  val data = Bits(width = conf.ibytes*8)
  val taken = Bool()
  val xcpt_ma = Bool()
  val xcpt_if = Bool()

  override def clone = new FrontendResp().asInstanceOf[this.type]
}

class IOCPUFrontend(implicit conf: ICacheConfig) extends Bundle {
  val req = new PipeIO()(new FrontendReq)
  val resp = new FIFOIO()(new FrontendResp).flip
  val ptw = new IOTLBPTW().flip
  val invalidate = Bool(OUTPUT)
}

class Frontend(implicit c: ICacheConfig) extends Component
{
  val io = new Bundle {
    val cpu = new IOCPUFrontend()(c).flip
    val mem = new ioUncachedRequestor
  }
  
  val btb = new rocketDpathBTB(BTB_ENTRIES)
  val icache = new ICache
  val tlb = new TLB(ITLB_ENTRIES)

  val s1_pc = Reg() { UFix() }
  val s1_same_block = Reg() { Bool() }
  val s2_valid = Reg(resetVal = Bool(true))
  val s2_pc = Reg(resetVal = UFix(START_ADDR))
  val s2_btb_hit = Reg(resetVal = Bool(false))
  val s2_xcpt_if = Reg(resetVal = Bool(false))

  val btbTarget = Cat(btb.io.target(VADDR_BITS-1), btb.io.target)
  val pcp4_0 = s1_pc + UFix(c.ibytes)
  val pcp4 = Cat(s1_pc(VADDR_BITS-1) & pcp4_0(VADDR_BITS-1), pcp4_0(VADDR_BITS-1,0))
  val icmiss = s2_valid && !icache.io.resp.valid
  val predicted_npc = Mux(btb.io.hit, btbTarget, pcp4)
  val npc = Mux(icmiss, s2_pc, predicted_npc).toUFix
  val s0_same_block = !icmiss && !io.cpu.req.valid && (predicted_npc >> log2Up(c.databits/8)) === (s1_pc >> log2Up(c.databits/8))

  val stall = io.cpu.resp.valid && !io.cpu.resp.ready
  when (!stall) {
    s1_same_block := s0_same_block && !tlb.io.resp.miss
    s1_pc := npc
    s2_valid := !icmiss
    s2_pc := s1_pc
    s2_btb_hit := btb.io.hit
    s2_xcpt_if := tlb.io.resp.xcpt_if
  }
  when (io.cpu.req.valid) {
    s1_same_block := Bool(false)
    s1_pc := io.cpu.req.bits.pc
    s2_valid := Bool(false)
  }

  btb.io.current_pc := s1_pc
  btb.io.wen := io.cpu.req.bits.mispredict
  btb.io.clr := !io.cpu.req.bits.taken
  btb.io.correct_pc := io.cpu.req.bits.currentpc
  btb.io.correct_target := io.cpu.req.bits.pc
  btb.io.invalidate := io.cpu.invalidate || io.cpu.ptw.invalidate

  tlb.io.ptw <> io.cpu.ptw
  tlb.io.req.valid := !stall && !icmiss
  tlb.io.req.bits.vpn := s1_pc >> UFix(PGIDX_BITS)
  tlb.io.req.bits.asid := UFix(0)
  tlb.io.req.bits.passthrough := Bool(false)
  tlb.io.req.bits.instruction := Bool(true)

  icache.io.mem <> io.mem
  icache.io.req.valid := !stall && !s0_same_block
  icache.io.req.bits.idx := Mux(io.cpu.req.valid, io.cpu.req.bits.pc, npc)
  icache.io.invalidate := io.cpu.invalidate
  icache.io.req.bits.ppn := tlb.io.resp.ppn
  icache.io.req.bits.kill := io.cpu.req.valid || tlb.io.resp.miss
  icache.io.resp.ready := !stall && !s1_same_block

  io.cpu.resp.valid := s2_valid && (s2_xcpt_if || icache.io.resp.valid)
  io.cpu.resp.bits.pc := s2_pc
  io.cpu.resp.bits.data := icache.io.resp.bits.datablock >> (s2_pc(log2Up(c.databits/8)-1,log2Up(c.ibytes)) << log2Up(c.ibytes*8))
  io.cpu.resp.bits.taken := s2_btb_hit
  io.cpu.resp.bits.xcpt_ma := s2_pc(log2Up(c.ibytes)-1,0) != UFix(0)
  io.cpu.resp.bits.xcpt_if := s2_xcpt_if
}

class ICache(implicit c: ICacheConfig) extends Component
{
  val io = new Bundle {
    val req = new PipeIO()(new Bundle {
      val idx = UFix(width = PGIDX_BITS)
      val ppn = UFix(width = PPN_BITS) // delayed one cycle
      val kill = Bool() // delayed one cycle
    }).flip
    val resp = new FIFOIO()(new Bundle {
      val data = Bits(width = c.ibytes*8)
      val datablock = Bits(width = c.databits)
    })
    val invalidate = Bool(INPUT)
    val mem = new ioUncachedRequestor
  }

  val s_ready :: s_request :: s_refill_wait :: s_refill :: Nil = Enum(4) { UFix() }
  val state = Reg(resetVal = s_ready)
  val invalidated = Reg() { Bool() }
  val stall = !io.resp.ready
  val rdy = Bool()

  val s2_valid = Reg(resetVal = Bool(false))
  val s2_addr = Reg { UFix(width = PADDR_BITS) }
  val s2_any_tag_hit = Bool()

  val s1_valid = Reg(resetVal = Bool(false))
  val s1_pgoff = Reg() { UFix(width = PGIDX_BITS) }
  val s1_addr = Cat(io.req.bits.ppn, s1_pgoff).toUFix
  val s1_tag = s1_addr(c.tagbits+c.untagbits-1,c.untagbits)

  val s0_valid = io.req.valid || s1_valid && stall
  val s0_pgoff = Mux(io.req.valid, io.req.bits.idx, s1_pgoff)

  s1_valid := io.req.valid && rdy || s1_valid && stall && !io.req.bits.kill
  when (io.req.valid && rdy) {
    s1_pgoff := s0_pgoff
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

  val (rf_cnt, refill_done) = Counter(io.mem.xact_rep.valid, REFILL_CYCLES)
  val repl_way = if (c.dm) UFix(0) else LFSR16(s2_miss)(log2Up(c.assoc)-1,0)

  val enc_tagbits = c.code.width(c.tagbits)
  val tag_array = Mem(c.sets, seqRead = true) { Bits(width = enc_tagbits*c.assoc) }
  val tag_rdata = Reg() { Bits() }
  when (refill_done) {
    val wmask = FillInterleaved(enc_tagbits, if (c.dm) Bits(1) else UFixToOH(repl_way))
    val tag = c.code.encode(s2_tag)
    tag_array.write(s2_idx, Fill(c.assoc, tag), wmask)
  }
  /*.else*/when (s0_valid) { // uncomment ".else" to infer 6T SRAM
    tag_rdata := tag_array(s0_pgoff(c.untagbits-1,c.offbits))
  }

  val vb_array = Reg(resetVal = Bits(0, c.lines))
  when (refill_done && !invalidated) {
    vb_array := vb_array.bitSet(Cat(repl_way, s2_idx), Bool(true))
  }
  when (io.invalidate) {
    vb_array := Bits(0)
    invalidated := Bool(true)
  }
  val s2_disparity = Vec(c.assoc) { Bool() }
  for (i <- 0 until c.assoc)
    when (s2_valid && s2_disparity(i)) { vb_array := vb_array.bitSet(Cat(UFix(i), s2_idx), Bool(false)) }

  val s1_tag_match = Vec(c.assoc) { Bool() }
  val s2_tag_hit = Vec(c.assoc) { Bool() }
  val s2_dout = Vec(c.assoc){Reg{Bits()}}

  for (i <- 0 until c.assoc) {
    val s1_vb = vb_array(Cat(UFix(i), s1_pgoff(c.untagbits-1,c.offbits))).toBool
    val s2_vb = Reg() { Bool() }
    val s2_tag_disparity = Reg() { Bool() }
    val s2_tag_match = Reg() { Bool() }
    val tag_out = tag_rdata(enc_tagbits*(i+1)-1, enc_tagbits*i)
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
    val data_array = Mem(c.sets*REFILL_CYCLES, seqRead = true){ Bits(width = c.code.width(c.databits)) }
    val s1_dout = Reg(){ Bits() }
    when (io.mem.xact_rep.valid && repl_way === UFix(i)) {
      val d = io.mem.xact_rep.bits.data
      data_array(Cat(s2_idx,rf_cnt)) := c.code.encode(d)
    }
    /*.else*/when (s0_valid) { // uncomment ".else" to infer 6T SRAM
      s1_dout := data_array(s0_pgoff(c.untagbits-1,c.offbits-rf_cnt.getWidth))
    }
    // if s1_tag_match is critical, replace with partial tag check
    when (s1_valid && rdy && !stall && (Bool(c.dm) || s1_tag_match(i))) { s2_dout(i) := s1_dout }
  }
  val s2_dout_word = s2_dout.map(x => (x >> (s2_offset(log2Up(c.databits/8)-1,log2Up(c.ibytes)) << log2Up(c.ibytes*8)))(c.ibytes*8-1,0))
  io.resp.bits.data := Mux1H(s2_tag_hit, s2_dout_word)
  io.resp.bits.datablock := Mux1H(s2_tag_hit, s2_dout)

  val finish_q = (new Queue(1)) { new TransactionFinish }
  finish_q.io.enq.valid := refill_done && io.mem.xact_rep.bits.require_ack
  finish_q.io.enq.bits.global_xact_id := io.mem.xact_rep.bits.global_xact_id

  // output signals
  io.resp.valid := s2_hit
  io.mem.xact_init.valid := (state === s_request) && finish_q.io.enq.ready
  io.mem.xact_init.bits := c.co.getUncachedReadTransactionInit(s2_addr >> UFix(c.offbits), UFix(0))
  io.mem.xact_finish <> finish_q.io.deq
  io.mem.xact_rep.ready := Bool(true)

  // control state machine
  switch (state) {
    is (s_ready) {
      when (s2_miss) { state := s_request }
      invalidated := Bool(false)
    }
    is (s_request) {
      when (io.mem.xact_init.ready && finish_q.io.enq.ready) { state := s_refill_wait }
    }
    is (s_refill_wait) {
      when (io.mem.xact_abort.valid) { state := s_request }
      when (io.mem.xact_rep.valid) { state := s_refill }
    }
    is (s_refill) {
      when (refill_done) { state := s_ready }
    }
  }
}
