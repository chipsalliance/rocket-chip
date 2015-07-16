package rocket

import Chisel._
import uncore._
import Util._

abstract trait L1CacheParameters extends CacheParameters with CoreParameters {
  val outerDataBeats = params(TLDataBeats)
  val outerDataBits = params(TLDataBits)
  val refillCyclesPerBeat = outerDataBits/rowBits
  val refillCycles = refillCyclesPerBeat*outerDataBeats
}

abstract trait FrontendParameters extends L1CacheParameters
abstract class FrontendBundle extends Bundle with FrontendParameters
abstract class FrontendModule extends Module with FrontendParameters

class FrontendReq extends CoreBundle {
  val pc = UInt(width = vaddrBitsExtended)
}

class FrontendResp extends CoreBundle {
  val pc = UInt(width = vaddrBitsExtended)  // ID stage PC
  val data = Vec.fill(coreFetchWidth) (Bits(width = coreInstBits))
  val mask = Bits(width = coreFetchWidth)
  val xcpt_if = Bool()
}

class CPUFrontendIO extends CoreBundle {
  val req = Valid(new FrontendReq)
  val resp = Decoupled(new FrontendResp).flip
  val btb_resp = Valid(new BTBResp).flip
  val btb_update = Valid(new BTBUpdate)
  val bht_update = Valid(new BHTUpdate)
  val ras_update = Valid(new RASUpdate)
  val invalidate = Bool(OUTPUT)
  val npc = UInt(INPUT, width = vaddrBitsExtended)
}

class Frontend(btb_updates_out_of_order: Boolean = false) extends FrontendModule
{
  val io = new Bundle {
    val cpu = new CPUFrontendIO().flip
    val ptw = new TLBPTWIO()
    val mem = new ClientUncachedTileLinkIO
  }

  val btb = Module(new BTB(btb_updates_out_of_order))
  val icache = Module(new ICache)
  val tlb = Module(new TLB)

  val s1_pc_ = Reg(UInt())
  val s1_pc = s1_pc_ & SInt(-coreInstBytes) // discard PC LSBS (this propagates down the pipeline)
  val s1_same_block = Reg(Bool())
  val s2_valid = Reg(init=Bool(true))
  val s2_pc = Reg(init=UInt(START_ADDR))
  val s2_btb_resp_valid = Reg(init=Bool(false))
  val s2_btb_resp_bits = Reg(btb.io.resp.bits)
  val s2_xcpt_if = Reg(init=Bool(false))

  val msb = vaddrBits-1
  val lsb = log2Up(coreFetchWidth*coreInstBytes)
  val btbTarget = Cat(btb.io.resp.bits.target(msb), btb.io.resp.bits.target)
  val ntpc_0 = s1_pc + UInt(coreInstBytes*coreFetchWidth)
  val ntpc = Cat(s1_pc(msb) & ntpc_0(msb), ntpc_0(msb,lsb), Bits(0,lsb)) // unsure
  val icmiss = s2_valid && !icache.io.resp.valid
  val predicted_npc = Mux(btb.io.resp.bits.taken, btbTarget, ntpc)
  val npc = Mux(icmiss, s2_pc, predicted_npc).toUInt
  val s0_same_block = !icmiss && !io.cpu.req.valid && !btb.io.resp.bits.taken && ((ntpc & rowBytes) === (s1_pc & rowBytes))

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

  btb.io.req.valid := !stall && !icmiss
  btb.io.req.bits.addr := s1_pc
  btb.io.btb_update := io.cpu.btb_update
  btb.io.bht_update := io.cpu.bht_update
  btb.io.ras_update := io.cpu.ras_update
  btb.io.invalidate := io.cpu.invalidate || io.ptw.invalidate

  tlb.io.ptw <> io.ptw
  tlb.io.req.valid := !stall && !icmiss
  tlb.io.req.bits.vpn := s1_pc >> UInt(pgIdxBits)
  tlb.io.req.bits.asid := UInt(0)
  tlb.io.req.bits.passthrough := Bool(false)
  tlb.io.req.bits.instruction := Bool(true)
  tlb.io.req.bits.store := Bool(false)

  icache.io.mem <> io.mem
  icache.io.req.valid := !stall && !s0_same_block
  icache.io.req.bits.idx := io.cpu.npc
  icache.io.invalidate := io.cpu.invalidate
  icache.io.req.bits.ppn := tlb.io.resp.ppn
  icache.io.req.bits.kill := io.cpu.req.valid || tlb.io.resp.miss || icmiss || io.ptw.invalidate
  icache.io.resp.ready := !stall && !s1_same_block

  io.cpu.resp.valid := s2_valid && (s2_xcpt_if || icache.io.resp.valid)
  io.cpu.resp.bits.pc := s2_pc
  io.cpu.npc := Mux(io.cpu.req.valid, io.cpu.req.bits.pc, npc)

  require(coreFetchWidth * coreInstBytes <= rowBytes)
  val fetch_data =
    if (coreFetchWidth * coreInstBytes == rowBytes) icache.io.resp.bits.datablock
    else icache.io.resp.bits.datablock >> (s2_pc(log2Up(rowBytes)-1,log2Up(coreFetchWidth*coreInstBytes)) << log2Up(coreFetchWidth*coreInstBits))

  for (i <- 0 until coreFetchWidth) {
    io.cpu.resp.bits.data(i) := fetch_data(i*coreInstBits+coreInstBits-1, i*coreInstBits)
  }

  val all_ones = UInt((1 << (coreFetchWidth+1))-1)
  val msk_pc = if (coreFetchWidth == 1) all_ones else all_ones << s2_pc(log2Up(coreFetchWidth) -1+2,2)
  io.cpu.resp.bits.mask := Mux(s2_btb_resp_valid, msk_pc & s2_btb_resp_bits.mask, msk_pc)
  io.cpu.resp.bits.xcpt_if := s2_xcpt_if

  io.cpu.btb_resp.valid := s2_btb_resp_valid
  io.cpu.btb_resp.bits := s2_btb_resp_bits
}

class ICacheReq extends FrontendBundle {
  val idx = UInt(width = pgIdxBits)
  val ppn = UInt(width = ppnBits) // delayed one cycle
  val kill = Bool() // delayed one cycle
}

class ICacheResp extends FrontendBundle {
  val data = Bits(width = coreInstBits)
  val datablock = Bits(width = rowBits)
}

class ICache extends FrontendModule
{
  val io = new Bundle {
    val req = Valid(new ICacheReq).flip
    val resp = Decoupled(new ICacheResp)
    val invalidate = Bool(INPUT)
    val mem = new ClientUncachedTileLinkIO
  }
  require(isPow2(nSets) && isPow2(nWays))
  require(isPow2(coreInstBytes))
  require(pgIdxBits >= untagBits)

  val s_ready :: s_request :: s_refill_wait :: s_refill :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  val invalidated = Reg(Bool())
  val stall = !io.resp.ready
  val rdy = Wire(Bool())

  val s2_valid = Reg(init=Bool(false))
  val s2_addr = Reg(UInt(width = paddrBits))
  val s2_any_tag_hit = Wire(Bool())

  val s1_valid = Reg(init=Bool(false))
  val s1_pgoff = Reg(UInt(width = pgIdxBits))
  val s1_addr = Cat(io.req.bits.ppn, s1_pgoff).toUInt
  val s1_tag = s1_addr(tagBits+untagBits-1,untagBits)

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

  val s2_tag = s2_addr(tagBits+untagBits-1,untagBits)
  val s2_idx = s2_addr(untagBits-1,blockOffBits)
  val s2_offset = s2_addr(blockOffBits-1,0)
  val s2_hit = s2_valid && s2_any_tag_hit
  val s2_miss = s2_valid && !s2_any_tag_hit
  rdy := state === s_ready && !s2_miss

  val narrow_grant = FlowThroughSerializer(io.mem.grant, refillCyclesPerBeat)
  val (refill_cnt, refill_wrap) = Counter(narrow_grant.fire(), refillCycles) //TODO Zero width wire
  val refill_done = state === s_refill && refill_wrap
  narrow_grant.ready := Bool(true)

  val repl_way = if (isDM) UInt(0) else LFSR16(s2_miss)(log2Up(nWays)-1,0)
  val entagbits = code.width(tagBits)
  val tag_array = SeqMem(Bits(width = entagbits*nWays), nSets)
  val tag_rdata = tag_array.read(s0_pgoff(untagBits-1,blockOffBits), !refill_done && s0_valid)
  when (refill_done) {
    val wmask = FillInterleaved(entagbits, if (isDM) Bits(1) else UIntToOH(repl_way))
    val tag = code.encode(s2_tag).toUInt
    tag_array.write(s2_idx, Fill(nWays, tag), wmask)
  }

  val vb_array = Reg(init=Bits(0, nSets*nWays))
  when (refill_done && !invalidated) {
    vb_array := vb_array.bitSet(Cat(repl_way, s2_idx), Bool(true))
  }
  when (io.invalidate) {
    vb_array := Bits(0)
    invalidated := Bool(true)
  }
  val s2_disparity = Wire(Vec(Bool(), nWays))
  for (i <- 0 until nWays)
    when (s2_valid && s2_disparity(i)) { vb_array := vb_array.bitSet(Cat(UInt(i), s2_idx), Bool(false)) }

  val s1_tag_match = Wire(Vec(Bool(), nWays))
  val s2_tag_hit = Wire(Vec(Bool(), nWays))
  val s2_dout = Reg(Vec(Bits(), nWays))

  for (i <- 0 until nWays) {
    val s1_vb = !io.invalidate && vb_array(Cat(UInt(i), s1_pgoff(untagBits-1,blockOffBits))).toBool
    val s2_vb = Reg(Bool())
    val s2_tag_disparity = Reg(Bool())
    val s2_tag_match = Reg(Bool())
    val tag_out = tag_rdata(entagbits*(i+1)-1, entagbits*i)
    when (s1_valid && rdy && !stall) {
      s2_vb := s1_vb
      s2_tag_disparity := code.decode(tag_out).error
      s2_tag_match := s1_tag_match(i)
    }
    s1_tag_match(i) := tag_out(tagBits-1,0) === s1_tag
    s2_tag_hit(i) := s2_vb && s2_tag_match
    s2_disparity(i) := s2_vb && (s2_tag_disparity || code.decode(s2_dout(i)).error)
  }
  s2_any_tag_hit := s2_tag_hit.reduceLeft(_||_) && !s2_disparity.reduceLeft(_||_)

  for (i <- 0 until nWays) {
    val data_array = SeqMem(Bits(width = code.width(rowBits)), nSets*refillCycles)
    val wen = narrow_grant.valid && repl_way === UInt(i)
    when (wen) {
      val e_d = code.encode(narrow_grant.bits.data).toUInt
      if(refillCycles > 1) data_array.write(Cat(s2_idx, refill_cnt), e_d)
      else data_array.write(s2_idx, e_d)
    }
    val s0_raddr = s0_pgoff(untagBits-1,blockOffBits-(if(refillCycles > 1) refill_cnt.getWidth else 0))
    val s1_rdata = data_array.read(s0_raddr, !wen && s0_valid)
    // if s1_tag_match is critical, replace with partial tag check
    when (s1_valid && rdy && !stall && (Bool(isDM) || s1_tag_match(i))) { s2_dout(i) := s1_rdata }
  }
  val s2_dout_word = s2_dout.map(x => (x >> (s2_offset(log2Up(rowBytes)-1,log2Up(coreInstBytes)) << log2Up(coreInstBits)))(coreInstBits-1,0))
  io.resp.bits.data := Mux1H(s2_tag_hit, s2_dout_word)
  io.resp.bits.datablock := Mux1H(s2_tag_hit, s2_dout)

  // output signals
  io.resp.valid := s2_hit
  io.mem.acquire.valid := (state === s_request)
  io.mem.acquire.bits := GetBlock(addr_block = s2_addr >> UInt(blockOffBits))

  // control state machine
  switch (state) {
    is (s_ready) {
      when (s2_miss) { state := s_request }
      invalidated := Bool(false)
    }
    is (s_request) {
      when (io.mem.acquire.ready) { state := s_refill_wait }
    }
    is (s_refill_wait) {
      when (io.mem.grant.valid) { state := s_refill }
    }
    is (s_refill) {
      when (refill_done) { state := s_ready }
    }
  }
}
