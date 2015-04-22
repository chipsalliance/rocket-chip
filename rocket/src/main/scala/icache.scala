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
  val pc = UInt(width = vaddrBits+1)
}

class FrontendResp extends CoreBundle {
  val pc = UInt(width = vaddrBits+1)  // ID stage PC
  val data = Vec.fill(coreFetchWidth) (Bits(width = coreInstBits))
  val mask = Bits(width = coreFetchWidth)
  val xcpt_if = Bool()
}

class CPUFrontendIO extends Bundle {
  val req = Valid(new FrontendReq)
  val resp = Decoupled(new FrontendResp).flip
  val btb_resp = Valid(new BTBResp).flip
  val btb_update = Valid(new BTBUpdate)
  val bht_update = Valid(new BHTUpdate)
  val ras_update = Valid(new RASUpdate)
  val invalidate = Bool(OUTPUT)
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
  val s2_btb_resp_bits = Reg(btb.io.resp.bits.clone)
  val s2_xcpt_if = Reg(init=Bool(false))
  val icbuf = Module(new Queue(new ICacheResp, 1, pipe=true))

  val msb = vaddrBits-1
  val lsb = log2Up(coreFetchWidth*coreInstBytes)
  val btbTarget = Cat(btb.io.resp.bits.target(msb), btb.io.resp.bits.target)
  val ntpc_0 = s1_pc + UInt(coreInstBytes*coreFetchWidth)
  val ntpc = Cat(s1_pc(msb) & ntpc_0(msb), ntpc_0(msb,lsb), Bits(0,lsb)) // unsure
  val icmiss = s2_valid && !icbuf.io.deq.valid
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
  icache.io.req.bits.idx := Mux(io.cpu.req.valid, io.cpu.req.bits.pc, npc)
  icache.io.invalidate := io.cpu.invalidate
  icache.io.req.bits.ppn := tlb.io.resp.ppn
  icache.io.req.bits.kill := io.cpu.req.valid || tlb.io.resp.miss || icmiss || io.ptw.invalidate

  io.cpu.resp.valid := s2_valid && (s2_xcpt_if || icbuf.io.deq.valid)
  io.cpu.resp.bits.pc := s2_pc

  icbuf.io.enq <> icache.io.resp
  icbuf.io.deq.ready := !stall && !s1_same_block

  require(coreFetchWidth * coreInstBytes <= rowBytes)
  val fetch_data =
    if (coreFetchWidth * coreInstBytes == rowBytes) icbuf.io.deq.bits.datablock
    else icbuf.io.deq.bits.datablock >> (s2_pc(log2Up(rowBytes)-1,log2Up(coreFetchWidth*coreInstBytes)) << log2Up(coreFetchWidth*coreInstBits))

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
  val rdy = Bool()

  val refill_addr = Reg(UInt(width = paddrBits))
  val s1_any_tag_hit = Bool()

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

  val out_valid = s1_valid && !io.req.bits.kill && state === s_ready
  val s1_idx = s1_addr(untagBits-1,blockOffBits)
  val s1_offset = s1_addr(blockOffBits-1,0)
  val s1_hit = out_valid && s1_any_tag_hit
  val s1_miss = out_valid && !s1_any_tag_hit
  rdy := state === s_ready && !s1_miss

  when (s1_valid && state === s_ready && s1_miss) {
    refill_addr := s1_addr
  }
  val refill_tag = refill_addr(tagBits+untagBits-1,untagBits)

  val narrow_grant = FlowThroughSerializer(io.mem.grant, refillCyclesPerBeat)
  val (refill_cnt, refill_wrap) = Counter(narrow_grant.fire(), refillCycles) //TODO Zero width wire
  val refill_done = state === s_refill && refill_wrap
  narrow_grant.ready := Bool(true)

  val repl_way = if (isDM) UInt(0) else LFSR16(s1_miss)(log2Up(nWays)-1,0)
  val entagbits = code.width(tagBits)
  val tag_array = Mem(Bits(width = entagbits*nWays), nSets, seqRead = true)
  val tag_raddr = Reg(UInt())
  when (refill_done) {
    val wmask = FillInterleaved(entagbits, if (isDM) Bits(1) else UIntToOH(repl_way))
    val tag = code.encode(refill_tag).toUInt
    tag_array.write(s1_idx, Fill(nWays, tag), wmask)
  }
//  /*.else*/when (s0_valid) { // uncomment ".else" to infer 6T SRAM
  .elsewhen (s0_valid) {
    tag_raddr := s0_pgoff(untagBits-1,blockOffBits)
  }

  val vb_array = Reg(init=Bits(0, nSets*nWays))
  when (refill_done && !invalidated) {
    vb_array := vb_array.bitSet(Cat(repl_way, s1_idx), Bool(true))
  }
  when (io.invalidate) {
    vb_array := Bits(0)
    invalidated := Bool(true)
  }
  val s1_disparity = Vec.fill(nWays){Bool()}
  for (i <- 0 until nWays)
    when (s1_valid && s1_disparity(i)) { vb_array := vb_array.bitSet(Cat(UInt(i), s1_idx), Bool(false)) }

  val s1_tag_match = Vec.fill(nWays){Bool()}
  val s1_tag_hit = Vec.fill(nWays){Bool()}
  val s1_dout = Vec.fill(nWays){(Bits())}

  for (i <- 0 until nWays) {
    val s1_vb = !io.invalidate && vb_array(Cat(UInt(i), s1_pgoff(untagBits-1,blockOffBits))).toBool
    val tag_out = tag_array(tag_raddr)(entagbits*(i+1)-1, entagbits*i)
    val s1_tag_disparity = code.decode(tag_out).error
    when (s1_valid && rdy && !stall) {
    }
    s1_tag_match(i) := tag_out(tagBits-1,0) === s1_tag
    s1_tag_hit(i) := s1_vb && s1_tag_match(i)
    s1_disparity(i) := s1_vb && (s1_tag_disparity || code.decode(s1_dout(i)).error)
  }
  s1_any_tag_hit := s1_tag_hit.reduceLeft(_||_) && !s1_disparity.reduceLeft(_||_)

  for (i <- 0 until nWays) {
    val data_array = Mem(Bits(width = code.width(rowBits)), nSets*refillCycles, seqRead = true)
    val s1_raddr = Reg(UInt())
    when (narrow_grant.valid && repl_way === UInt(i)) {
      val e_d = code.encode(narrow_grant.bits.data)
      if(refillCycles > 1) data_array(Cat(s1_idx, refill_cnt)) := e_d
      else data_array(s1_idx) := e_d
    }
//    /*.else*/when (s0_valid) { // uncomment ".else" to infer 6T SRAM
    .elsewhen (s0_valid) {
      s1_raddr := s0_pgoff(untagBits-1,blockOffBits-(if(refillCycles > 1) refill_cnt.getWidth else 0))
    }
    // if s1_tag_match is critical, replace with partial tag check
    s1_dout(i) := 0
    when (s1_valid && rdy && !stall && (Bool(isDM) || s1_tag_match(i))) { s1_dout(i) := data_array(s1_raddr) }
  }
  io.resp.bits.datablock := Mux1H(s1_tag_hit, s1_dout)

  // output signals
  io.resp.valid := s1_hit
  io.mem.acquire.valid := (state === s_request)
  io.mem.acquire.bits := GetBlock(addr_block = refill_addr >> UInt(blockOffBits))

  // control state machine
  switch (state) {
    is (s_ready) {
      when (s1_miss) { state := s_request }
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
