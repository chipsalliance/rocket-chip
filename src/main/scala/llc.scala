package uncore
import Chisel._

class BigMem[T <: Data](n: Int, preLatency: Int, postLatency: Int, leaf: Mem[UInt])(gen: => T) extends Module
{
  class Inputs extends Bundle {
    val addr = UInt(INPUT, log2Up(n))
    val rw = Bool(INPUT)
    val wdata = gen.asInput
    val wmask = gen.asInput
    override def clone = new Inputs().asInstanceOf[this.type]
  }
  val io = new Bundle {
    val in = Valid(new Inputs).flip
    val rdata = gen.asOutput
  }
  val data = gen
  val colMux = if (2*data.width <= leaf.data.width && n > leaf.n) 1 << math.floor(math.log(leaf.data.width/data.width)/math.log(2)).toInt else 1
  val nWide = if (data.width > leaf.data.width) 1+(data.width-1)/leaf.data.width else 1
  val nDeep = if (n > colMux*leaf.n) 1+(n-1)/(colMux*leaf.n) else 1
  if (nDeep > 1 || colMux > 1)
    require(isPow2(n) && isPow2(leaf.n))

  val rdataDeep = Vec.fill(nDeep){Bits()}
  val rdataSel = Vec.fill(nDeep){Bool()}
  for (i <- 0 until nDeep) {
    val in = Pipe(io.in.valid && (if (nDeep == 1) Bool(true) else UInt(i) === io.in.bits.addr(log2Up(n)-1, log2Up(n/nDeep))), io.in.bits, preLatency)
    val idx = in.bits.addr(log2Up(n/nDeep/colMux)-1, 0)
    val wdata = in.bits.wdata.toBits
    val wmask = in.bits.wmask.toBits
    val ren = in.valid && !in.bits.rw
    val reg_ren = Reg(next=ren)
    val rdata = Vec.fill(nWide){Bits()}

    val r = Pipe(ren, in.bits.addr, postLatency)

    for (j <- 0 until nWide) {
      val mem = leaf.clone
      var dout: Bits = null
      val ridx = if (postLatency > 0) Reg(Bits()) else null

      var wmask0 = Fill(colMux, wmask(math.min(wmask.getWidth, leaf.data.width*(j+1))-1, leaf.data.width*j))
      if (colMux > 1)
        wmask0 = wmask0 & FillInterleaved(gen.width, UIntToOH(in.bits.addr(log2Up(n/nDeep)-1, log2Up(n/nDeep/colMux)), log2Up(colMux)))
      val wdata0 = Fill(colMux, wdata(math.min(wdata.getWidth, leaf.data.width*(j+1))-1, leaf.data.width*j))
      when (in.valid) {
        when (in.bits.rw) { mem.write(idx, wdata0, wmask0) }
        .otherwise { if (postLatency > 0) ridx := idx }
      }

      if (postLatency == 0) {
        dout = mem(idx)
      } else if (postLatency == 1) {
        dout = mem(ridx)
      } else
        dout = Pipe(reg_ren, mem(ridx), postLatency-1).bits

      rdata(j) := dout
    }
    val rdataWide = rdata.reduceLeft((x, y) => Cat(y, x))

    var colMuxOut = rdataWide
    if (colMux > 1) {
      val colMuxIn = Vec((0 until colMux).map(k => rdataWide(gen.width*(k+1)-1, gen.width*k)))
      colMuxOut = colMuxIn(r.bits(log2Up(n/nDeep)-1, log2Up(n/nDeep/colMux)))
    }

    rdataDeep(i) := colMuxOut
    rdataSel(i) := r.valid
  }

  io.rdata := Mux1H(rdataSel, rdataDeep)
}

class LLCDataReq(ways: Int)(implicit conf: MemoryIFConfiguration) extends MemReqCmd
{
  val way = UInt(width = log2Up(ways))
  val isWriteback = Bool()
  override def clone = new LLCDataReq(ways)(conf).asInstanceOf[this.type]
}

class LLCTagReq(ways: Int)(implicit val conf: MemoryIFConfiguration) extends HasMemAddr
{
  val way = UInt(width = log2Up(ways))
  override def clone = new LLCTagReq(ways)(conf).asInstanceOf[this.type]
}

class LLCMSHRFile(sets: Int, ways: Int, outstanding: Int, refill_cycles: Int)(implicit conf: MemoryIFConfiguration) extends Module
{
  val io = new Bundle {
    val cpu = Decoupled(new MemReqCmd).flip
    val repl_way = UInt(INPUT, log2Up(ways))
    val repl_dirty = Bool(INPUT)
    val repl_tag = UInt(INPUT, conf.addrBits - log2Up(sets))
    val data = Decoupled(new LLCDataReq(ways))
    val tag = Decoupled(new LLCTagReq(ways))
    val mem = new MemPipeIO
    val mem_resp_set = UInt(OUTPUT, log2Up(sets))
    val mem_resp_way = UInt(OUTPUT, log2Up(ways))
  }

  class MSHR extends Bundle {
    val addr = UInt(width = conf.addrBits)
    val way = UInt(width = log2Up(ways))
    val tag = io.cpu.bits.tag.clone
    val refilled = Bool()
    val refillCount = UInt(width = log2Up(refill_cycles))
    val requested = Bool()
    val old_dirty = Bool()
    val old_tag = UInt(width = conf.addrBits - log2Up(sets))
    val wb_busy = Bool()

    override def clone = new MSHR().asInstanceOf[this.type]
  }

  val valid = Vec.fill(outstanding){Reg(init=Bool(false))}
  val validBits = valid.toBits
  val freeId = PriorityEncoder(~validBits)
  val mshr = Vec.fill(outstanding){Reg(new MSHR)}
  when (io.cpu.valid && io.cpu.ready) {
    valid(freeId) := Bool(true)
    mshr(freeId).addr := io.cpu.bits.addr
    mshr(freeId).tag := io.cpu.bits.tag
    mshr(freeId).way := io.repl_way
    mshr(freeId).old_dirty := io.repl_dirty
    mshr(freeId).old_tag := io.repl_tag
    mshr(freeId).wb_busy := Bool(false)
    mshr(freeId).requested := Bool(false)
    mshr(freeId).refillCount := UInt(0)
    mshr(freeId).refilled := Bool(false)
  }

  val requests = Cat(Bits(0), (outstanding-1 to 0 by -1).map(i => valid(i) && !mshr(i).old_dirty && !mshr(i).wb_busy && !mshr(i).requested):_*)
  val request = requests.orR && io.data.ready // allow in-flight hits to drain
  val requestId = PriorityEncoder(requests)
  when (io.mem.req_cmd.valid && io.mem.req_cmd.ready) { mshr(requestId).requested := Bool(true) }

  val refillId = io.mem.resp.bits.tag(log2Up(outstanding)-1, 0)
  val refillCount = mshr(refillId).refillCount
  when (io.mem.resp.valid) {
    mshr(refillId).refillCount := refillCount + UInt(1)
    when (refillCount === UInt(refill_cycles-1)) { mshr(refillId).refilled := Bool(true) }
  }

  val replays = Cat(Bits(0), (outstanding-1 to 0 by -1).map(i => valid(i) && mshr(i).refilled):_*)
  val replay = replays.orR
  val replayId = PriorityEncoder(replays)
  when (replay && io.data.ready && io.tag.ready) { valid(replayId) := Bool(false) }

  val writebacks = Cat(Bits(0), (outstanding-1 to 0 by -1).map(i => valid(i) && mshr(i).old_dirty):_*)
  val writeback = writebacks.orR
  val writebackId = PriorityEncoder(writebacks)
  when (writeback && io.data.ready && !replay) {
    mshr(writebackId).old_dirty := Bool(false)
    mshr(writebackId).wb_busy := Bool(true)
  }
  mshr.foreach(m => when (m.wb_busy && io.data.ready) { m.wb_busy := Bool(false) })

  val conflicts = Cat(Bits(0), (0 until outstanding).map(i => valid(i) && io.cpu.bits.addr(log2Up(sets)-1, 0) === mshr(i).addr(log2Up(sets)-1, 0)):_*)
  io.cpu.ready := !conflicts.orR && !validBits.andR

  io.data.valid := writeback
  io.data.bits.rw := Bool(false)
  io.data.bits.tag := mshr(replayId).tag
  io.data.bits.isWriteback := Bool(true)
  io.data.bits.addr := Cat(mshr(writebackId).old_tag, mshr(writebackId).addr(log2Up(sets)-1, 0)).toUInt
  io.data.bits.way := mshr(writebackId).way
  when (replay) {
    io.data.valid := io.tag.ready
    io.data.bits.isWriteback := Bool(false)
    io.data.bits.addr := mshr(replayId).addr
    io.data.bits.way := mshr(replayId).way
  }
  io.tag.valid := replay && io.data.ready
  io.tag.bits.addr := io.data.bits.addr
  io.tag.bits.way := io.data.bits.way

  io.mem.req_cmd.valid := request
  io.mem.req_cmd.bits.rw := Bool(false)
  io.mem.req_cmd.bits.addr := mshr(requestId).addr
  io.mem.req_cmd.bits.tag := requestId
  io.mem_resp_set := mshr(refillId).addr
  io.mem_resp_way := mshr(refillId).way
}

class LLCWriteback(requestors: Int, refill_cycles: Int)(implicit conf: MemoryIFConfiguration) extends Module
{
  val io = new Bundle {
    val req = Vec.fill(requestors){Decoupled(UInt(width = conf.addrBits)).flip }
    val data = Vec.fill(requestors){Decoupled(new MemData).flip }
    val mem = new MemPipeIO
  }

  val valid = Reg(init=Bool(false))
  val who = Reg(UInt())
  val addr = Reg(UInt())
  val cmd_sent = Reg(Bool())
  val data_sent = Reg(Bool())
  val count = Reg(init=UInt(0, log2Up(refill_cycles)))

  var anyReq = Bool(false)
  for (i <- 0 until requestors) {
    io.req(i).ready := !valid && !anyReq
    io.data(i).ready := valid && who === UInt(i) && io.mem.req_data.ready
    anyReq = anyReq || io.req(i).valid
  }

  val nextWho = PriorityEncoder(io.req.map(_.valid))
  when (!valid && io.req.map(_.valid).reduceLeft(_||_)) {
    valid := Bool(true)
    cmd_sent := Bool(false)
    data_sent := Bool(false)
    who := nextWho
    addr := io.req(nextWho).bits
  }

  when (io.mem.req_data.valid && io.mem.req_data.ready) {
    count := count + UInt(1)
    when (count === UInt(refill_cycles-1)) {
      data_sent := Bool(true)
      when (cmd_sent) { valid := Bool(false) }
    }
  }
  when (io.mem.req_cmd.valid && io.mem.req_cmd.ready) { cmd_sent := Bool(true) }
  when (valid && cmd_sent && data_sent) { valid := Bool(false) }

  io.mem.req_cmd.valid := valid && !cmd_sent
  io.mem.req_cmd.bits.addr := addr
  io.mem.req_cmd.bits.rw := Bool(true)

  io.mem.req_data.valid := valid && !data_sent && io.data(who).valid
  io.mem.req_data.bits := io.data(who).bits
}

class LLCData(latency: Int, sets: Int, ways: Int, refill_cycles: Int, leaf: Mem[UInt])(implicit conf: MemoryIFConfiguration) extends Module
{
  val io = new Bundle {
    val req = Decoupled(new LLCDataReq(ways)).flip
    val req_data = Decoupled(new MemData).flip
    val writeback = Decoupled(UInt(width = conf.addrBits))
    val writeback_data = Decoupled(new MemData)
    val resp = Decoupled(new MemResp)
    val mem_resp = Valid(new MemResp).flip
    val mem_resp_set = UInt(INPUT, log2Up(sets))
    val mem_resp_way = UInt(INPUT, log2Up(ways))
  }

  val data = Module(new BigMem(sets*ways*refill_cycles, 1, latency-1, leaf)(Bits(width = conf.dataBits)))
  class QEntry extends MemResp {
    val isWriteback = Bool()
    override def clone = new QEntry().asInstanceOf[this.type]
  }
  val q = Module(new Queue(new QEntry, latency+2))
  val qReady = q.io.count <= UInt(q.entries-latency-1)
  val valid = Reg(init=Bool(false))
  val req = Reg(io.req.bits.clone)
  val count = Reg(init=UInt(0, log2Up(refill_cycles)))
  val refillCount = Reg(init=UInt(0, log2Up(refill_cycles)))

  when (data.io.in.valid && !io.mem_resp.valid) {
    count := count + UInt(1)
    when (valid && count === UInt(refill_cycles-1)) { valid := Bool(false) }
  }
  when (io.req.valid && io.req.ready) { valid := Bool(true); req := io.req.bits }
  when (io.mem_resp.valid) { refillCount := refillCount + UInt(1) }

  data.io.in.valid := io.req.valid && io.req.ready && Mux(io.req.bits.rw, io.req_data.valid, qReady)
  data.io.in.bits.addr := Cat(io.req.bits.way, io.req.bits.addr(log2Up(sets)-1, 0), count).toUInt
  data.io.in.bits.rw := io.req.bits.rw
  data.io.in.bits.wdata := io.req_data.bits.data
  data.io.in.bits.wmask := SInt(-1, io.req_data.bits.data.width)
  when (valid) {
    data.io.in.valid := Mux(req.rw, io.req_data.valid, qReady)
    data.io.in.bits.addr := Cat(req.way, req.addr(log2Up(sets)-1, 0), count).toUInt
    data.io.in.bits.rw := req.rw
  }
  when (io.mem_resp.valid) {
    data.io.in.valid := Bool(true)
    data.io.in.bits.addr := Cat(io.mem_resp_way, io.mem_resp_set, refillCount).toUInt
    data.io.in.bits.rw := Bool(true)
    data.io.in.bits.wdata := io.mem_resp.bits.data
  }

  val tagPipe = Pipe(data.io.in.valid && !data.io.in.bits.rw, Mux(valid, req.tag, io.req.bits.tag), latency)
  q.io.enq.valid := tagPipe.valid
  q.io.enq.bits.tag := tagPipe.bits
  q.io.enq.bits.isWriteback := Pipe(Mux(valid, req.isWriteback, io.req.bits.isWriteback), Bool(false), latency).valid
  q.io.enq.bits.data := data.io.rdata

  io.req.ready := !valid && Mux(io.req.bits.isWriteback, io.writeback.ready, Bool(true))
  io.req_data.ready := !io.mem_resp.valid && Mux(valid, req.rw, io.req.valid && io.req.bits.rw)

  io.writeback.valid := io.req.valid && io.req.ready && io.req.bits.isWriteback
  io.writeback.bits := io.req.bits.addr

  q.io.deq.ready := Mux(q.io.deq.bits.isWriteback, io.writeback_data.ready, io.resp.ready)
  io.resp.valid := q.io.deq.valid && !q.io.deq.bits.isWriteback
  io.resp.bits := q.io.deq.bits
  io.writeback_data.valid := q.io.deq.valid && q.io.deq.bits.isWriteback
  io.writeback_data.bits := q.io.deq.bits
}

class MemReqArb(n: Int, refill_cycles: Int)(implicit conf: MemoryIFConfiguration) extends Module
{
  val io = new Bundle {
    val cpu = Vec.fill(n){new MemIO().flip}
    val mem = new MemIO
  }

  val lock = Reg(init=Bool(false))
  val locker = Reg(UInt())

  val arb = Module(new RRArbiter(new MemReqCmd, n))
  val respWho = io.mem.resp.bits.tag(log2Up(n)-1,0)
  val respTag = io.mem.resp.bits.tag >> UInt(log2Up(n))
  for (i <- 0 until n) {
    val me = UInt(i, log2Up(n))
    arb.io.in(i).valid := io.cpu(i).req_cmd.valid
    arb.io.in(i).bits := io.cpu(i).req_cmd.bits
    arb.io.in(i).bits.tag := Cat(io.cpu(i).req_cmd.bits.tag, me)
    io.cpu(i).req_cmd.ready := arb.io.in(i).ready
    io.cpu(i).req_data.ready := Bool(false)

    val getLock = io.cpu(i).req_cmd.fire() && io.cpu(i).req_cmd.bits.rw && !lock
    val haveLock = lock && locker === me
    when (getLock) {
      lock := Bool(true)
      locker := UInt(i)
    }
    when (getLock || haveLock) {
      io.cpu(i).req_data.ready := io.mem.req_data.ready
      io.mem.req_data.valid := Bool(true)
      io.mem.req_data.bits := io.cpu(i).req_data.bits
    }

    io.cpu(i).resp.valid := io.mem.resp.valid && respWho === me
    io.cpu(i).resp.bits := io.mem.resp.bits
    io.cpu(i).resp.bits.tag := respTag
  }
  io.mem.resp.ready := io.cpu(respWho).resp.ready

  val unlock = Counter(io.mem.req_data.fire(), refill_cycles)._2
  when (unlock) { lock := Bool(false) }
}

class DRAMSideLLC(sets: Int, ways: Int, outstanding: Int, refill_cycles: Int, tagLeaf: Mem[UInt], dataLeaf: Mem[UInt])(implicit conf: MemoryIFConfiguration) extends Module
{
  val io = new Bundle {
    val cpu = new MemIO().flip
    val mem = new MemPipeIO
  }

  val tagWidth = conf.addrBits - log2Up(sets)
  val metaWidth = tagWidth + 2 // valid + dirty

  val memCmdArb = Module(new Arbiter(new MemReqCmd, 2))
  val dataArb = Module(new Arbiter(new LLCDataReq(ways), 2))
  val mshr = Module(new LLCMSHRFile(sets, ways, outstanding, refill_cycles))
  val tags = Module(new BigMem(sets, 0, 1, tagLeaf)(Bits(width = metaWidth*ways)))
  val data = Module(new LLCData(4, sets, ways, refill_cycles, dataLeaf))
  val writeback = Module(new LLCWriteback(2, refill_cycles))

  val initCount = Reg(init=UInt(0, log2Up(sets+1)))
  val initialize = !initCount(log2Up(sets))
  when (initialize) { initCount := initCount + UInt(1) }

  val replay_s2 = Reg(init=Bool(false))
  val s2_valid = Reg(init=Bool(false))
  val s2 = Reg(new MemReqCmd)
  val s3_rdy = Bool()
  val replay_s2_rdy = Bool()

  val s1_valid = Reg(next = io.cpu.req_cmd.fire() || replay_s2 && replay_s2_rdy, init = Bool(false))
  val s1 = Reg(new MemReqCmd)
  when (io.cpu.req_cmd.fire()) { s1 := io.cpu.req_cmd.bits }
  when (replay_s2 && replay_s2_rdy) { s1 := s2 }

  s2_valid := s1_valid
  replay_s2 := s2_valid && !s3_rdy || replay_s2 && !replay_s2_rdy
  val s2_tags = Vec.fill(ways){Reg(Bits(width = metaWidth))}
  when (s1_valid) {
    s2 := s1
    for (i <- 0 until ways)
      s2_tags(i) := tags.io.rdata(metaWidth*(i+1)-1, metaWidth*i)
  }
  val s2_hits = s2_tags.map(t => t(tagWidth) && s2.addr(s2.addr.width-1, s2.addr.width-tagWidth) === t(tagWidth-1, 0))
  val s2_hit_way = OHToUInt(s2_hits)
  val s2_hit = s2_hits.reduceLeft(_||_)
  val s2_hit_dirty = s2_tags(s2_hit_way)(tagWidth+1)
  val repl_way = LFSR16(s2_valid)(log2Up(ways)-1, 0)
  val repl_tag = s2_tags(repl_way).toUInt
  val setDirty = s2_valid && s2.rw && s2_hit && !s2_hit_dirty

  val tag_we = initialize || setDirty || mshr.io.tag.fire()
  val tag_waddr = Mux(initialize, initCount, Mux(setDirty, s2.addr, mshr.io.tag.bits.addr))
  val tag_wdata = Cat(setDirty, !initialize, Mux(setDirty, s2.addr, mshr.io.tag.bits.addr)(mshr.io.tag.bits.addr.width-1, mshr.io.tag.bits.addr.width-tagWidth))
  val tag_wmask = Mux(initialize, SInt(-1, ways), UIntToOH(Mux(setDirty, s2_hit_way, mshr.io.tag.bits.way)))
  tags.io.in.valid := io.cpu.req_cmd.fire() || replay_s2 && replay_s2_rdy || tag_we
  tags.io.in.bits.addr := Mux(tag_we, tag_waddr, Mux(replay_s2, s2.addr, io.cpu.req_cmd.bits.addr)(log2Up(sets)-1,0))
  tags.io.in.bits.rw := tag_we
  tags.io.in.bits.wdata := Fill(ways, tag_wdata)
  tags.io.in.bits.wmask := FillInterleaved(metaWidth, tag_wmask)

  mshr.io.cpu.valid := s2_valid && !s2_hit && !s2.rw
  mshr.io.cpu.bits := s2
  mshr.io.repl_way := repl_way
  mshr.io.repl_dirty := repl_tag(tagWidth+1, tagWidth).andR
  mshr.io.repl_tag := repl_tag
  mshr.io.mem.resp := io.mem.resp
  mshr.io.tag.ready := !s1_valid && !s2_valid

  data.io.req <> dataArb.io.out
  data.io.mem_resp := io.mem.resp
  data.io.mem_resp_set := mshr.io.mem_resp_set
  data.io.mem_resp_way := mshr.io.mem_resp_way
  data.io.req_data.bits := io.cpu.req_data.bits
  data.io.req_data.valid := io.cpu.req_data.valid

  writeback.io.req(0) <> data.io.writeback
  writeback.io.data(0) <> data.io.writeback_data
  writeback.io.req(1).valid := s2_valid && !s2_hit && s2.rw && mshr.io.cpu.ready
  writeback.io.req(1).bits := s2.addr
  writeback.io.data(1).valid := io.cpu.req_data.valid
  writeback.io.data(1).bits := io.cpu.req_data.bits

  memCmdArb.io.in(0) <> mshr.io.mem.req_cmd
  memCmdArb.io.in(1) <> writeback.io.mem.req_cmd

  dataArb.io.in(0) <> mshr.io.data
  dataArb.io.in(1).valid := s2_valid && s2_hit && mshr.io.cpu.ready
  dataArb.io.in(1).bits := s2
  dataArb.io.in(1).bits.way := s2_hit_way
  dataArb.io.in(1).bits.isWriteback := Bool(false)

  s3_rdy := mshr.io.cpu.ready && Mux(s2_hit, dataArb.io.in(1).ready, !s2.rw || writeback.io.req(1).ready)
  replay_s2_rdy := s3_rdy && !tag_we

  io.cpu.resp <> data.io.resp
  io.cpu.req_cmd.ready := !s1_valid && !s2_valid && !replay_s2 && !tag_we
  io.cpu.req_data.ready := writeback.io.data(1).ready || data.io.req_data.ready
  io.mem.req_cmd <> memCmdArb.io.out
  io.mem.req_data <> writeback.io.mem.req_data
}

class HellaFlowQueue[T <: Data](val entries: Int)(data: => T) extends Module
{
  val io = new QueueIO(data, entries)
  require(isPow2(entries) && entries > 1)

  val do_flow = Bool()
  val do_enq = io.enq.fire() && !do_flow
  val do_deq = io.deq.fire() && !do_flow

  val maybe_full = Reg(init=Bool(false))
  val enq_ptr = Counter(do_enq, entries)._1
  val deq_ptr = Counter(do_deq, entries)._1
  when (do_enq != do_deq) { maybe_full := do_enq }

  val ptr_match = enq_ptr === deq_ptr
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val atLeastTwo = full || enq_ptr - deq_ptr >= UInt(2)
  do_flow := empty && io.deq.ready

  val ram = Mem(data, entries, seqRead = true)
  val ram_addr = Reg(Bits())
  val ram_out_valid = Reg(Bool())
  ram_out_valid := Bool(false)
  when (do_enq) { ram(enq_ptr) := io.enq.bits }
  when (io.deq.ready && (atLeastTwo || !io.deq.valid && !empty)) {
    ram_out_valid := Bool(true)
    ram_addr := Mux(io.deq.valid, deq_ptr + UInt(1), deq_ptr)
  }

  io.deq.valid := Mux(empty, io.enq.valid, ram_out_valid)
  io.enq.ready := !full
  io.deq.bits := Mux(empty, io.enq.bits, ram(ram_addr))
}

class HellaQueue[T <: Data](val entries: Int)(data: => T) extends Module
{
  val io = new QueueIO(data, entries)

  val fq = Module(new HellaFlowQueue(entries)(data))
  io.enq <> fq.io.enq
  io.deq <> Queue(fq.io.deq, 1, pipe = true)
}

object HellaQueue
{
  def apply[T <: Data](enq: DecoupledIO[T], entries: Int) = {
    val q = Module((new HellaQueue(entries)) { enq.bits.clone })
    q.io.enq.valid := enq.valid // not using <> so that override is allowed
    q.io.enq.bits := enq.bits
    enq.ready := q.io.enq.ready
    q.io.deq
  }
}

class DRAMSideLLCNull(numRequests: Int, refillCycles: Int)(implicit conf: MemoryIFConfiguration) extends Module
{
  val io = new Bundle {
    val cpu = new MemIO().flip
    val mem = new MemPipeIO
  }

  val numEntries = numRequests * refillCycles
  val size = log2Down(numEntries) + 1

  val inc = Bool()
  val dec = Bool()
  val count = Reg(init=UInt(numEntries, size))
  val watermark = count >= UInt(refillCycles)

  when (inc && !dec) {
    count := count + UInt(1)
  }
  when (!inc && dec) {
    count := count - UInt(refillCycles)
  }
  when (inc && dec) {
    count := count - UInt(refillCycles-1)
  }

  val cmdq_mask = io.cpu.req_cmd.bits.rw || watermark

  io.mem.req_cmd.valid := io.cpu.req_cmd.valid && cmdq_mask
  io.cpu.req_cmd.ready := io.mem.req_cmd.ready && cmdq_mask
  io.mem.req_cmd.bits := io.cpu.req_cmd.bits

  io.mem.req_data <> io.cpu.req_data

  val resp_dataq = Module((new HellaQueue(numEntries)) { new MemResp })
  resp_dataq.io.enq <> io.mem.resp
  io.cpu.resp <> resp_dataq.io.deq

  inc := resp_dataq.io.deq.fire()
  dec := io.mem.req_cmd.fire() && !io.mem.req_cmd.bits.rw
}
