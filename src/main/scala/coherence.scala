package rocket

import Chisel._
import Constants._

class MemData extends Bundle {
  val data = Bits(width = MEM_DATA_BITS)
}

class MemReqCmd() extends Bundle 
{
  val rw = Bool()
  val addr = UFix(width = PADDR_BITS - OFFSET_BITS)
  val tag = Bits(width = MEM_TAG_BITS)
}

class MemResp () extends MemData
{
  val tag = Bits(width = MEM_TAG_BITS)
}

class ioMem() extends Bundle
{
  val req_cmd  = (new ioDecoupled) { new MemReqCmd() }.flip
  val req_data = (new ioDecoupled) { new MemData() }.flip
  val resp     = (new ioValid) { new MemResp() }
}

class HubMemReq extends Bundle {
  val lock   = Bool() 
}

class TrackerProbeData extends Bundle {
  val valid = Bool()
  val data_tile_id = Bits(width = log2up(NTILES))
}

class TrackerAllocReq extends Bundle {
  val xact_init = new TransactionInit()
  val init_tile_id = Bits(width = TILE_ID_BITS)
  val data_valid = Bool()
}


class TransactionInit extends Bundle {
  val t_type = Bits(width = TTYPE_BITS)
  val has_data = Bool()
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
  val address = UFix(width = PADDR_BITS)
}

class TransactionInitData extends MemData

class TransactionAbort extends Bundle {
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
}

class ProbeRequest extends Bundle {
  val p_type = Bits(width = PTYPE_BITS)
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
  val address = Bits(width = PADDR_BITS)
}

class ProbeReply extends Bundle {
  val p_type = Bits(width = PTYPE_BITS)
  val has_data = Bool()
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
}

class ProbeReplyData extends MemData

class TransactionReply extends MemData {
  val t_type = Bits(width = TTYPE_BITS)
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
}

class TransactionFinish extends Bundle {
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
}

class ioTileLink extends Bundle { 
  val xact_init      = (new ioDecoupled) { new TransactionInit() }.flip
  val xact_init_data = (new ioDecoupled) { new TransactionInitData() }.flip
  val xact_abort     = (new ioDecoupled) { new TransactionAbort() }
  val probe_req      = (new ioDecoupled) { new ProbeRequest() }
  val probe_rep      = (new ioDecoupled) { new ProbeReply() }.flip
  val probe_rep_data = (new ioDecoupled) { new ProbeReplyData() }.flip
  val xact_rep       = (new ioValid)     { new TransactionReply() }
  val xact_finish    = (new ioDecoupled) { new TransactionFinish() }.flip
}

trait CoherencePolicy {
  def cpuCmdToRW( cmd: Bits): (Bool, Bool) = {
    val store   = (cmd === M_XWR)
    val load    = (cmd === M_XRD)
    val amo     = cmd(3).toBool
    val read    = load  || amo || (cmd === M_PFR)
    val write   = store || amo || (cmd === M_PFW)
    (read, write)
  }
}

trait ThreeStateIncoherence extends CoherencePolicy {
  val tileInvalid :: tileClean :: tileDirty :: Nil = Enum(3){ UFix() }

  def isHit ( cmd: Bits, state: UFix): Bool = {
    val (read, write) = cpuCmdToRW(cmd)
    ( state === tileClean || state === tileDirty)
  }

  def isValid (state: UFix): Bool = {
    state != tileInvalid
  }

  def needsWriteback (state: UFix): Bool = {
    state === tileDirty
  }

  def newStateOnWriteback() = tileInvalid
  def newStateOnFlush() = tileInvalid
  def newState(cmd: Bits, state: UFix): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, tileDirty, Mux(read, Mux(state === tileDirty, tileDirty, tileClean), state))
  }
  def newStateOnHit(cmd: Bits, state: UFix): UFix = newState(cmd, state)
  def newStateOnPrimaryMiss(cmd: Bits): UFix = newState(cmd, tileInvalid)
  def newStateOnSecondaryMiss(cmd: Bits, state: UFix): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, tileDirty, state)
  }

}

trait FourStateCoherence extends CoherencePolicy {

  val tileInvalid :: tileShared :: tileExclusiveClean :: tileExclusiveDirty :: Nil = Enum(4){ UFix() }
  val globalInvalid :: globalShared :: globalExclusiveClean :: Nil = Enum(3){ UFix() }
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(3){ UFix() }

  def isHit ( cmd: Bits, state: UFix): Bool = {
    val (read, write) = cpuCmdToRW(cmd)
    ((read && ( state === tileShared || state === tileExclusiveClean || state === tileExclusiveDirty)) ||
     (write && (state === tileExclusiveClean || state === tileExclusiveDirty)))
  }

  def isValid (state: UFix): Bool = {
    state != tileInvalid
  }

  def needsWriteback (state: UFix): Bool = {
    state === tileExclusiveDirty
  }

  def newStateOnWriteback() = tileInvalid
  def newStateOnFlush() = tileInvalid

  // TODO: New funcs as compared to incoherent protocol:
  def newState(cmd: Bits, state: UFix): UFix
  def newStateOnHit(cmd: Bits, state: UFix): UFix 
  def newStateOnPrimaryMiss(cmd: Bits): UFix 
  def newStateOnSecondaryMiss(cmd: Bits, state: UFix): UFix 

  def needsSecondaryXact (cmd: Bits, outstanding: TransactionInit): Bool

  def getMetaUpdateOnProbe (incoming: ProbeRequest): Bits = {
    val state = UFix(0)
    switch(incoming.p_type) {
      is(probeInvalidate) { state := tileInvalid }
      is(probeDowngrade) { state := tileShared }
    }
    state.toBits
  }

  def replyTypeHasData (reply: TransactionReply): Bool = {
    (reply.t_type != X_WRITE_UNCACHED)
  }
}

class XactTracker(id: Int) extends Component with CoherencePolicy {
  val io = new Bundle {
    val alloc_req       = (new ioDecoupled) { new TrackerAllocReq() }
    val probe_data      = (new TrackerProbeData).asInput
    val can_alloc       = Bool(INPUT)
    val xact_finish     = Bool(INPUT)
    val p_rep_cnt_dec   = Bits(NTILES, INPUT)
    val p_req_cnt_inc   = Bits(NTILES, INPUT)
    val p_rep_data      = (new ioDecoupled) { new ProbeReplyData() }
    val x_init_data     = (new ioDecoupled) { new TransactionInitData() }

    val mem_req_cmd     = (new ioDecoupled) { new MemReqCmd() }
    val mem_req_data    = (new ioDecoupled) { new MemData() }
    val mem_req_lock    = Bool(OUTPUT)
    val probe_req       = (new ioDecoupled) { new ProbeRequest() }.flip
    val busy            = Bool(OUTPUT)
    val addr            = Bits(PADDR_BITS, OUTPUT)
    val init_tile_id    = Bits(TILE_ID_BITS, OUTPUT)
    val p_rep_tile_id   = Bits(log2up(NTILES), INPUT)
    val tile_xact_id    = Bits(TILE_XACT_ID_BITS, OUTPUT)
    val sharer_count    = Bits(TILE_ID_BITS, OUTPUT)
    val t_type          = Bits(TTYPE_BITS, OUTPUT)
    val push_p_req      = Bits(NTILES, OUTPUT)
    val pop_p_rep       = Bits(NTILES, OUTPUT)
    val pop_p_rep_data  = Bits(NTILES, OUTPUT)
    val pop_x_init      = Bool(OUTPUT)
    val pop_x_init_data = Bool(OUTPUT)
    val send_x_rep_ack  = Bool(OUTPUT)
  }

  def sendProbeReqType(t_type: UFix, global_state: UFix): UFix = {
      MuxCase(P_COPY, Array((t_type === X_READ_SHARED)    -> P_DOWNGRADE,
                            (t_type === X_READ_EXCLUSIVE) -> P_INVALIDATE, 
                            (t_type === X_READ_UNCACHED)  -> P_COPY, 
                            (t_type === X_WRITE_UNCACHED) -> P_INVALIDATE))
  }

  def needsMemRead(t_type: UFix, global_state: UFix): Bool = {
      (t_type != X_WRITE_UNCACHED)
  }

  def needsAckRep(t_type: UFix, global_state: UFix): Bool = {
      (t_type === X_WRITE_UNCACHED)
  }

  val s_idle :: s_mem :: s_probe :: s_busy :: Nil = Enum(4){ UFix() }
  val state = Reg(resetVal = s_idle)
  val addr_  = Reg{ UFix() }
  val t_type_ = Reg{ Bits() }
  val init_tile_id_ = Reg{ Bits() }
  val tile_xact_id_ = Reg{ Bits() }
  val p_rep_count = Reg(resetVal = UFix(0, width = log2up(NTILES)))
  val p_req_flags = Reg(resetVal = Bits(0, width = NTILES))
  val p_rep_tile_id_ = Reg{ Bits() }
  val x_needs_read = Reg(resetVal = Bool(false))
  val x_init_data_needs_write = Reg(resetVal = Bool(false))
  val p_rep_data_needs_write = Reg(resetVal = Bool(false))
  val mem_cmd_sent = Reg(resetVal = Bool(false))
  val mem_cnt = Reg(resetVal = UFix(0, width = log2up(REFILL_CYCLES)))
  val mem_cnt_next = mem_cnt + UFix(1)

  def doMemReqWrite(req_cmd: ioDecoupled[MemReqCmd], req_data: ioDecoupled[MemData], lock: Bool,  data: ioDecoupled[MemData], trigger: Bool, pop: Bool) {
    req_cmd.valid := mem_cmd_sent
    req_cmd.bits.rw := Bool(true)
    req_data <> data
    lock := Bool(true)
    when(req_cmd.ready && req_cmd.valid) {
      mem_cmd_sent := Bool(false)
    }
    when(req_data.ready && req_data.valid) {
      pop := Bool(true)
      mem_cnt := mem_cnt_next
    }
    when(mem_cnt === ~UFix(0)) {
      trigger := Bool(false)
    }
  }

  def doMemReqRead(req_cmd: ioDecoupled[MemReqCmd], trigger: Bool) {
    req_cmd.valid := Bool(true)
    req_cmd.bits.rw := Bool(false)
    when(req_cmd.ready ) {
      trigger := Bool(false)
    }
  }

  io.busy := state != s_idle
  io.addr := addr_
  io.init_tile_id := init_tile_id_
  io.tile_xact_id := tile_xact_id_
  io.sharer_count := UFix(NTILES) // TODO: Broadcast only
  io.t_type := t_type_

  io.mem_req_cmd.valid := Bool(false)
  io.mem_req_cmd.bits.rw := Bool(false)
  io.mem_req_cmd.bits.addr := addr_
  io.mem_req_cmd.bits.tag := UFix(id)
  io.mem_req_data.valid := Bool(false)
  io.mem_req_data.bits.data := UFix(0)
  io.mem_req_lock := Bool(false)
  io.probe_req.valid := Bool(false)
  io.probe_req.bits.p_type := sendProbeReqType(t_type_, UFix(0))
  io.probe_req.bits.global_xact_id := UFix(id)
  io.probe_req.bits.address := addr_
  io.push_p_req      := Bits(0, width = NTILES)
  io.pop_p_rep       := Bits(0, width = NTILES)
  io.pop_p_rep_data  := Bits(0, width = NTILES)
  io.pop_x_init      := Bool(false)
  io.pop_x_init_data := Bool(false)
  io.send_x_rep_ack  := Bool(false)

  switch (state) {
    is(s_idle) {
      when( io.alloc_req.valid && io.can_alloc ) {
        addr_ := io.alloc_req.bits.xact_init.address
        t_type_ := io.alloc_req.bits.xact_init.t_type
        init_tile_id_ := io.alloc_req.bits.init_tile_id
        tile_xact_id_ := io.alloc_req.bits.xact_init.tile_xact_id
        x_init_data_needs_write := io.alloc_req.bits.xact_init.has_data
        x_needs_read := needsMemRead(io.alloc_req.bits.xact_init.t_type, UFix(0))
        p_rep_count := UFix(NTILES-1)
        p_req_flags := ~( UFix(1) << io.alloc_req.bits.init_tile_id )
        state := Mux(p_req_flags.orR, s_probe, s_mem)
        mem_cnt := UFix(0)
        mem_cmd_sent := Bool(false)
        io.pop_x_init := Bool(true)
      }
    }
    is(s_probe) {
      when(p_req_flags.orR) {
        io.push_p_req := p_req_flags
        io.probe_req.valid := Bool(true)
      }
      when(io.p_req_cnt_inc.orR) {
        p_req_flags := p_req_flags & ~io.p_req_cnt_inc // unflag sent reqs
      }
      when(io.p_rep_cnt_dec.orR) {
        val p_rep_count_next = p_rep_count - PopCount(io.p_rep_cnt_dec)
        io.pop_p_rep := io.p_rep_cnt_dec
        p_rep_count := p_rep_count_next
        when(p_rep_count_next === UFix(0)) {
          mem_cnt := UFix(0)
          mem_cmd_sent := Bool(false)
          state := s_mem
        }
      }
      when(io.probe_data.valid) {
        p_rep_data_needs_write := Bool(true)
        p_rep_tile_id_ := io.p_rep_tile_id
      }
    }
    is(s_mem) {
      when (p_rep_data_needs_write) {
        doMemReqWrite(io.mem_req_cmd, io.mem_req_data, io.mem_req_lock, io.p_rep_data, p_rep_data_needs_write, io.pop_p_rep_data)
      } . elsewhen(x_init_data_needs_write) {
        doMemReqWrite(io.mem_req_cmd, io.mem_req_data, io.mem_req_lock, io.x_init_data, x_init_data_needs_write, io.pop_x_init_data)
      } . elsewhen (x_needs_read) {    
        doMemReqRead(io.mem_req_cmd, x_needs_read)
      } . otherwise { 
        io.send_x_rep_ack := needsAckRep(t_type_, UFix(0))
        state := s_busy 
      }
    }
    is(s_busy) { // Nothing left to do but wait for transaction to complete
      when (io.xact_finish) {
        state := s_idle
      }
    }
  }

  //TODO: Decrement the probe count when final data piece is written
  //      Connent io.mem.ready sig to correct pop* outputs
  //      P_rep and x_init must be popped on same cycle of receipt
}

abstract class CoherenceHub extends Component with CoherencePolicy {
  val io = new Bundle {
    val tiles = Vec(NTILES) { new ioTileLink() }.flip
    val mem = new ioMem
  }
}

class CoherenceHubNull extends CoherenceHub {

  val x_init = io.tiles(0).xact_init
  val is_write = x_init.bits.t_type === X_WRITE_UNCACHED
  x_init.ready := io.mem.req_cmd.ready && !(is_write && io.mem.resp.valid) //stall write req/resp to handle previous read resp
  io.mem.req_cmd.valid   := x_init.valid && !(is_write && io.mem.resp.valid)
  io.mem.req_cmd.bits.rw    := is_write
  io.mem.req_cmd.bits.tag   := x_init.bits.tile_xact_id
  io.mem.req_cmd.bits.addr  := x_init.bits.address
  io.mem.req_data <> io.tiles(0).xact_init_data

  val x_rep = io.tiles(0).xact_rep
  x_rep.bits.t_type := Mux(io.mem.resp.valid, X_READ_EXCLUSIVE, X_WRITE_UNCACHED)
  x_rep.bits.tile_xact_id := Mux(io.mem.resp.valid, io.mem.resp.bits.tag, x_init.bits.tile_xact_id)
  x_rep.bits.global_xact_id := UFix(0) // don't care
  x_rep.bits.data := io.mem.resp.bits.data
  x_rep.valid := io.mem.resp.valid || x_init.valid && is_write
}


class CoherenceHubBroadcast extends CoherenceHub {

  def coherenceConflict(addr1: Bits, addr2: Bits): Bool = {
    addr1(PADDR_BITS-1, OFFSET_BITS) === addr2(PADDR_BITS-1, OFFSET_BITS)
  }
  def getTransactionReplyType(t_type: UFix, count: UFix): Bits = {
    val ret = Wire() { Bits(width = TTYPE_BITS) }
    switch (t_type) {
      is(X_READ_SHARED) { ret := Mux(count > UFix(0), X_READ_SHARED, X_READ_EXCLUSIVE) }
      is(X_READ_EXCLUSIVE) { ret := X_READ_EXCLUSIVE }
      is(X_READ_UNCACHED)  { ret := X_READ_UNCACHED  }
      is(X_WRITE_UNCACHED) { ret := X_WRITE_UNCACHED }
    }
    ret 
  }

  val trackerList = (0 until NGLOBAL_XACTS).map(new XactTracker(_))

/*
  val busy_arr           = Vec(NGLOBAL_XACTS){ Wire(){Bool()} }
  val addr_arr           = Vec(NGLOBAL_XACTS){ Wire(){Bits(width=PADDR_BITS)} }
  val init_tile_id_arr   = Vec(NGLOBAL_XACTS){ Wire(){Bits(width=TILE_ID_BITS)} }
  val tile_xact_id_arr   = Vec(NGLOBAL_XACTS){ Wire(){Bits(width=TILE_XACT_ID_BITS)} }
  val t_type_arr         = Vec(NGLOBAL_XACTS){ Wire(){Bits(width=TTYPE_BITS)} }
  val sh_count_arr       = Vec(NGLOBAL_XACTS){ Wire(){Bits(width=TILE_ID_BITS)} }
  val send_x_rep_ack_arr = Vec(NGLOBAL_XACTS){ Wire(){Bool()} }

  val do_free_arr        = Vec(NGLOBAL_XACTS){ Wire(){Bool()} }
  val p_rep_cnt_dec_arr  = Vec(NGLOBAL_XACTS){ Wire(){Bits(width=NTILES)} }
  val p_req_cnt_inc_arr  = Vec(NGLOBAL_XACTS){ Wire(){Bits(width=NTILES)} }

  for( i <- 0 until NGLOBAL_XACTS) {
    busy_arr.write(          UFix(i), trackerList(i).io.busy)
    addr_arr.write(          UFix(i), trackerList(i).io.addr)
    init_tile_id_arr.write(  UFix(i), trackerList(i).io.init_tile_id)
    tile_xact_id_arr.write(  UFix(i), trackerList(i).io.tile_xact_id)
    t_type_arr.write(        UFix(i), trackerList(i).io.t_type)
    sh_count_arr.write(      UFix(i), trackerList(i).io.sharer_count)
    send_x_rep_ack_arr.write(UFix(i), trackerList(i).io.send_x_rep_ack)
    trackerList(i).io.xact_finish    := do_free_arr.read(UFix(i))
    trackerList(i).io.p_rep_cnt_dec  := p_rep_cnt_dec_arr.read(UFix(i))
    trackerList(i).io.p_req_cnt_inc  := p_req_cnt_inc_arr.read(UFix(i))
  }

  // Free finished transactions
  for( j <- 0 until NTILES ) {
    val finish = io.tiles(j).xact_finish
    do_free_arr.write(finish.bits.global_xact_id, finish.valid)
    finish.ready := Bool(true)
  }

  // Reply to initial requestor
  // Forward memory responses from mem to tile
  val idx = io.mem.resp.bits.tag
  for( j <- 0 until NTILES ) {
    io.tiles(j).xact_rep.bits.t_type := getTransactionReplyType(t_type_arr.read(idx), sh_count_arr.read(idx))
    io.tiles(j).xact_rep.bits.tile_xact_id := tile_xact_id_arr.read(idx)
    io.tiles(j).xact_rep.bits.global_xact_id := idx
    io.tiles(j).xact_rep.bits.data := io.mem.resp.bits.data
    io.tiles(j).xact_rep.valid := (UFix(j) === init_tile_id_arr.read(idx)) && (io.mem.resp.valid || send_x_rep_ack_arr.read(idx))
  }
  // If there were a ready signal due to e.g. intervening network use:
  //io.mem.resp.ready  := io.tiles(init_tile_id_arr.read(idx)).xact_rep.ready
  
  // Create an arbiter for the one memory port
  // We have to arbitrate between the different trackers' memory requests
  // and once we have picked a request, get the right write data
  val mem_req_cmd_arb = (new LockingArbiter(NGLOBAL_XACTS)) { new MemReqCmd() }
  val mem_req_data_arb = (new LockingArbiter(NGLOBAL_XACTS)) { new MemData() }
  for( i <- 0 until NGLOBAL_XACTS ) {
    mem_req_cmd_arb.io.in(i)    <> trackerList(i).io.mem_req_cmd
    mem_req_cmd_arb.io.lock(i)  <> trackerList(i).io.mem_req_lock
    mem_req_data_arb.io.in(i)   <> trackerList(i).io.mem_req_data
    mem_req_data_arb.io.lock(i) <> trackerList(i).io.mem_req_lock
  }
  io.mem.req_cmd  <> mem_req_cmd_arb.io.out
  io.mem.req_data <> mem_req_data_arb.io.out
  
  // Handle probe replies, which may or may not have data
  for( j <- 0 until NTILES ) {
    val p_rep = io.tiles(j).probe_rep
    val p_rep_data = io.tiles(j).probe_rep_data
    val idx = p_rep.bits.global_xact_id
    p_rep.ready := foldR(trackerList.map(_.io.pop_p_rep(j)))(_ || _)
    p_rep_data.ready  := foldR(trackerList.map(_.io.pop_p_rep_data(j)))(_ || _)
  }
  for( i <- 0 until NGLOBAL_XACTS ) {
    trackerList(i).io.p_rep_data <> io.tiles(trackerList(i).io.p_rep_tile_id).probe_rep_data
    for( j <- 0 until NTILES) {
      val p_rep = io.tiles(j).probe_rep
      val dec = p_rep.valid && (p_rep.bits.global_xact_id === UFix(i))
      p_rep_cnt_dec_arr(UFix(i)) := p_rep_cnt_dec_arr(UFix(i)).bitSet(UFix(j), dec)
    }
  }

  // Nack conflicting transaction init attempts
  val aborting   = Wire() { Bits(width = NTILES) } 
  for( j <- 0 until NTILES ) {
    val x_init = io.tiles(j).xact_init
    val x_abort  = io.tiles(j).xact_abort
    val conflicts = Bits(width = NGLOBAL_XACTS) 
    for( i <- 0 until NGLOBAL_XACTS) {
      val t = trackerList(i).io
      conflicts(UFix(i), t.busy(i) && coherenceConflict(t.addr, x_init.bits.address) && 
                        !(x_init.bits.has_data && (UFix(j) === t.init_tile_id)))
                        // Don't abort writebacks stalled on mem. 
                        // TODO: This assumes overlapped writeback init reqs to 
                        // the same addr will never be issued; is this ok?
    }
    x_abort.bits.tile_xact_id := x_init.bits.tile_xact_id
    val want_to_abort = conflicts.orR || busy_arr.toBits.andR
    x_abort.valid := want_to_abort && x_init.valid
    aborting.bitSet(UFix(j), want_to_abort && x_abort.ready)
  }
  
  // Handle transaction initiation requests
  // Only one allocation per cycle
  // Init requests may or may not have data
  val alloc_arb = (new Arbiter(NGLOBAL_XACTS)) { Bool() }
  val init_arb = (new Arbiter(NTILES)) { new TrackerAllocReq() }
  for( i <- 0 until NGLOBAL_XACTS ) {
    alloc_arb.io.in(i).valid := !trackerList(i).io.busy
    trackerList(i).io.can_alloc := alloc_arb.io.in(i).ready
    trackerList(i).io.alloc_req.bits <> init_arb.io.out.bits
    trackerList(i).io.alloc_req.valid := init_arb.io.out.valid

    trackerList(i).io.x_init_data <> io.tiles(trackerList(i).io.init_tile_id).xact_init_data
  }

  for( j <- 0 until NTILES ) {
    val x_init = io.tiles(j).xact_init
    val x_init_data = io.tiles(j).xact_init_data
    init_arb.io.in(j).valid := x_init.valid
    init_arb.io.in(j).bits.xact_init := x_init.bits
    init_arb.io.in(j).bits.init_tile_id := UFix(j)
    init_arb.io.in(j).bits.data_valid := x_init_data.valid
    x_init.ready := aborting(j) || foldR(trackerList.map(_.io.pop_x_init && init_arb.io.out.bits.init_tile_id === UFix(j)))(_||_)
    x_init_data.ready := aborting(j) || foldR(trackerList.map(_.io.pop_x_init_data && init_arb.io.out.bits.init_tile_id === UFix(j)))(_||_)
  }
  
  alloc_arb.io.out.ready := init_arb.io.out.valid && !busy_arr.toBits.andR &&
                              !foldR(trackerList.map(t => t.io.busy && coherenceConflict(t.io.addr, init_arb.io.out.bits.xact_init.address)))(_||_)


  // Handle probe request generation
  // Must arbitrate for each request port
  val p_req_arb_arr = List.fill(NTILES)((new Arbiter(NGLOBAL_XACTS)) { new ProbeRequest() })
  for( j <- 0 until NTILES ) {
    for( i <- 0 until NGLOBAL_XACTS ) {
      val t = trackerList(i).io
      p_req_arb_arr(j).io.in(i).bits :=  t.probe_req.bits
      p_req_arb_arr(j).io.in(i).valid := t.probe_req.valid && t.push_p_req(j)
      p_rep_cnt_dec_arr(i) = p_rep_cnt_dec_arr(i).bitSet(UFix(j), p_req_arb_arr(j).io.in(i).ready)
    }
    p_req_arb_arr(j).io.out <> io.tiles(j).probe_req
  }
*/
}
