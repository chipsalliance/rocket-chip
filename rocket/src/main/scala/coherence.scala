package rocket

import Chisel._
import Constants._

class HubMemReq extends Bundle {
  val rw   = Bool()
  val addr = UFix(width = PADDR_BITS-OFFSET_BITS)
  val tag  = Bits(width = GLOBAL_XACT_ID_BITS)
  // Figure out which data-in port to pull from
  val data_idx = Bits(width = TILE_ID_BITS)
  val is_probe_rep = Bool()
}

class MemData extends Bundle {
  val data = Bits(width = MEM_DATA_BITS)
}

class TransactionInit extends Bundle {
  val t_type = Bits(width = TTYPE_BITS)
  val has_data = Bool()
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
  val address = Bits(width = PADDR_BITS)
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

class TransactionReply extends Bundle {
  val t_type = Bits(width = TTYPE_BITS)
  val has_data = Bool()
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
}

class TransactionReplyData extends MemData

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
  val xact_rep       = (new ioDecoupled) { new TransactionReply() }
  val xact_rep_data  = (new ioDecoupled) { new TransactionReplyData() }
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
}

class XactTracker(id: Int) extends Component {
  val io = new Bundle {
    val mem_req     = (new ioDecoupled) { new HubMemReq()  }.flip
    val xact_finish = Bool(INPUT)
    val p_rep_has_data   =  Bool(INPUT) 
    val x_init_has_data   =  Bool(INPUT) 
    val p_rep_data_idx = Bits(log2up(NTILES), INPUT)
    val x_init_data_idx = Bits(log2up(NTILES), INPUT)
    val rep_cnt_dec  = Bits(NTILES, INPUT)
    val busy        = Bool(OUTPUT)
    val addr         = Bits(PADDR_BITS, OUTPUT)
    val tile_id      = Bits(TILE_ID_BITS, OUTPUT)
    val tile_xact_id = Bits(TILE_XACT_ID_BITS, OUTPUT)
    val sharer_count = Bits(TILE_ID_BITS, OUTPUT)
    val t_type        = Bits(TTYPE_BITS, OUTPUT)
    val pop_p_rep    = Bool(OUTPUT)
    val pop_p_rep_data = Bool(OUTPUT)
    val send_x_rep_ack = Bool(OUTPUT)
  }

  val valid = Reg(resetVal = Bool(false))
  val addr  = Reg{ Bits() }
  val t_type = Reg{ Bits() }
  val tile_id = Reg{ Bits() }
  val tile_xact_id = Reg{ Bits() }
  val probe_done = Reg{ Bits() }
  
}

abstract class CoherenceHub extends Component

class CoherenceHubNoDir extends CoherenceHub {

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

  val io = new Bundle {
    val tiles = Vec(NTILES) { new ioTileLink() }
    val mem = new ioDCache().flip
  }
  
  val trackerList      = (0 until NGLOBAL_XACTS).map(new XactTracker(_))
  val busy_arr         = Vec(NGLOBAL_XACTS){ Wire(){Bool()} }
  val addr_arr         = Vec(NGLOBAL_XACTS){ Wire(){Bits(width=PADDR_BITS)} }
  val tile_id_arr      = Vec(NGLOBAL_XACTS){ Wire(){Bits(width=TILE_ID_BITS)} }
  val tile_xact_id_arr = Vec(NGLOBAL_XACTS){ Wire(){Bits(width=TILE_XACT_ID_BITS)} }
  val t_type_arr        = Vec(NGLOBAL_XACTS){ Wire(){Bits(width=TTYPE_BITS)} }
  val sh_count_arr     = Vec(NGLOBAL_XACTS){ Wire(){Bits(width=TILE_ID_BITS)} }
  val send_x_rep_ack_arr    = Vec(NGLOBAL_XACTS){ Wire(){Bool()} }

  val do_free_arr      = Vec(NGLOBAL_XACTS){ Wire(){Bool()} }
  val p_rep_has_data_arr     = Vec(NGLOBAL_XACTS){ Wire(){Bool()} } 
  val p_rep_data_idx_arr     = Vec(NGLOBAL_XACTS){ Wire(){Bits(width=log2up(NTILES))} }
  val rep_cnt_dec_arr  = Vec(NGLOBAL_XACTS){ Wire(){Bits(width=NTILES)} }

  for( i <- 0 until NGLOBAL_XACTS) {
    busy_arr.write(         UFix(i), trackerList(i).io.busy)
    addr_arr.write(         UFix(i), trackerList(i).io.addr)
    tile_id_arr.write(      UFix(i), trackerList(i).io.tile_id)
    tile_xact_id_arr.write( UFix(i), trackerList(i).io.tile_xact_id)
    t_type_arr.write(        UFix(i), trackerList(i).io.t_type)
    sh_count_arr.write(     UFix(i), trackerList(i).io.sharer_count)
    send_x_rep_ack_arr.write(     UFix(i), trackerList(i).io.send_x_rep_ack)
    trackerList(i).io.xact_finish := do_free_arr.read(UFix(i))
    trackerList(i).io.p_rep_has_data := p_rep_has_data_arr.read(UFix(i))
    trackerList(i).io.p_rep_data_idx := p_rep_data_idx_arr.read(UFix(i))
    trackerList(i).io.rep_cnt_dec := rep_cnt_dec_arr.read(UFix(i))
  }

  // Nack conflicting transaction init attempts
  val aborting   = Wire() { Bits(width = NTILES) } 
  val initiating = Wire() { Bits(width = NTILES) } 
  for( j <- 0 until NTILES ) {
    val init   = io.tiles(j).xact_init
    val abort  = io.tiles(j).xact_abort
    val conflicts = Bits(width = NGLOBAL_XACTS) 
    for( i <- 0 until NGLOBAL_XACTS) {
      val t = trackerList(i).io
      conflicts(i) := t.busy(i) && coherenceConflict(t.addr, init.bits.address)
    }
    aborting(j) := (conflicts.orR || busy_arr.toBits().andR)
    abort.valid := init.valid && aborting
    abort.bits.tile_xact_id := init.bits.tile_xact_id
    init.ready  := aborting(j) || initiating(j)
  }
  
  // Free finished transactions
  for( j <- 0 until NTILES ) {
    val finish = io.tiles(j).xact_finish
    do_free_arr.write(finish.bits.global_xact_id, finish.valid)
    finish.ready := Bool(true)
  }

  // Forward memory responses from mem to tile
  val xrep_cnt = Reg(resetVal = UFix(0, log2up(REFILL_CYCLES)))
  val xrep_cnt_next = xrep_cnt + UFix(1)
  when (io.mem.resp_val) { xrep_cnt := xrep_cnt_next }
  val idx = io.mem.resp_tag
  val readys = Bits(width = NTILES) 
  for( j <- 0 until NTILES ) {
    io.tiles(j).xact_rep.bits.t_type := getTransactionReplyType(t_type_arr.read(idx), sh_count_arr.read(idx))
    io.tiles(j).xact_rep.bits.tile_xact_id := tile_xact_id_arr.read(idx)
    io.tiles(j).xact_rep.bits.global_xact_id := idx
    io.tiles(j).xact_rep_data.bits.data := io.mem.resp_data
    readys := Mux(xrep_cnt === UFix(0), io.tiles(j).xact_rep.ready && io.tiles(j).xact_rep_data.ready, io.tiles(j).xact_rep_data.ready)
    io.tiles(j).xact_rep.valid      := (UFix(j) === tile_id_arr.read(idx)) && ((io.mem.resp_val && xrep_cnt === UFix(0)) || send_x_rep_ack_arr.read(idx))
    io.tiles(j).xact_rep_data.valid := (UFix(j) === tile_id_arr.read(idx))
  }
  // If there were a ready signal due to e.g. intervening network use:
  //io.mem.resp_rdy := readys(tile_id_arr.read(idx)).xact_rep.ready
  
  // Create an arbiter for the one memory port
  // We have to arbitrate between the different trackers' memory requests
  // and once we have picked a request, get the right write data

  val mem_req_arb = (new Arbiter(NGLOBAL_XACTS)) { new HubMemReq() }
  for( i <- 0 until NGLOBAL_XACTS ) {
    mem_req_arb.io.in(i) <> trackerList(i).io.mem_req
  }
  mem_req_arb.io.out.ready := io.mem.req_rdy
  io.mem.req_val := mem_req_arb.io.out.valid
  io.mem.req_rw    := mem_req_arb.io.out.bits.rw
  io.mem.req_tag   := mem_req_arb.io.out.bits.tag
  io.mem.req_addr  := mem_req_arb.io.out.bits.addr
  io.mem.req_wdata := MuxLookup(mem_req_arb.io.out.bits.data_idx, 
                              Bits(0, width = MEM_DATA_BITS),
                              (0 until NTILES).map( j => 
                                UFix(j) -> Mux(mem_req_arb.io.out.bits.is_probe_rep, 
                                  io.tiles(j).probe_rep_data.bits.data, 
                                  io.tiles(j).xact_init_data.bits.data)))
  
  for( j <- 0 until NTILES ) {
    val p_rep = io.tiles(j).probe_rep
    val p_rep_data = io.tiles(j).probe_rep_data
    val idx = p_rep.bits.global_xact_id
    p_rep_has_data_arr.write(idx, p_rep.valid && p_rep.bits.has_data)
    p_rep_data_idx_arr.write(idx, UFix(j))
    p_rep.ready := foldR(trackerList.map(_.io.pop_p_rep))(_ || _)
    p_rep_data.ready  := foldR(trackerList.map(_.io.pop_p_rep_data))(_ || _)
  }
  for( i <- 0 until NGLOBAL_XACTS ) {
    val flags = Bits(width = NTILES)
    for( j <- 0 until NTILES) {
      val p_rep = io.tiles(j).probe_rep
      flags(j) := p_rep.valid && (p_rep.bits.global_xact_id === UFix(i))
    }
    rep_cnt_dec_arr.write(UFix(i), flags)
  }


  // Pick a single request of these types to process
  //val xact_init_arb   = (new Arbiter(NTILES)) { new TransactionInit() }
  //val probe_reply_arb = (new Arbiter(NTILES)) { new ProbeReply() }


}
