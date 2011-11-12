package Top {

import Chisel._
import Node._;
import Constants._;
import scala.math._;

// interface between D$ and processor/DTLB
class ioDmem(view: List[String] = null) extends Bundle(view) {
//   val dtlb_busy  = Bool('input);
  val dtlb_miss  = Bool('input);
  val req_val   = Bool('input);
  val req_rdy   = Bool('output);
  val req_cmd   = Bits(4, 'input);
  val req_type  = Bits(3, 'input);
  val req_idx   = Bits(PGIDX_BITS, 'input);
  val req_ppn   = Bits(PPN_BITS, 'input);
//   val req_addr  = UFix(PADDR_BITS, 'input);
  val req_data  = Bits(64, 'input);
  val req_tag   = Bits(5, 'input);
  val resp_miss = Bool('output);
  val resp_val  = Bool('output);
  val resp_data = Bits(64, 'output);
  val resp_tag  = Bits(12, 'output);
}

// interface between D$ and next level in memory hierarchy
class ioDcache(view: List[String] = null) extends Bundle(view) {
  val req_addr  = UFix(PADDR_BITS, 'input);
  val req_tag   = UFix(3, 'input);
  val req_val   = Bool('input);
  val req_rdy   = Bool('output);
  val req_wdata = Bits(128, 'input);
  val req_rw    = Bool('input);
  val resp_data = Bits(128, 'output);
//   val resp_tag  = Bits(3, 'output);
  val resp_val  = Bool('output);
}

class ioDCacheDM extends Bundle() {
  val cpu = new ioDmem();
  val mem = new ioDcache().flip();
}

class rocketDCacheStoreGen extends Component {
  val io = new Bundle {
    val req_type      = Bits(3, 'input);
    val req_addr_lsb  = Bits(3, 'input);
    val req_data      = Bits(64, 'input);
    val store_wmask   = Bits(64, 'output);
    val store_data    = Bits(64, 'output);
  }
  
  // generate write mask and store data signals based on store type and address LSBs
  val wmask_b =
    Mux(io.req_addr_lsb === UFix(0, 3), Bits("b0000_0001", 8),
    Mux(io.req_addr_lsb === UFix(1, 3), Bits("b0000_0010", 8),
    Mux(io.req_addr_lsb === UFix(2, 3), Bits("b0000_0100", 8),
    Mux(io.req_addr_lsb === UFix(3, 3), Bits("b0000_1000", 8),
    Mux(io.req_addr_lsb === UFix(4, 3), Bits("b0001_0000", 8),
    Mux(io.req_addr_lsb === UFix(5, 3), Bits("b0010_0000", 8),
    Mux(io.req_addr_lsb === UFix(6, 3), Bits("b0100_0000", 8),
    Mux(io.req_addr_lsb === UFix(7, 3), Bits("b1000_0000", 8),
        UFix(0, 8)))))))));

  val wmask_h =
    Mux(io.req_addr_lsb(2,1) === UFix(0, 2), Bits("b0000_0011", 8),
    Mux(io.req_addr_lsb(2,1) === UFix(1, 2), Bits("b0000_1100", 8),
    Mux(io.req_addr_lsb(2,1) === UFix(2, 2), Bits("b0011_0000", 8),
    Mux(io.req_addr_lsb(2,1) === UFix(3, 2), Bits("b1100_0000", 8),
        UFix(0, 8)))));

  val wmask_w =
    Mux(io.req_addr_lsb(2) === UFix(0, 1), Bits("b0000_1111", 8),
    Mux(io.req_addr_lsb(2) === UFix(1, 1), Bits("b1111_0000", 8),
        UFix(0, 8)));

  val wmask_d =
    Bits("b1111_1111", 8);
    
  val store_wmask_byte =
    Mux(io.req_type === MT_B, wmask_b,
    Mux(io.req_type === MT_H, wmask_h,
    Mux(io.req_type === MT_W, wmask_w,
    Mux(io.req_type === MT_D, wmask_d,
        UFix(0, 8)))));
  
  io.store_wmask := 
    Cat(Fill(8, store_wmask_byte(7)),
  		Fill(8, store_wmask_byte(6)),
  		Fill(8, store_wmask_byte(5)),
  		Fill(8, store_wmask_byte(4)),
  		Fill(8, store_wmask_byte(3)),
  		Fill(8, store_wmask_byte(2)),
  		Fill(8, store_wmask_byte(1)),
  		Fill(8, store_wmask_byte(0)));
    
  io.store_data :=
    Mux(io.req_type === MT_B, Fill(8, io.req_data( 7,0)),
    Mux(io.req_type === MT_H, Fill(4, io.req_data(15,0)),
    Mux(io.req_type === MT_W, Fill(2, io.req_data(31,0)),
    Mux(io.req_type === MT_D, io.req_data,
       UFix(0, 64)))));  
  
}
 
// state machine to flush (write back dirty lines, invalidate clean ones) the D$
class rocketDCacheDM_flush(lines: Int) extends Component {
  val io = new ioDCacheDM();
  val dcache = new rocketDCacheDM(lines);
  
  val addrbits = PADDR_BITS;
  val indexbits = ceil(log10(lines)/log10(2)).toInt;
  val offsetbits = 6;
  val tagmsb    = addrbits - 1;
  val taglsb    = indexbits+offsetbits;
  val tagbits   = tagmsb-taglsb+1;
  val indexmsb  = taglsb-1;
  val indexlsb  = offsetbits;
  val offsetmsb = indexlsb-1;
  val offsetlsb = 3;
  
  val flush_count = Reg(resetVal = UFix(0, indexbits));
  val flush_resp_count = Reg(resetVal = UFix(0, indexbits));
  val flushing = Reg(resetVal = Bool(false));
  val flush_waiting = Reg(resetVal = Bool(false));
  val r_cpu_req_tag = Reg(resetVal = Bits(0, 5));

  when (io.cpu.req_val && io.cpu.req_rdy && (io.cpu.req_cmd === M_FLA)) 
  { 
    r_cpu_req_tag <== io.cpu.req_tag;
    flushing <== Bool(true);
    flush_waiting <== Bool(true);
  }
  
  when (dcache.io.cpu.req_rdy && (flush_count === ~Bits(0, indexbits))) {
    flushing <== Bool(false);
  }
  when (dcache.io.cpu.resp_val && (dcache.io.cpu.resp_tag === r_cpu_req_tag) && (flush_resp_count === ~Bits(0, indexbits))) {
    flush_waiting <== Bool(false);
  }
  
  when (flushing && dcache.io.cpu.req_rdy) {
    flush_count <== flush_count + UFix(1,1);
  }
  when (flush_waiting && dcache.io.cpu.resp_val && (dcache.io.cpu.resp_tag(5,0) === r_cpu_req_tag)) {
    flush_resp_count <== flush_resp_count + UFix(1,1);
  }
  
  dcache.io.cpu.req_val   := (io.cpu.req_val && (io.cpu.req_cmd != M_FLA) && !flush_waiting) || flushing;
  dcache.io.cpu.req_cmd   := Mux(flushing, M_FLA, io.cpu.req_cmd);
  dcache.io.cpu.req_idx   := Mux(flushing, Cat(flush_count, Bits(0,offsetbits)), io.cpu.req_idx);
  dcache.io.cpu.req_ppn   := Mux(flushing, UFix(0,PPN_BITS), io.cpu.req_ppn);
//   dcache.io.cpu.req_addr  := 
//     Mux(flushing, Cat(Bits(0,tagmsb-taglsb+1), flush_count, Bits(0,offsetbits)).toUFix, 
//       io.cpu.req_addr);
  dcache.io.cpu.req_tag   := Mux(flushing, r_cpu_req_tag, io.cpu.req_tag);
  dcache.io.cpu.req_type  := io.cpu.req_type;
  dcache.io.cpu.req_data  ^^ io.cpu.req_data;
//   dcache.io.cpu.dtlb_busy := io.cpu.dtlb_busy;
  dcache.io.cpu.dtlb_miss := io.cpu.dtlb_miss;
  dcache.io.mem           ^^ io.mem;

  io.cpu.req_rdy   := dcache.io.cpu.req_rdy && !flush_waiting;
  io.cpu.resp_miss := dcache.io.cpu.resp_miss;
  io.cpu.resp_data := dcache.io.cpu.resp_data;
  io.cpu.resp_tag  := dcache.io.cpu.resp_tag;
  io.cpu.resp_val  := dcache.io.cpu.resp_val & 
    !(flush_waiting && (io.cpu.resp_tag === r_cpu_req_tag) && (flush_count != ~Bits(0, addrbits)));
  
}

class rocketDCacheDM(lines: Int) extends Component {
  val io = new ioDCacheDM();
  
  val addrbits = PADDR_BITS;
  val indexbits = ceil(log10(lines)/log10(2)).toInt;
  val offsetbits = 6; // 64 byte cache lines = 2^6 bytes
  val tagmsb    = PADDR_BITS-1;
  val taglsb    = indexbits+offsetbits;
  val tagbits   = tagmsb-taglsb+1;
  val indexmsb  = taglsb-1;
  val indexlsb  = offsetbits;
  val offsetmsb = indexlsb-1;
  val offsetlsb = 3;
  
  val s_reset :: s_ready :: s_replay_load :: s_start_writeback :: s_writeback :: s_req_refill :: s_refill :: s_resolve_miss :: Nil = Enum(8) { UFix() };
  val state = Reg(resetVal = s_reset);
  
  // idx arrives one clock cycle prior to ppn b/c of DTLB
  val r_cpu_req_idx    = Reg(resetVal = Bits(0, PGIDX_BITS)); 
  val r_cpu_req_ppn    = Reg(resetVal = Bits(0, PPN_BITS)); 
  val r_cpu_req_val    = Reg(resetVal = Bool(false));
  val r_cpu_req_cmd    = Reg(resetVal = Bits(0,4));
  val r_cpu_req_type   = Reg(resetVal = Bits(0,3));
  val r_cpu_req_tag    = Reg(resetVal = Bits(0,5));
  val r_cpu_resp_val   = Reg(resetVal = Bool(false));

  val p_store_data      = Reg(resetVal = Bits(0,64));
  val p_store_idx       = Reg(resetVal = Bits(0,PGIDX_BITS));
  val p_store_type      = Reg(resetVal = Bits(0,3));
  val p_store_valid     = Reg(resetVal = Bool(false));

  val req_store   = (io.cpu.req_cmd === M_XWR);
  val r_req_load  = (r_cpu_req_cmd === M_XRD) || (r_cpu_req_cmd === M_PRD);
  val r_req_store = (r_cpu_req_cmd === M_XWR);
  val r_req_flush = (r_cpu_req_cmd === M_FLA);
  val r_req_ptw_load = (r_cpu_req_cmd === M_PRD);

  when (io.cpu.req_val && io.cpu.req_rdy) {
    r_cpu_req_idx   <== io.cpu.req_idx;
    r_cpu_req_cmd   <== io.cpu.req_cmd;
    r_cpu_req_type  <== io.cpu.req_type;
    r_cpu_req_tag   <== io.cpu.req_tag;
  }
  
  when ((state === s_ready) && r_cpu_req_val && !io.cpu.dtlb_miss) {
    r_cpu_req_ppn <== io.cpu.req_ppn;
  }
  when (io.cpu.req_rdy) {
    r_cpu_req_val <== io.cpu.req_val; 
  }
  otherwise {
    r_cpu_req_val <== Bool(false);
  }
  when (((state === s_resolve_miss) && r_req_load) || (state === s_replay_load)) {
    r_cpu_resp_val <== Bool(true);
  }
  otherwise {
    r_cpu_resp_val <== Bool(false);
  }
  
  // refill counter
  val rr_count = Reg(resetVal = UFix(0,2));
  val rr_count_next = rr_count + UFix(1);
  when (((state === s_refill) && io.mem.resp_val) || ((state === s_writeback) && io.mem.req_rdy)) { 
    rr_count <== rr_count_next;
  }

  // tag array
  val tag_addr = 
    Mux((state === s_ready), io.cpu.req_idx(PGIDX_BITS-1,offsetbits),
      r_cpu_req_idx(PGIDX_BITS-1,offsetbits)).toUFix;  
  val tag_we = 
    ((state === s_refill) && io.mem.req_rdy && (rr_count === UFix(3,2))) ||
    ((state === s_resolve_miss) && r_req_flush);

  val tag_array = new rocketSRAMsp(lines, tagbits);  
  tag_array.io.a    := tag_addr;
  tag_array.io.d    := r_cpu_req_ppn;
  tag_array.io.we   := tag_we;
  tag_array.io.bweb := ~Bits(0,tagbits);
  tag_array.io.ce   := (state === s_ready) || (state === s_start_writeback) || (state === s_writeback);
  val tag_rdata      = tag_array.io.q;

  // valid bit array
  val vb_array = Reg(resetVal = Bits(0, lines));
//   val vb_rdata = Reg(vb_array(tag_raddr));
  when (tag_we && !r_req_flush) { 
    vb_array <== vb_array.bitSet(r_cpu_req_idx(PGIDX_BITS-1,offsetbits).toUFix, UFix(1,1));
  }
  when (tag_we && r_req_flush) {
    vb_array <== vb_array.bitSet(r_cpu_req_idx(PGIDX_BITS-1,offsetbits).toUFix, UFix(0,1));
  }
  val vb_rdata = Reg(vb_array(tag_addr).toBool);
  val tag_valid = r_cpu_req_val && vb_rdata;
//   val tag_valid = Reg(vb_array(tag_addr)).toBool;
  val tag_match = (tag_rdata === io.cpu.req_ppn);
  val addr_match    = (r_cpu_req_idx(PGIDX_BITS-1,offsetbits) === p_store_idx(PGIDX_BITS-1,offsetbits));
  val ldst_conflict = r_cpu_req_val && r_req_load && p_store_valid && addr_match;

  // write the pending store data when the cache is idle, when the next command isn't a load
  // or when there's a load to the same address (in which case there's a 2 cycle delay:
  // once cycle to write the store data and another to read the data back) 
  val drain_store = !io.cpu.dtlb_miss && p_store_valid && (!io.cpu.req_val || req_store || ldst_conflict);

  // write pending store data from a store which missed
  // after the cache line refill has completed
  val resolve_store = (state === s_resolve_miss) && r_req_store;
  
  // dirty bit array
  val db_array  = Reg(resetVal = Bits(0, lines));
//   val db_rdata  = Reg(db_array(tag_raddr));
  val tag_dirty = Reg(db_array(tag_addr)).toBool;
  
  when (io.cpu.req_val && io.cpu.req_rdy && req_store) { 
    p_store_idx   <== io.cpu.req_idx;
    p_store_data  <== io.cpu.req_data;
    p_store_type  <== io.cpu.req_type;
    p_store_valid <== Bool(true);  
  }
  // cancel store if there's a DTLB miss
  when (r_cpu_req_val && r_req_store && io.cpu.dtlb_miss)
  {
    p_store_valid <== Bool(false);
  }
  when (drain_store)  {
    p_store_valid <== Bool(false);
    db_array <== db_array.bitSet(p_store_idx(PGIDX_BITS-1,offsetbits).toUFix, UFix(1,1));
  }
  when (resolve_store) {
    db_array <== db_array.bitSet(p_store_idx(PGIDX_BITS-1,offsetbits).toUFix, UFix(1,1));
  }
  when (tag_we) {
    db_array <== db_array.bitSet(r_cpu_req_idx(PGIDX_BITS-1,offsetbits).toUFix, UFix(0,1));
  }
  
  // generate write mask and data signals for stores
  val storegen = new rocketDCacheStoreGen();
  storegen.io.req_addr_lsb := p_store_idx(2,0);
  storegen.io.req_data := p_store_data;
  storegen.io.req_type := p_store_type
  val store_data = Fill(2, storegen.io.store_data);
  val store_wmask_d = storegen.io.store_wmask;
  val store_idx_sel = p_store_idx(offsetlsb).toBool;
  val store_wmask = Mux(store_idx_sel, Cat(store_wmask_d, Bits(0,64)), Cat(Bits(0,64), store_wmask_d)); 

  // data array
  val data_array = new rocketSRAMsp(lines*4, 128);
  data_array.io.a := 
    Mux(drain_store || resolve_store, p_store_idx(PGIDX_BITS-1, offsetmsb-1),
    Mux((state === s_writeback) && io.mem.req_rdy, Cat(r_cpu_req_idx(PGIDX_BITS-1, offsetbits), rr_count_next),
    Mux((state === s_start_writeback) || (state === s_writeback) || (state === s_refill), Cat(r_cpu_req_idx(PGIDX_BITS-1, offsetbits), rr_count),
    Mux((state === s_resolve_miss) || (state === s_replay_load),  r_cpu_req_idx(PGIDX_BITS-1, offsetmsb-1),
      io.cpu.req_idx(PGIDX_BITS-1, offsetmsb-1))))).toUFix;
      
  data_array.io.d :=  Mux((state === s_refill), io.mem.resp_data, store_data);
  data_array.io.we := ((state === s_refill) && io.mem.resp_val) || drain_store || resolve_store;
  data_array.io.bweb := Mux((state === s_refill), ~Bits(0,128), store_wmask);
  data_array.io.ce := Bool(true); // FIXME
  val data_array_rdata = data_array.io.q;

  // signal a load miss when the data isn't present in the cache and when it's in the pending store data register
  // (causes the cache to block for 2 cycles and the load instruction is replayed)
  val hit  = tag_valid && tag_match;
  val load_miss = !io.cpu.dtlb_miss && (state === s_ready) && r_cpu_req_val && r_req_load && (!hit || (p_store_valid && addr_match));

  // output signals
  // busy when there's a load to the same address as a pending store, or on a cache miss, or when executing a flush
  io.cpu.req_rdy   := !io.cpu.dtlb_miss && (state === s_ready) && !ldst_conflict && (!r_cpu_req_val || (hit && !r_req_flush));
  io.cpu.resp_val  := !io.cpu.dtlb_miss && ((state === s_ready) &&  hit && r_req_load && !(p_store_valid && addr_match)) || 
                      ((state === s_resolve_miss) && r_req_flush) ||
                      r_cpu_resp_val;
                      
  io.cpu.resp_miss := load_miss;
  // tag MSB distinguishes between loads destined for the PTW and CPU
  io.cpu.resp_tag  := Cat(r_req_ptw_load, r_cpu_req_type, r_cpu_req_idx(2,0), r_cpu_req_tag);
  io.cpu.resp_data := 
    Mux(r_cpu_req_idx(offsetlsb).toBool, data_array_rdata(127, 64), 
      data_array_rdata(63,0));

  io.mem.req_val   := (state === s_req_refill) || (state === s_writeback);
  io.mem.req_rw    := (state === s_writeback);
  io.mem.req_wdata := data_array_rdata;
  io.mem.req_tag   := UFix(0);
  io.mem.req_addr  := 
    Mux(state === s_writeback, Cat(tag_rdata, r_cpu_req_idx(PGIDX_BITS-1, offsetbits), rr_count), 
      Cat(r_cpu_req_ppn, r_cpu_req_idx(PGIDX_BITS-1, offsetbits), Bits(0,2))).toUFix;

  // control state machine
  switch (state) {
    is (s_reset) {
      state <== s_ready;
    }
    is (s_ready) {
      when (io.cpu.dtlb_miss) {
        state <== s_ready;
      }
      when (ldst_conflict) {
        state <== s_replay_load;
      }
      when (!r_cpu_req_val || (hit && !r_req_flush)) {
        state <== s_ready;
      }
      when (tag_valid & tag_dirty) {
        state <== s_start_writeback; 
      }
      when (r_req_flush) {
        state <== s_resolve_miss;
      }
      otherwise {
        state <== s_req_refill;
      }
    }
    is (s_replay_load) {
      state <== s_ready;
    }
    is (s_start_writeback) {
      state <== s_writeback;
    }
    is (s_writeback) {
      when (io.mem.req_rdy && (rr_count === UFix(3,2))) { 
        when (r_req_flush) {
          state <== s_resolve_miss;
        } 
        otherwise {
          state <== s_req_refill;
        }
      }
    }
    is (s_req_refill)
    {
      when (io.mem.req_rdy) { state <== s_refill; }
    }
    is (s_refill) {
      when (io.mem.resp_val && (rr_count === UFix(3,2))) { state <== s_resolve_miss; }
    }
    is (s_resolve_miss) {
      state <== s_ready;
    }
  }  
}

}
