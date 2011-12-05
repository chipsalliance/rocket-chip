package Top {

import Chisel._
import Node._;
import Constants._;
import scala.math._;

// interface between D$ and processor/DTLB
class ioDmem(view: List[String] = null) extends Bundle(view) {
  val dtlb_miss  = Bool('input);
  val req_val   = Bool('input);
  val req_rdy   = Bool('output);
  val req_cmd   = Bits(4, 'input);
  val req_type  = Bits(3, 'input);
  val req_idx   = Bits(PGIDX_BITS, 'input);
  val req_ppn   = Bits(PPN_BITS, 'input);
  val req_data  = Bits(64, 'input);
  val req_tag   = Bits(5, 'input);
  val xcpt_ma_ld  = Bool('output); // misaligned load
  val xcpt_ma_st = Bool('output); // misaligned store
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
  
  val store_wmask_d = Cat(Fill(8, store_wmask_byte(7)),
  		Fill(8, store_wmask_byte(6)),
  		Fill(8, store_wmask_byte(5)),
  		Fill(8, store_wmask_byte(4)),
  		Fill(8, store_wmask_byte(3)),
  		Fill(8, store_wmask_byte(2)),
  		Fill(8, store_wmask_byte(1)),
  		Fill(8, store_wmask_byte(0)));
  
  io.store_wmask := store_wmask_d;
  
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
  dcache.io.cpu.req_tag   := Mux(flushing, r_cpu_req_tag, io.cpu.req_tag);
  dcache.io.cpu.req_type  := io.cpu.req_type;
  dcache.io.cpu.req_data  ^^ io.cpu.req_data;
  dcache.io.cpu.dtlb_miss := io.cpu.dtlb_miss;
  dcache.io.mem           ^^ io.mem;

  io.cpu.xcpt_ma_ld   := dcache.io.cpu.xcpt_ma_ld;
  io.cpu.xcpt_ma_st   := dcache.io.cpu.xcpt_ma_st;
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
  
  val s_reset :: s_ready :: s_replay_load :: s_write_amo :: s_start_writeback :: s_writeback :: s_req_refill :: s_refill :: s_resolve_miss :: Nil = Enum(9) { UFix() };
  val state = Reg(resetVal = s_reset);
  
  // idx arrives one clock cycle prior to ppn b/c of DTLB
  val r_cpu_req_idx    = Reg(resetVal = Bits(0, PGIDX_BITS)); 
  val r_cpu_req_ppn    = Reg(resetVal = Bits(0, PPN_BITS)); 
  val r_cpu_req_val    = Reg(resetVal = Bool(false));
  val r_cpu_req_cmd    = Reg(resetVal = Bits(0,4));
  val r_cpu_req_type   = Reg(resetVal = Bits(0,3));
  val r_cpu_req_tag    = Reg(resetVal = Bits(0,5));
  val r_cpu_resp_val   = Reg(resetVal = Bool(false));
  val r_amo_data       = Reg(resetVal = Bits(0,64));

  val p_store_data      = Reg(resetVal = Bits(0,64));
  val p_store_idx       = Reg(resetVal = Bits(0,PGIDX_BITS));
  val p_store_type      = Reg(resetVal = Bits(0,3));
  val p_store_valid     = Reg(resetVal = Bool(false));

  val req_store   = (io.cpu.req_cmd === M_XWR);
  val req_load    = (io.cpu.req_cmd === M_XRD) || (io.cpu.req_cmd === M_PRD);
  val req_flush   = (io.cpu.req_cmd === M_FLA);
  val req_amo     = io.cpu.req_cmd(3).toBool;
  val r_req_load  = (r_cpu_req_cmd === M_XRD) || (r_cpu_req_cmd === M_PRD);
  val r_req_store = (r_cpu_req_cmd === M_XWR);
  val r_req_flush = (r_cpu_req_cmd === M_FLA);
  val r_req_ptw_load = (r_cpu_req_cmd === M_PRD);
  val r_req_amo     = r_cpu_req_cmd(3).toBool;
  
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
  when (((state === s_resolve_miss) && (r_req_load || r_req_amo)) || (state === s_replay_load)) {
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
    ((state === s_refill) && io.mem.resp_val && (rr_count === UFix(3,2))) ||
    ((state === s_resolve_miss) && r_req_flush);

  val tag_array = Mem4(lines, r_cpu_req_ppn);
  tag_array.setReadLatency(SRAM_READ_LATENCY);
//   tag_array.setTarget('inst);
  val tag_rdata = tag_array.rw(tag_addr, r_cpu_req_ppn, tag_we);

  // valid bit array
  val vb_array = Reg(resetVal = Bits(0, lines));
  when (tag_we && !r_req_flush) { 
    vb_array <== vb_array.bitSet(r_cpu_req_idx(PGIDX_BITS-1,offsetbits).toUFix, UFix(1,1));
  }
  when (tag_we && r_req_flush) {
    vb_array <== vb_array.bitSet(r_cpu_req_idx(PGIDX_BITS-1,offsetbits).toUFix, UFix(0,1));
  }
  val vb_rdata = vb_array(r_cpu_req_idx(PGIDX_BITS-1,offsetbits).toUFix).toBool;
  val tag_valid = r_cpu_req_val && vb_rdata;
  val tag_match = (tag_rdata === io.cpu.req_ppn);
  val tag_hit  = tag_valid && tag_match;
  val miss = r_cpu_req_val && (!vb_rdata || !tag_match);

  // load/store addresses conflict if they are to any part of the same 64 bit word
  val addr_match    = (r_cpu_req_idx(PGIDX_BITS-1,offsetlsb) === p_store_idx(PGIDX_BITS-1,offsetlsb));
  val ldst_conflict = tag_valid && tag_match && (r_req_load || r_req_amo) && p_store_valid && addr_match;
  val store_hit = r_cpu_req_val && !io.cpu.dtlb_miss && tag_hit && r_req_store ;

  // write the pending store data when the cache is idle, when the next command isn't a load
  // or when there's a load to the same address (in which case there's a 2 cycle delay:
  // once cycle to write the store data and another to read the data back)
  val drain_store = 
    ((store_hit || p_store_valid) && (!io.cpu.req_val || req_store || req_flush)) ||
    (p_store_valid && (miss || ldst_conflict));

  // write pending store data from a store which missed
  // after the cache line refill has completed
  val resolve_store = (state === s_resolve_miss) && r_req_store;
  
  // pending store data
  when (io.cpu.req_val && io.cpu.req_rdy && req_store) { 
    p_store_idx   <== io.cpu.req_idx;
    p_store_data  <== io.cpu.req_data;
    p_store_type  <== io.cpu.req_type;
  }
  when (store_hit && !drain_store) {
    p_store_valid <== Bool(true);
  }
  when (drain_store)  {
    p_store_valid <== Bool(false);
  }
  
  // AMO operand
  when (io.cpu.req_val && io.cpu.req_rdy && req_amo) {
    r_amo_data <== io.cpu.req_data;
  }

  // dirty bit array
  val db_array  = Reg(resetVal = Bits(0, lines));
  val tag_dirty = db_array(r_cpu_req_idx(PGIDX_BITS-1,offsetbits).toUFix).toBool;  
  when ((r_cpu_req_val && !io.cpu.dtlb_miss && tag_hit && r_req_store) || resolve_store)  {
    db_array <== db_array.bitSet(p_store_idx(PGIDX_BITS-1,offsetbits).toUFix, UFix(1,1));
  }
  when (state === s_write_amo) {
    db_array <== db_array.bitSet(r_cpu_req_idx(PGIDX_BITS-1,offsetbits).toUFix, UFix(1,1));
  }
  when (tag_we) {
    db_array <== db_array.bitSet(r_cpu_req_idx(PGIDX_BITS-1,offsetbits).toUFix, UFix(0,1));
  }
  
  // generate write mask and data signals for stores and amos
  val storegen = new rocketDCacheStoreGen();
  storegen.io.req_addr_lsb := p_store_idx(2,0);
  storegen.io.req_data := p_store_data;
  storegen.io.req_type := p_store_type;
  val store_data = Fill(2, storegen.io.store_data);
  val store_wmask_d = storegen.io.store_wmask;
  val store_wmask = Mux(p_store_idx(offsetlsb).toBool, Cat(store_wmask_d, Bits(0,64)), Cat(Bits(0,64), store_wmask_d)); 

  // ALU for AMOs
  val amo_alu = new rocketDCacheAmoALU();
  val amo_alu_out = Cat(amo_alu.io.result,amo_alu.io.result);
  val amo_wmask = 
    Mux(r_cpu_req_type === MT_D, ~Bits(0,8),
    Mux(r_cpu_req_idx(2).toBool, Cat(~Bits(0,4), Bits(0,4)),
      Cat(Bits(0,4), ~Bits(0,4))));

  val amo_store_wmask_d = Cat(Fill(8, amo_wmask(7)),
  		Fill(8, amo_wmask(6)),
  		Fill(8, amo_wmask(5)),
  		Fill(8, amo_wmask(4)),
  		Fill(8, amo_wmask(3)),
  		Fill(8, amo_wmask(2)),
  		Fill(8, amo_wmask(1)),
  		Fill(8, amo_wmask(0)));

  val amo_store_wmask = Mux(r_cpu_req_idx(offsetlsb).toBool, Cat(amo_store_wmask_d, Bits(0,64)), Cat(Bits(0,64), amo_store_wmask_d)); 
  
  // data array
  val data_addr = 
    Mux(drain_store || resolve_store, p_store_idx(PGIDX_BITS-1, offsetmsb-1),
    Mux((state === s_writeback) && io.mem.req_rdy, Cat(r_cpu_req_idx(PGIDX_BITS-1, offsetbits), rr_count_next),
    Mux((state === s_start_writeback) || (state === s_writeback) || (state === s_refill), Cat(r_cpu_req_idx(PGIDX_BITS-1, offsetbits), rr_count),
    Mux((state === s_resolve_miss) || (state === s_replay_load) || (state === s_write_amo),  r_cpu_req_idx(PGIDX_BITS-1, offsetmsb-1),
      io.cpu.req_idx(PGIDX_BITS-1, offsetmsb-1))))).toUFix;
      
  val data_wdata = 
    Mux((state === s_refill), io.mem.resp_data, 
    Mux((state === s_write_amo), amo_alu_out,
      store_data));
     
  val data_we =
    ((state === s_refill) && io.mem.resp_val) ||
    (state === s_write_amo) ||
     drain_store || resolve_store;
     
  val data_wmask =
    Mux((state === s_refill), ~Bits(0,128),
    Mux((state === s_write_amo), amo_store_wmask,
      store_wmask));

  val data_array = Mem4(lines*4, data_wdata);
  data_array.setReadLatency(SRAM_READ_LATENCY);
//   data_array.setTarget('inst);
  val data_array_rdata = data_array.rw(data_addr, data_wdata, data_we, data_wmask);
  val resp_data = Mux(r_cpu_req_idx(offsetlsb).toBool, data_array_rdata(127, 64), data_array_rdata(63,0));
  val r_resp_data = Reg(resp_data);

  amo_alu.io.cmd := r_cpu_req_cmd;
  amo_alu.io.wmask := amo_wmask;
  amo_alu.io.lhs := Mux(r_cpu_resp_val, resp_data, r_resp_data).toUFix;
  amo_alu.io.rhs := r_amo_data.toUFix;

  // signal a load miss when the data isn't present in the cache and when it's in the pending store data register
  // (causes the cache to block for 2 cycles and the load or amo instruction is replayed)
  val load_miss = 
    !io.cpu.dtlb_miss && 
    (state === s_ready) && r_cpu_req_val && (r_req_load || r_req_amo) && (!tag_hit || (p_store_valid && addr_match));

  // output signals
  // busy when there's a load to the same address as a pending store, or on a cache miss, or when executing a flush
  io.cpu.req_rdy   := (state === s_ready) && !io.cpu.dtlb_miss && !ldst_conflict && (!r_cpu_req_val || (tag_hit && !(r_req_flush || r_req_amo)));
  io.cpu.resp_val  := !io.cpu.dtlb_miss && 
                      ((state === s_ready) &&  tag_hit && (r_req_load || r_req_amo) && !(p_store_valid && addr_match)) || 
                      ((state === s_resolve_miss) && r_req_flush) ||
                      r_cpu_resp_val;
                      
  val misaligned =
    (((r_cpu_req_type === MT_H) || (r_cpu_req_type === MT_HU)) && r_cpu_req_idx(0).toBool) ||
    (((r_cpu_req_type === MT_W) || (r_cpu_req_type === MT_WU)) && (r_cpu_req_idx(1,0) != Bits(0,2))) ||
    ((r_cpu_req_type === MT_D) && (r_cpu_req_idx(2,0) != Bits(0,3)));
    
  io.cpu.xcpt_ma_ld := r_cpu_req_val && (r_req_load  || r_req_amo) && misaligned;
  io.cpu.xcpt_ma_st := r_cpu_req_val && (r_req_store || r_req_amo) && misaligned;
    
  io.cpu.resp_miss := load_miss;
  // tag MSB distinguishes between loads destined for the PTW and CPU
  io.cpu.resp_tag  := Cat(r_req_ptw_load, r_cpu_req_type, r_cpu_req_idx(2,0), r_cpu_req_tag);
  io.cpu.resp_data := resp_data;

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
      when (!r_cpu_req_val || (tag_hit && !(r_req_flush || r_req_amo))) {
        state <== s_ready;
      }
      when (tag_hit && r_req_amo) {
        state <== s_write_amo;
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
    is (s_write_amo) {
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
      when (r_req_amo) {
        state <== s_write_amo;
      }
      state <== s_ready;
    }
  }  
}

class rocketDCacheAmoALU extends Component {
  val io = new Bundle {
    val cmd  = Bits(4, 'input);
    val wmask = Bits(8, 'input);
    val lhs   = UFix(64, 'input);
    val rhs   = UFix(64, 'input);
    val result    = UFix(64, 'output);
  }
  
//   val signed_cmp = (op === M_XA_MIN) || (op === M_XA_MAX);
//   val sub = (op === M_XA_MIN) || (op === M_XA_MINU) ||
//             (op === M_XA_MAX) || (op === M_XA_MAXU);

  val adder_lhs = Cat(io.lhs(63,32),io.wmask(3) & io.lhs(31), io.lhs(30,0)).toUFix;
  val adder_rhs = Cat(io.rhs(63,32),io.wmask(3) & io.rhs(31), io.rhs(30,0)).toUFix;
//   val adder_rhs = Cat(Mux(sub, ~io.rhs, io.rhs), sub).toUFix;
//   val sum = adder_lhs + adder_rhs;
//   val adder_out = sum(64,1);
  val adder_out = adder_lhs + adder_rhs;
  val alu_out = Wire() { UFix() };
  switch (io.cmd) {
//     is (M_XA_ADD)  { alu_out <== adder_out; }
    is (M_XA_SWAP) { alu_out <== io.rhs; }
    is (M_XA_AND)  { alu_out <== io.lhs & io.rhs; }
    is (M_XA_OR)   { alu_out <== io.lhs | io.rhs; }
  }
  alu_out <== adder_out;
  io.result := alu_out;
}
  
}
