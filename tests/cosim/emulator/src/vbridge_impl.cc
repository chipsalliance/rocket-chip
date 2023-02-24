#include <fmt/core.h>
#include <glog/logging.h>

#include "disasm.h"

#include "verilated.h"

#include "exceptions.h"
#include "simple_sim.h"
#include "tl_interface.h"
#include "util.h"
#include "vbridge_impl.h"
#include "vpi.h"

#include "glog_exception_safe.h"

/// convert TL style size to size_by_bytes
inline uint32_t decode_size(uint32_t encoded_size) {
  return 1 << encoded_size;
}

VBridgeImpl::VBridgeImpl() : sim(1 << 30),
                             isa("rv64gc", "msu"),
                             _cycles(100),
                             proc(
                                 /*isa*/ &isa,
                                 /*varch*/ fmt::format("").c_str(),
                                 /*sim*/ &sim,
                                 /*id*/ 0,
                                 /*halt on reset*/ true,
                                 /* endianness*/ memif_endianness_little,
                                 /*log_file_t*/ nullptr,
                                 /*sout*/ std::cerr) {
}

void VBridgeImpl::setup(const std::string &_bin, const std::string &_ebin, const std::string &_wave, uint64_t _reset_vector, uint64_t cycles) {
  this->bin = _bin;
  this->ebin = _ebin;
  this->wave = _wave;
  this->reset_vector = _reset_vector;
  this->timeout = cycles;
}


void VBridgeImpl::reset() {
  top.clock = 0;
  top.reset = 1;
  top.resetVector = 0x1000;
  top.eval();
  tfp.dump(0);

  // posedge
  top.clock = 1;
  top.eval();
  tfp.dump(1);


  // negedge
  top.reset = 0;
  top.clock = 0;
  top.eval();
  tfp.dump(2);
  // posedge
  LOG(INFO) << fmt::format("Reset compeleted, now we start");
  top.reset = 0;
  top.clock = 1;
  top.eval();
  tfp.dump(3);
  ctx.time(2);
}

VBridgeImpl::~VBridgeImpl() {
  terminate_simulator();
}

void VBridgeImpl::configure_simulator(int argc, char **argv) {
  ctx.commandArgs(argc, argv);
}

void VBridgeImpl::init_spike() {
  auto state = proc.get_state();
  LOG(INFO) << fmt::format("Spike reset misa={:08X}", state->misa->read());
  LOG(INFO) << fmt::format("Spike reset mstatus={:08X}", state->mstatus->read());
  // load binary to reset_vector
  sim.load(bin, ebin, reset_vector);
}

SpikeEvent *find_se_to_issue();
void VBridgeImpl::init_simulator() {
  Verilated::traceEverOn(true);
  top.trace(&tfp, 99);
  tfp.open(wave.c_str());
  _cycles = timeout;
}

void VBridgeImpl::terminate_simulator() {
  if (tfp.isOpen()) {
    tfp.close();
  }
  top.final();
}

uint64_t VBridgeImpl::get_t() {
  return ctx.time();
}

uint8_t VBridgeImpl::load(uint64_t address) {
  return *sim.addr_to_mem(address);
}

/* For TL Acquire
 * Record mem info in se.block when init spike event;
 * receive tl Acquire, find corresponding SE in queue. store se.block info in AcquireBanks;set AcquireBanks.remaining as true;
 * return: drive D channel with AcquireBanks
 * */
void VBridgeImpl::run() {

  init_spike();
  // sim.load(bin, reset_vector);
  init_simulator();
  reset();

  // start loop
  // queue size must > for trap insn to pop correct
  while (true) {
    loop_until_se_queue_full();
    while (to_rtl_queue.size() > 1) {
      return_fetch_response();

      top.clock = 1;
      top.eval();

      top.memory_0_a_ready = 1;
      top.memory_0_c_ready = 1;
      top.memory_0_e_ready = 1;
      // negedge

      receive_tl_req();
      top.clock = 0;
      top.eval();
      tfp.dump(2 * ctx.time());
      ctx.timeInc(1);


      // posedge, update registers
      top.clock = 1;
      top.eval();
      tfp.dump(2 * ctx.time() - 1);


      // when RTL write back
      if (top.rootp->DUT__DOT__ldut__DOT__tile__DOT__core__DOT__wb_valid) {
        uint64_t pc = top.rootp->DUT__DOT__ldut__DOT__tile__DOT__core__DOT__wb_reg_pc;
        LOG(INFO) << fmt::format("RTL write back insn {:08X} ", pc);
        // Check rf write
        // todo: use rf_valid
        if (top.rootp->DUT__DOT__ldut__DOT__tile__DOT__core__DOT____Vcellinp__rf_ext__W0_en) {
          record_rf_access();
        }
        // commit spike event
        bool commit = false;

        for (auto se_iter = to_rtl_queue.rbegin(); se_iter != to_rtl_queue.rend(); se_iter++) {
          if (se_iter->pc == pc) {
            // mechanism to the insn which causes trap.
            // trapped insn will commit with the first insn after trap(0x80000004).
            // It demands the trap insn not to be the last one in the queue.
            if (se_iter->pc == 0x80000004) {
              for (auto se_it = to_rtl_queue.rbegin(); se_it != to_rtl_queue.rend(); se_it++) {
                if (se_it->is_trap) se_it->is_committed = true;
              }
            }
            commit = true;
            se_iter->is_committed = true;
            LOG(INFO) << fmt::format("Set spike {:08X} as committed", se_iter->pc);
            break;
          }
        }
        if (!commit) LOG(INFO) << fmt::format("RTL wb without se in pc =  {:08X}", pc);
        // pop the committed Event from the queue
        for (int i = 0; i < to_rtl_queue_size; i++) {
          if (to_rtl_queue.back().is_committed) {
            LOG(INFO) << fmt::format("Pop SE pc = {:08X} ", to_rtl_queue.back().pc);
            to_rtl_queue.pop_back();
          }
        }
      }
      if (get_t() >= timeout) {
        throw TimeoutException();
      }
    }
  }
}

void VBridgeImpl::loop_until_se_queue_full() {
  LOG(INFO) << fmt::format("Refilling Spike queue");
  while (to_rtl_queue.size() < to_rtl_queue_size) {
    try {
      std::optional<SpikeEvent> spike_event = spike_step();
      if (spike_event.has_value()) {
        SpikeEvent &se = spike_event.value();
        to_rtl_queue.push_front(std::move(se));
      }
    } catch (trap_t &trap) {
      LOG(FATAL) << fmt::format("spike trapped with {}", trap.name());
    }
  }
  LOG(INFO) << fmt::format("to_rtl_queue is full now, start to simulate.");
  for (auto se_iter = to_rtl_queue.rbegin(); se_iter != to_rtl_queue.rend(); se_iter++) {
    LOG(INFO) << fmt::format("List: spike pc = {:08X}, write reg({}) from {:08x} to {:08X}, is commit:{}",
                             se_iter->pc, se_iter->rd_idx, se_iter->rd_old_bits, se_iter->rd_new_bits, se_iter->is_committed);
  }
}
// don't creat spike event for csr insn
// todo: haven't created spike event for insn which traps during fetch stage;
// dealing with trap:
// most traps are dealt by Spike when [proc.step(1)];
// traps during fetch stage [fetch = proc.get_mmu()->load_insn(state->pc)] are dealt manually using try-catch block below.
std::optional<SpikeEvent> VBridgeImpl::spike_step() {
  auto state = proc.get_state();
  // to use pro.state, set some csr
  state->dcsr->halt = false;
  // record pc before execute
  auto pc_before = state->pc;
  try {
    auto fetch = proc.get_mmu()->load_insn(state->pc);
    auto event = create_spike_event(fetch);
    auto &xr = proc.get_state()->XPR;
    LOG(INFO) << fmt::format("Spike start to execute pc=[{:08X}] insn = {:08X} DISASM:{}",
                             pc_before, fetch.insn.bits(), proc.get_disassembler()->disassemble(fetch.insn));
    auto &se = event.value();
    se.pre_log_arch_changes();
    proc.step(1);
    se.log_arch_changes();
    // todo: detect exactly the trap
    // if a insn_after_pc = 0x80000004,set it as committed
    // set insn which traps as committed in case the queue stalls
    if (state->pc == 0x80000004) {
      se.is_trap = true;
      LOG(INFO) << fmt::format("Trap happens at pc = {:08X} ", pc_before);
    }
    LOG(INFO) << fmt::format("Spike after execute pc={:08X} ", state->pc);
    return event;
  } catch (trap_t &trap) {
    LOG(INFO) << fmt::format("spike fetch trapped with {}", trap.name());
    proc.step(1);
    LOG(INFO) << fmt::format("Spike mcause={:08X}", state->mcause->read());
    return {};
  } catch (triggers::matched_t &t) {
    LOG(INFO) << fmt::format("spike fetch triggers ");
    proc.step(1);
    LOG(INFO) << fmt::format("Spike mcause={:08X}", state->mcause->read());
    return {};
  }
}
// now we take all the instruction as spike event except csr insn
std::optional<SpikeEvent> VBridgeImpl::create_spike_event(insn_fetch_t fetch) {
  return SpikeEvent{proc, fetch, this};
}

// For every RTL commit event, finds the corresponding Spike event and checks their results.
// two types of failure:
// 1: can't find spike event in queue
// 2: results mismatch
void VBridgeImpl::record_rf_access() {

  // peek rtl rf access
  uint32_t waddr = top.rootp->DUT__DOT__ldut__DOT__tile__DOT__core__DOT__rf_waddr;
  uint64_t wdata = top.rootp->DUT__DOT__ldut__DOT__tile__DOT__core__DOT__rf_wdata;
  uint64_t pc = top.rootp->DUT__DOT__ldut__DOT__tile__DOT__core__DOT__wb_reg_pc;
  uint64_t insn = top.rootp->DUT__DOT__ldut__DOT__tile__DOT__core__DOT__wb_reg_inst;

  uint8_t opcode = clip(insn, 0, 6);
  bool rtl_csr = opcode == 0b1110011;

  // exclude those rtl reg_write from csr insn
  if (!rtl_csr) {
    LOG(INFO) << fmt::format("RTL wirte reg({}) = {:08X}, pc = {:08X}", waddr, wdata, pc);
    // find corresponding spike event
    SpikeEvent *se = nullptr;
    for (auto se_iter = to_rtl_queue.rbegin(); se_iter != to_rtl_queue.rend(); se_iter++) {
      if ((se_iter->pc == pc) && (se_iter->rd_idx == waddr) && (!se_iter->is_committed)) {
        se = &(*se_iter);
        break;
      }
    }
    if (se == nullptr) {
      for (auto se_iter = to_rtl_queue.rbegin(); se_iter != to_rtl_queue.rend(); se_iter++) {
        // LOG(INFO) << fmt::format("se pc = {:08X}, rd_idx = {:08X}",se_iter->pc,se_iter->rd_idx);
        LOG(INFO) << fmt::format("List: spike pc = {:08X}, write reg({}) from {:08x} to {:08X}, is commit:{}",
                                 se_iter->pc, se_iter->rd_idx, se_iter->rd_old_bits, se_iter->rd_new_bits, se_iter->is_committed);
      }
      LOG(FATAL) << fmt::format("RTL rf_write Cannot find se ; pc = {:08X} , insn={:08X}, waddr={:08X}", pc, insn, waddr);
    }
    // start to check RTL rf_write with spike event
    // for non-store ins. check rf write
    // todo: why exclude store insn? store insn shouldn't write regfile.
    if (!(se->is_store)) {
      CHECK_EQ_S(wdata, se->rd_new_bits) << fmt::format("\n RTL write Reg({})={:08X} but Spike write={:08X}", waddr, wdata, se->rd_new_bits);
    } else {
      LOG(INFO) << fmt::format("Find Store insn");
    }
  } else {
    LOG(INFO) << fmt::format("RTL csr insn wirte reg({}) = {:08X}, pc = {:08X}", waddr, wdata, pc);
  }
}


void VBridgeImpl::receive_tl_req() {
#define TL(name) (get_tl_##name(top))
  uint64_t pc = top.rootp->DUT__DOT__ldut__DOT__tile__DOT__core__DOT__ex_reg_pc;
  int miss = top.rootp->DUT__DOT__ldut__DOT__tile__DOT__frontend__DOT__icache__DOT__s2_miss;
  int valid = TL(a_valid);
  if (!TL(a_valid)) return;
  // store A channel req
  uint8_t opcode = TL(a_bits_opcode);
  uint32_t addr = TL(a_bits_address);
  uint8_t size = TL(a_bits_size);
  uint8_t src = TL(a_bits_source);//  TODO: be returned in D channel
  uint8_t param = TL(a_bits_param);
  // find icache refill request, fill fetch_banks
  if (miss) {
    switch (opcode) {
      case TlOpcode::Get: {
        for (int i = 0; i < 8; i++) {
          uint64_t insn = 0;
          for (int j = 0; j < 8; ++j) {
            insn += (uint64_t) load(addr + j + i * 8) << (j * 8);
          }
          fetch_banks[i].data = insn;
          fetch_banks[i].source = src;
          fetch_banks[i].remaining = true;
        }
        return;
      }
      // fetch
      case TlOpcode::AcquireBlock: {
        cnt = 1;
        LOG(INFO) << fmt::format("acquire fetch  here ");
        for (int i = 0; i < 8; i++) {
          uint64_t data = 0;
          for (int j = 0; j < 8; ++j) {
            data += (uint64_t) load(addr + j + i * 8) << (j * 8);
          }
          aquire_banks[i].data = data;
          aquire_banks[i].param = param;
          aquire_banks[i].source = src;
          aquire_banks[i].remaining = true;
        }
      }
      default:
        LOG(FATAL) << fmt::format("unknown tl opcode {}", opcode);
    }
  }
  // find corresponding SpikeEvent with addr
  SpikeEvent *se = nullptr;
  for (auto se_iter = to_rtl_queue.rbegin(); se_iter != to_rtl_queue.rend(); se_iter++) {
    if (addr == se_iter->block.addr) {
      se = &(*se_iter);
      LOG(INFO) << fmt::format("success find acqure Spike pc = {:08X}", se_iter->pc);
      break;
    }
  }
  // list the queue if error
  if (se == nullptr) {
    for (auto se_iter = to_rtl_queue.rbegin(); se_iter != to_rtl_queue.rend(); se_iter++) {
      LOG(INFO) << fmt::format("List: spike pc = {:08X}, write reg({}) from {:08x} to {:08X}, is commit:{}",
                               se_iter->pc, se_iter->rd_idx, se_iter->rd_old_bits, se_iter->rd_new_bits, se_iter->is_committed);
      LOG(INFO) << fmt::format("List:spike block.addr = {:08X}", se_iter->block.addr);
    }
    LOG(FATAL) << fmt::format("cannot find spike_event for tl_request; addr = {:08X}, pc = {:08X} , opcode = {}", addr, pc, opcode);
  }

  switch (opcode) {

    case TlOpcode::Get: {
      auto mem_read = se->mem_access_record.all_reads.find(addr);
      CHECK_S(mem_read != se->mem_access_record.all_reads.end())
          << fmt::format(": [{}] cannot find mem read of addr {:08X}", get_t(), addr);
      CHECK_EQ_S(mem_read->second.size_by_byte, decode_size(size)) << fmt::format(
          ": [{}] expect mem read of size {}, actual size {} (addr={:08X}, {})",
          get_t(), mem_read->second.size_by_byte, 1 << decode_size(size), addr, se->describe_insn());

      uint64_t data = mem_read->second.val;
      LOG(INFO) << fmt::format("[{}] receive rtl mem get req (addr={}, size={}byte), should return data {}",
                               get_t(), addr, decode_size(size), data);
      tl_banks.emplace(std::make_pair(addr, TLReqRecord{
                                                data, 1u << size, src, TLReqRecord::opType::Get, get_mem_req_cycles()}));
      mem_read->second.executed = true;
      break;
    }

    case TlOpcode::PutFullData: {
      uint32_t data = TL(a_bits_data);
      LOG(INFO) << fmt::format("[{}] receive rtl mem put req (addr={:08X}, size={}byte, data={})",
                               addr, decode_size(size), data);
      auto mem_write = se->mem_access_record.all_writes.find(addr);

      CHECK_S(mem_write != se->mem_access_record.all_writes.end())
          << fmt::format(": [{}] cannot find mem write of addr={:08X}", get_t(), addr);
      CHECK_EQ_S(mem_write->second.size_by_byte, decode_size(size)) << fmt::format(
          ": [{}] expect mem write of size {}, actual size {} (addr={:08X}, insn='{}')",
          get_t(), mem_write->second.size_by_byte, 1 << decode_size(size), addr, se->describe_insn());
      CHECK_EQ_S(mem_write->second.val, data) << fmt::format(
          ": [{}] expect mem write of data {}, actual data {} (addr={:08X}, insn='{}')",
          get_t(), mem_write->second.size_by_byte, 1 << decode_size(size), addr, se->describe_insn());

      tl_banks.emplace(std::make_pair(addr, TLReqRecord{
                                                data, 1u << size, src, TLReqRecord::opType::PutFullData, get_mem_req_cycles()}));
      mem_write->second.executed = true;
      break;
    }

    case TlOpcode::AcquireBlock: {
      cnt = 1;
      LOG(INFO) << fmt::format("Find AcquireBlock for mem = {:08X}", addr);
      for (int i = 0; i < 8; i++) {
        uint64_t data = 0;
        for (int j = 0; j < 8; ++j) {
          data += (uint64_t) load(addr + j + i * 8) << (j * 8);
        }
        aquire_banks[i].data = se->block.blocks[i];
        aquire_banks[i].param = param;
        aquire_banks[i].source = src;
        aquire_banks[i].remaining = true;
      }
      break;
    }

#undef TL
  }
}


void VBridgeImpl::return_fetch_response() {
#define TL(name) (get_tl_##name(top))
  bool fetch_valid = false;
  bool aqu_valid = false;
  uint8_t size = 0;
  uint16_t source = 0;
  uint16_t param = 0;
  for (auto &fetch_bank: fetch_banks) {
    if (fetch_bank.remaining) {
      fetch_bank.remaining = false;
      TL(d_bits_opcode) = 1;
      TL(d_bits_data) = fetch_bank.data;
      source = fetch_bank.source;
      size = 6;
      fetch_valid = true;
      break;
    }
  }
  // todo: source for acquire?
  for (auto &aquire_bank: aquire_banks) {
    if (cnt) {
      cnt = 0;
      break;
    }
    if (aquire_bank.remaining) {
      aquire_bank.remaining = false;
      TL(d_bits_opcode) = 5;
      TL(d_bits_data) = aquire_bank.data;
      TL(d_bits_param) = 0;
      source = aquire_bank.source;
      size = 6;
      aqu_valid = true;
    }
  }
output:
  TL(d_bits_source) = source;
  TL(d_bits_size) = size;
  TL(d_valid) = fetch_valid | aqu_valid;
#undef TL
}
