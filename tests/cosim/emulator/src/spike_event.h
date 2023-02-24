#pragma once

#include <optional>
#include <queue>

#include "mmu.h"
#include "processor.h"

#include "VV.h"
#include "verilated_fst_c.h"

#include "simple_sim.h"
#include "vbridge_config.h"
#include "vbridge_impl.h"

class VBridgeImpl;

struct Cacheblock {
  uint64_t addr;
  uint64_t blocks[8];
  bool remaining;
};

struct SpikeEvent {
  SpikeEvent(processor_t &proc, insn_fetch_t &fetch, VBridgeImpl *impl);

  [[nodiscard]] std::string describe_insn() const;

  void pre_log_arch_changes();
  void log_arch_changes();

  commit_log_mem_t mem_read_info;

  struct mem_log {
    uint64_t addr;
    uint64_t value;
    uint8_t size;
  };
  std::vector<mem_log> log_mem_queue;

  processor_t &proc;
  VBridgeImpl *impl;

  bool is_issued;
  bool is_committed;

  uint8_t opcode;
  bool is_load;
  bool is_store;
  bool is_csr;
  bool is_amo;

  std::string disasm;


  uint64_t pc;
  uint32_t inst_bits;
  bool is_compress;

  // scalar to vector interface(used for driver)
  uint32_t rs1_bits;
  uint32_t rs2_bits;
  // rd idx and bits before insn
  uint32_t rd_idx;
  uint64_t rd_old_bits;
  // rd idx and bits after insn
  uint64_t rd_new_bits;
  bool is_rd_written;

  //csr
  uint64_t satp;
  uint64_t satp_ppn;
  uint8_t satp_mode;

  bool is_trap;

  Cacheblock block;

  uint64_t target_mem;
  std::list<Cacheblock> cache_queue;

  struct {
    struct single_mem_write {
      uint32_t size_by_byte;
      reg_t val;
      bool executed = false;// set to true when rtl execute this mem access
    };
    struct single_mem_read {
      uint16_t size_by_byte;
      reg_t val;
      bool executed = false;// set to true when rtl execute this mem access
    };
    std::map<uint32_t, single_mem_write> all_writes;
    std::map<uint32_t, single_mem_read> all_reads;
  } mem_access_record;
};
