// See LICENSE for license details.

#ifndef _MM_EMULATOR_DRAMSIM2_H
#define _MM_EMULATOR_DRAMSIM2_H

#include "mm.h"
#include <DRAMSim.h>
#include <map>
#include <queue>
#include <stdint.h>

class mm_dramsim2_t : public mm_t
{
 public:
  mm_dramsim2_t() : store_inflight(false), store_count(0) {}

  virtual void init(size_t sz, int word_size, int line_size);

  virtual bool req_cmd_ready() { return mem->willAcceptTransaction() && !store_inflight; }
  virtual bool req_data_ready() { return mem->willAcceptTransaction() && store_inflight; }
  virtual bool resp_valid() { return !resp.empty(); }
  virtual uint64_t resp_tag() { return resp_valid() ? resp.front().first : 0; }
  virtual void* resp_data() { return resp_valid() ? &resp.front().second[0] : &dummy_data[0]; }

  virtual void tick
  (
    bool req_cmd_val,
    bool req_cmd_store,
    uint64_t req_cmd_addr,
    uint64_t req_cmd_tag,
    bool req_data_val,
    void* req_data_bits,
    bool resp_rdy
  );


 protected:
  DRAMSim::MultiChannelMemorySystem *mem;
  uint64_t cycle;

  bool store_inflight;
  int store_count;
  uint64_t store_addr;
  std::vector<char> dummy_data;

  std::map<uint64_t,uint64_t> req;
  std::queue<std::pair<uint64_t, std::vector<char>>> resp;

  void read_complete(unsigned id, uint64_t address, uint64_t clock_cycle);
  void write_complete(unsigned id, uint64_t address, uint64_t clock_cycle);
};

#endif
