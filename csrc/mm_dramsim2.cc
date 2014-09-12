// See LICENSE for license details.

#include "mm_dramsim2.h"
#include "mm.h"
#include <DRAMSim.h>
#include <iostream>
#include <fstream>
#include <list>
#include <queue>
#include <cstring>
#include <cstdlib>
#include <cassert>

//#define DEBUG_DRAMSIM2

using namespace DRAMSim;

void mm_dramsim2_t::read_complete(unsigned id, uint64_t address, uint64_t clock_cycle)
{
  assert(req.count(address));
  auto tag = req[address];
  req.erase(address);

  for (int i = 0; i < line_size/word_size; i++)
  {
    auto base = data + address + i*word_size;
    auto dat = std::vector<char>(base, base + word_size);
    resp.push(std::make_pair(tag, dat));
  }

#ifdef DEBUG_DRAMSIM2
  fprintf(stderr, "[Callback] read complete: id=%d , addr=0x%lx , cycle=%lu\n", id, address, clock_cycle);
#endif
}

void mm_dramsim2_t::write_complete(unsigned id, uint64_t address, uint64_t clock_cycle)
{
#ifdef DEBUG_DRAMSIM2
  fprintf(stderr, "[Callback] write complete: id=%d , addr=0x%lx , cycle=%lu\n", id, address, clock_cycle);
#endif
}

void power_callback(double a, double b, double c, double d)
{
    //fprintf(stderr, "power callback: %0.3f, %0.3f, %0.3f, %0.3f\n",a,b,c,d);
}

void mm_dramsim2_t::init(size_t sz, int wsz, int lsz)
{
  assert(lsz == 64); // assumed by dramsim2
  mm_t::init(sz, wsz, lsz);

  dummy_data.resize(word_size);

  assert(size % (1024*1024) == 0);
  mem = getMemorySystemInstance("DDR3_micron_64M_8B_x4_sg15.ini", "system.ini", "dramsim2_ini", "results", size/(1024*1024));

  TransactionCompleteCB *read_cb = new Callback<mm_dramsim2_t, void, unsigned, uint64_t, uint64_t>(this, &mm_dramsim2_t::read_complete);
  TransactionCompleteCB *write_cb = new Callback<mm_dramsim2_t, void, unsigned, uint64_t, uint64_t>(this, &mm_dramsim2_t::write_complete);
  mem->RegisterCallbacks(read_cb, write_cb, power_callback);

#ifdef DEBUG_DRAMSIM2
  fprintf(stderr,"Dramsim2 init successful\n");
#endif
}

void mm_dramsim2_t::tick
(
  bool req_cmd_val,
  bool req_cmd_store,
  uint64_t req_cmd_addr,
  uint64_t req_cmd_tag,
  bool req_data_val,
  void* req_data_bits,
  bool resp_rdy
)
{
  bool req_cmd_fire = req_cmd_val && req_cmd_ready();
  bool req_data_fire = req_data_val && req_data_ready();
  bool resp_fire = resp_valid() && resp_rdy;
  assert(!(req_cmd_fire && req_data_fire));

  if (resp_fire)
    resp.pop();

  if (req_cmd_fire)
  {
    // since the I$ can speculatively ask for address that are out of bounds
    auto byte_addr = (req_cmd_addr * line_size) % size;

    if (req_cmd_store)
    {
      store_inflight = 1;
      store_addr = byte_addr;
#ifdef DEBUG_DRAMSIM2
      fprintf(stderr, "Starting store transaction (addr=%lx ; tag=%ld ; cyc=%ld)\n", store_addr, req_cmd_tag, cycle);
#endif
    }
    else
    {
      assert(!req.count(byte_addr));
      req[byte_addr] = req_cmd_tag;

      mem->addTransaction(false, byte_addr);
#ifdef DEBUG_DRAMSIM2
      fprintf(stderr, "Adding load transaction (addr=%lx; cyc=%ld)\n", byte_addr, cycle);
#endif
    }
  }

  if (req_data_fire)
  {
    memcpy(data + store_addr + store_count*word_size, req_data_bits, word_size);

    store_count = (store_count + 1) % (line_size/word_size);
    if (store_count == 0)
    { // last chunch of cache line arrived.
      store_inflight = 0;
      mem->addTransaction(true, store_addr);
#ifdef DEBUG_DRAMSIM2
      fprintf(stderr, "Adding store transaction (addr=%lx; cyc=%ld)\n", store_addr, cycle);
#endif
    }
  }

  mem->update();
  cycle++;
}
