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
  mm_rresp_t resp;
  do {
    resp = rreq[address].front();
    rresp.push(resp);
    rreq[address].pop();
  } while (!resp.last);
}

void mm_dramsim2_t::write_complete(unsigned id, uint64_t address, uint64_t clock_cycle)
{
  auto b_id = wreq[address].front();
  bresp.push(b_id);
  wreq[address].pop();
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

void mm_dramsim2_t::tick(
  bool ar_valid,
  uint64_t ar_addr,
  uint64_t ar_id,
  uint64_t ar_size,
  uint64_t ar_len,

  bool aw_valid,
  uint64_t aw_addr,
  uint64_t aw_id,
  uint64_t aw_size,
  uint64_t aw_len,

  bool w_valid,
  uint64_t w_strb,
  void *w_data,
  bool w_last,

  bool r_ready,
  bool b_ready)
{
  bool ar_fire = ar_valid && ar_ready();
  bool aw_fire = aw_valid && aw_ready();
  bool w_fire = w_valid && w_ready();
  bool r_fire = r_valid() && r_ready;
  bool b_fire = b_valid() && b_ready;

  if (ar_fire) {
    uint64_t start_addr = (ar_addr / word_size) * word_size;
    for (int i = 0; i <= ar_len; i++) {
      auto dat = read(start_addr + i * word_size);
      rreq[ar_addr].push(mm_rresp_t(ar_id, dat, (i == ar_len)));
    }
    mem->addTransaction(false, ar_addr);
  }

  if (aw_fire) {
    store_addr = aw_addr;
    store_id = aw_id;
    store_count = aw_len + 1;
    store_size = 1 << aw_size;
    store_inflight = true;
  }

  if (w_fire) {
    write(store_addr, (uint8_t *) w_data, w_strb, store_size);
    store_addr += store_size;
    store_count--;

    if (store_count == 0) {
      store_inflight = false;
      mem->addTransaction(true, store_addr);
      wreq[store_addr].push(store_id);
      assert(w_last);
    }
  }

  if (b_fire)
    bresp.pop();

  if (r_fire)
    rresp.pop();

  mem->update();
  cycle++;
}
