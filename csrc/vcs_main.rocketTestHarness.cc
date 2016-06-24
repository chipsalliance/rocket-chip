// See LICENSE for license details.

#include "mm.h"
#include "mm_dramsim2.h"
#include <fesvr/dtm.h>
#include <assert.h>
#include <DirectC.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <sstream>
#include <iterator>

extern "C" {

extern int vcs_main(int argc, char** argv);

static dtm_t* dtm;
static mm_t* mm[N_MEM_CHANNELS];
static const char* loadmem;
static bool dramsim = false;
static int memory_channel_mux_select = 0;

int main(int argc, char** argv)
{
  for (int i = 1; i < argc; i++)
  {
    if (!strcmp(argv[i], "+dramsim"))
      dramsim = true;
    else if (!strncmp(argv[i], "+loadmem=", 9))
      loadmem = argv[i]+9;
    else if (!strncmp(argv[i], "+memory_channel_mux_select=", 27))
      memory_channel_mux_select = atoi(argv[i]+27);
  }

  dtm = new dtm_t(std::vector<std::string>(argv + 1, argv + argc));

  for (int i=0; i<N_MEM_CHANNELS; i++) {
    mm[i] = dramsim ? (mm_t*)(new mm_dramsim2_t) : (mm_t*)(new mm_magic_t);
    mm[i]->init(MEM_SIZE / N_MEM_CHANNELS, MEM_DATA_BITS / 8, CACHE_BLOCK_BYTES);
  }

  if (loadmem) {
    void *mems[N_MEM_CHANNELS];
    for (int i = 0; i < N_MEM_CHANNELS; i++)
      mems[i] = mm[i]->get_data();
    load_mem(mems, loadmem, CACHE_BLOCK_BYTES, N_MEM_CHANNELS);
  }

  vcs_main(argc, argv);
  abort(); // should never get here
}

void memory_tick(
  vc_handle channel,

  vc_handle ar_valid,
  vc_handle ar_ready,
  vc_handle ar_addr,
  vc_handle ar_id,
  vc_handle ar_size,
  vc_handle ar_len,

  vc_handle aw_valid,
  vc_handle aw_ready,
  vc_handle aw_addr,
  vc_handle aw_id,
  vc_handle aw_size,
  vc_handle aw_len,

  vc_handle w_valid,
  vc_handle w_ready,
  vc_handle w_strb,
  vc_handle w_data,
  vc_handle w_last,

  vc_handle r_valid,
  vc_handle r_ready,
  vc_handle r_resp,
  vc_handle r_id,
  vc_handle r_data,
  vc_handle r_last,

  vc_handle b_valid,
  vc_handle b_ready,
  vc_handle b_resp,
  vc_handle b_id)
{
  int c = vc_4stVectorRef(channel)->d;
  assert(c < N_MEM_CHANNELS);
  mm_t* mmc = mm[c];

  uint32_t write_data[mmc->get_word_size()/sizeof(uint32_t)];
  for (size_t i = 0; i < mmc->get_word_size()/sizeof(uint32_t); i++)
    write_data[i] = vc_4stVectorRef(w_data)[i].d;

  uint32_t aw_id_val, ar_id_val;

  if (MEM_ID_BITS == 1) {
    aw_id_val = vc_getScalar(aw_id);
    ar_id_val = vc_getScalar(ar_id);
  } else {
    aw_id_val = vc_4stVectorRef(aw_id)->d;
    ar_id_val = vc_4stVectorRef(ar_id)->d;
  }

  mmc->tick
  (
    vc_getScalar(ar_valid),
    vc_4stVectorRef(ar_addr)->d - MEM_BASE,
    ar_id_val,
    vc_4stVectorRef(ar_size)->d,
    vc_4stVectorRef(ar_len)->d,

    vc_getScalar(aw_valid),
    vc_4stVectorRef(aw_addr)->d - MEM_BASE,
    aw_id_val,
    vc_4stVectorRef(aw_size)->d,
    vc_4stVectorRef(aw_len)->d,

    vc_getScalar(w_valid),
    vc_4stVectorRef(w_strb)->d,
    write_data,
    vc_getScalar(w_last),

    vc_getScalar(r_ready),
    vc_getScalar(b_ready)
  );

  vc_putScalar(ar_ready, mmc->ar_ready());
  vc_putScalar(aw_ready, mmc->aw_ready());
  vc_putScalar(w_ready, mmc->w_ready());
  vc_putScalar(b_valid, mmc->b_valid());
  vc_putScalar(r_valid, mmc->r_valid());
  vc_putScalar(r_last, mmc->r_last());

  vec32 d[mmc->get_word_size()/sizeof(uint32_t)];

  d[0].c = 0;
  d[0].d = mmc->b_resp();
  vc_put4stVector(b_resp, d);

  d[0].c = 0;
  d[0].d = mmc->r_resp();
  vc_put4stVector(r_resp, d);

  if (MEM_ID_BITS > 1) {
    d[0].c = 0;
    d[0].d = mmc->b_id();
    vc_put4stVector(b_id, d);

    d[0].c = 0;
    d[0].d = mmc->r_id();
    vc_put4stVector(r_id, d);
  } else {
    vc_putScalar(b_id, mmc->b_id());
    vc_putScalar(r_id, mmc->r_id());
  }

  for (size_t i = 0; i < mmc->get_word_size()/sizeof(uint32_t); i++)
  {
    d[i].c = 0;
    d[i].d = ((uint32_t*)mmc->r_data())[i];
  }
  vc_put4stVector(r_data, d);
}

void debug_tick
(
  vc_handle debug_req_valid,
  vc_handle debug_req_ready,
  vc_handle debug_req_bits_addr,
  vc_handle debug_req_bits_op,
  vc_handle debug_req_bits_data,
  vc_handle debug_resp_valid,
  vc_handle debug_resp_ready,
  vc_handle debug_resp_bits_resp,
  vc_handle debug_resp_bits_data,
  vc_handle exit
)
{
  vec32 tmp[2];
  dtm_t::resp resp_bits;
  vc_get4stVector(debug_resp_bits_resp, tmp);
  resp_bits.resp = tmp[0].d;
  vc_get4stVector(debug_resp_bits_data, tmp);
  resp_bits.data = tmp[0].d | ((uint64_t)tmp[1].d << 32);

  dtm->tick
  (
    vc_getScalar(debug_req_ready),
    vc_getScalar(debug_resp_valid),
    resp_bits
  );

  vc_putScalar(debug_resp_ready, dtm->resp_ready());
  vc_putScalar(debug_req_valid, dtm->req_valid());
  tmp[0].d = dtm->req_bits().addr;
  vc_put4stVector(debug_req_bits_addr, tmp);
  tmp[0].d = dtm->req_bits().op;
  vc_put4stVector(debug_req_bits_op, tmp);
  tmp[0].d = dtm->req_bits().data;
  tmp[1].d = dtm->req_bits().data >> 32;
  vc_put4stVector(debug_req_bits_data, tmp);
  tmp[0].d = dtm->done() ? (dtm->exit_code() << 1 | 1) : 0;
  vc_put4stVector(exit, tmp);
}

}
