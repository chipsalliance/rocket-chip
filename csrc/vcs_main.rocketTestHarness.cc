// See LICENSE for license details.

#include "htif_emulator.h"
#include "mm.h"
#include "mm_dramsim2.h"
#include <DirectC.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <sstream>
#include <iterator>

extern "C" {

extern int vcs_main(int argc, char** argv);

static const int MEMORY_CHANNEL_MUX_CONFIGS[] = {
#ifdef MEMORY_CHANNEL_MUX_CONFIGS__0
  MEMORY_CHANNEL_MUX_CONFIGS__0,
#endif
#ifdef MEMORY_CHANNEL_MUX_CONFIGS__1
  MEMORY_CHANNEL_MUX_CONFIGS__1,
#endif
#ifdef MEMORY_CHANNEL_MUX_CONFIGS__2
#error "Add a preprocessor repeat macro"
#endif
};

static htif_emulator_t* htif;
static unsigned htif_bytes = HTIF_WIDTH / 8;
static mm_t* mm[N_MEM_CHANNELS];
static const char* loadmem;
static bool dramsim = false;
static int memory_channel_mux_select = 0;

void htif_fini(vc_handle failure)
{
  delete htif;
  htif = NULL;
  exit(vc_getScalar(failure));
}

int main(int argc, char** argv)
{
  unsigned long memsz_mb = MEM_SIZE / (1024*1024);

  for (int i = 1; i < argc; i++)
  {
    if (!strcmp(argv[i], "+dramsim"))
      dramsim = true;
    else if (!strncmp(argv[i], "+loadmem=", 9))
      loadmem = argv[i]+9;
    else if (!strncmp(argv[i], "+memory_channel_mux_select=", 27))
      memory_channel_mux_select = atoi(argv[i]+27);
  }

  int enabled_mem_channels = MEMORY_CHANNEL_MUX_CONFIGS[memory_channel_mux_select];

  htif = new htif_emulator_t(memsz_mb,
          std::vector<std::string>(argv + 1, argv + argc));

  for (int i=0; i<N_MEM_CHANNELS; i++) {
    mm[i] = dramsim ? (mm_t*)(new mm_dramsim2_t) : (mm_t*)(new mm_magic_t);
    mm[i]->init(MEM_SIZE / enabled_mem_channels, MEM_DATA_BITS / 8, CACHE_BLOCK_BYTES);
  }

  if (loadmem) {
    void *mems[N_MEM_CHANNELS];
    for (int i = 0; i < N_MEM_CHANNELS; i++)
      mems[i] = mm[i]->get_data();
    load_mem(mems, loadmem, CACHE_BLOCK_BYTES, enabled_mem_channels);
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

  mmc->tick
  (
    vc_getScalar(ar_valid),
    vc_4stVectorRef(ar_addr)->d,
    vc_4stVectorRef(ar_id)->d,
    vc_4stVectorRef(ar_size)->d,
    vc_4stVectorRef(ar_len)->d,

    vc_getScalar(aw_valid),
    vc_4stVectorRef(aw_addr)->d,
    vc_4stVectorRef(aw_id)->d,
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
  d[0].d = mmc->b_id();
  vc_put4stVector(b_id, d);

  d[0].c = 0;
  d[0].d = mmc->r_resp();
  vc_put4stVector(r_resp, d);

  d[0].c = 0;
  d[0].d = mmc->r_id();
  vc_put4stVector(r_id, d);

  for (size_t i = 0; i < mmc->get_word_size()/sizeof(uint32_t); i++)
  {
    d[i].c = 0;
    d[i].d = ((uint32_t*)mmc->r_data())[i];
  }
  vc_put4stVector(r_data, d);
}

void htif_tick
(
  vc_handle htif_in_valid,
  vc_handle htif_in_ready,
  vc_handle htif_in_bits,
  vc_handle htif_out_valid,
  vc_handle htif_out_ready,
  vc_handle htif_out_bits,
  vc_handle exit
)
{
  static bool peek_in_valid;
  static uint32_t peek_in_bits;
  if (vc_getScalar(htif_in_ready))
    peek_in_valid = htif->recv_nonblocking(&peek_in_bits, htif_bytes);

  vc_putScalar(htif_out_ready, 1);
  if (vc_getScalar(htif_out_valid))
  {
    vec32* bits = vc_4stVectorRef(htif_out_bits);
    htif->send(&bits->d, htif_bytes);
  }

  vec32 bits = {0, 0};
  bits.d = peek_in_bits;
  vc_put4stVector(htif_in_bits, &bits);
  vc_putScalar(htif_in_valid, peek_in_valid);

  bits.d = htif->done() ? (htif->exit_code() << 1 | 1) : 0;
  vc_put4stVector(exit, &bits);
}

}
