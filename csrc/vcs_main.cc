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

static htif_emulator_t* htif;
static unsigned htif_bytes;
static mm_t* mm;
static const char* loadmem;

void htif_fini(vc_handle failure)
{
  delete htif;
  htif = NULL;
  exit(vc_getScalar(failure));
}

int main(int argc, char** argv)
{
  bool dramsim = false;
  unsigned long memsz_mb = MEM_SIZE / (1024*1024);

  for (int i = 1; i < argc; i++)
  {
    if (!strcmp(argv[i], "+dramsim"))
      dramsim = true;
    else if (!strncmp(argv[i], "+loadmem=", 9))
      loadmem = argv[i]+9;
  }

  mm = dramsim ? (mm_t*)(new mm_dramsim2_t) : (mm_t*)(new mm_magic_t);
  htif = new htif_emulator_t(memsz_mb,
          std::vector<std::string>(argv + 1, argv + argc));

  vcs_main(argc, argv);
  abort(); // should never get here
}

void memory_tick(
  vc_handle mem_req_val,
  vc_handle mem_req_rdy,
  vc_handle mem_req_store,
  vc_handle mem_req_addr,
  vc_handle mem_req_tag,

  vc_handle mem_req_data_val,
  vc_handle mem_req_data_rdy,
  vc_handle mem_req_data_bits,

  vc_handle mem_resp_val,
  vc_handle mem_resp_rdy,
  vc_handle mem_resp_tag,
  vc_handle mem_resp_data)
{
  uint32_t req_data[mm->get_word_size()/sizeof(uint32_t)];
  for (size_t i = 0; i < mm->get_word_size()/sizeof(uint32_t); i++)
    req_data[i] = vc_4stVectorRef(mem_req_data_bits)[i].d;

  vc_putScalar(mem_req_rdy, mm->req_cmd_ready());
  vc_putScalar(mem_req_data_rdy, mm->req_data_ready());
  vc_putScalar(mem_resp_val, mm->resp_valid());

  vec32 d[mm->get_word_size()/sizeof(uint32_t)];
  d[0].c = 0;
  d[0].d = mm->resp_tag();
  vc_put4stVector(mem_resp_tag, d);

  for (size_t i = 0; i < mm->get_word_size()/sizeof(uint32_t); i++)
  {
    d[i].c = 0;
    d[i].d = ((uint32_t*)mm->resp_data())[i];
  }
  vc_put4stVector(mem_resp_data, d);

  mm->tick
  (
    vc_getScalar(mem_req_val),
    vc_getScalar(mem_req_store),
    vc_4stVectorRef(mem_req_addr)->d,
    vc_4stVectorRef(mem_req_tag)->d,
    vc_getScalar(mem_req_data_val),
    req_data,
    vc_getScalar(mem_resp_rdy)
  );
}

void htif_init(vc_handle htif_width, vc_handle mem_width)
{
  int mw = vc_4stVectorRef(mem_width)->d;
  assert(mw && (mw & (mw-1)) == 0);
  mm->init(MEM_SIZE, mw/8, LINE_SIZE);

  if (loadmem)
    load_mem(mm->get_data(), loadmem);

  vec32* w = vc_4stVectorRef(htif_width);
  assert(w->d <= 32 && w->d % 8 == 0); // htif_tick assumes data fits in a vec32
  htif_bytes = w->d/8;
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
