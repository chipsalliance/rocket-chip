#include "htif_emulator.h"
#include "mm.h"
#include "mm_dramsim2.h"
#include <DirectC.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <sstream>
#include <iterator>

static htif_emulator_t* htif = NULL;
static unsigned htif_bytes;
static mm_t* mm = NULL;

extern "C" {

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

void htif_init
(
  vc_handle htif_width,
  vc_handle mem_width,
  vc_handle argv,
  vc_handle loadmem,
  vc_handle dramsim
)
{
  int mw = vc_4stVectorRef(mem_width)->d;
  mm = vc_getScalar(dramsim) ? (mm_t*)(new mm_dramsim2_t) : (mm_t*)(new mm_magic_t);
  assert(mw && (mw & (mw-1)) == 0);
  mm->init(MEM_SIZE, mw/8, LINE_SIZE);

  vec32* w = vc_4stVectorRef(htif_width);
  assert(w->d <= 32 && w->d % 8 == 0); // htif_tick assumes data fits in a vec32
  htif_bytes = w->d/8;

  char loadmem_str[1024];
  vc_VectorToString(loadmem, loadmem_str);
  if (*loadmem_str)
    load_mem(mm->get_data(), loadmem_str);

  char argv_str[1024];
  vc_VectorToString(argv, argv_str);
  if (!*argv_str)
  {
    if (*loadmem_str)
      strcpy(argv_str, "none");
    else
    {
      fprintf(stderr, "Usage: ./simv [host options] +argv=\"<target program> [target args]\"\n");
      exit(-1);
    }
  }

  std::vector<std::string> args;
  std::stringstream ss(argv_str);
  std::istream_iterator<std::string> begin(ss), end;
  std::copy(begin, end, std::back_inserter<std::vector<std::string>>(args));

  htif = new htif_emulator_t(args);
}

void htif_fini()
{
  delete htif;
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
