#include "mm_model.h"
#include "htif_phy.h"
#include "mm_types.h"
#include "mm_emulator.cc"
#include <DirectC.h>

htif_phy_t* htif_phy = NULL;

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
  vc_handle mem_resp_tag,
  vc_handle mem_resp_data)
{
  uint64_t req_data[MM_WORD_SIZE*refill_size/sizeof(uint64_t)];
  for (size_t i = 0; i < MM_WORD_SIZE*refill_size/sizeof(uint32_t); i++)
    ((uint32_t*)req_data)[i] = vc_4stVectorRef(mem_req_data_bits)[i].d;

  uint64_t req_rdy, req_data_rdy, resp_val, resp_tag;
  uint64_t resp_data[MM_WORD_SIZE*refill_size/sizeof(uint64_t)];

  mm_tick_emulator(
    vc_getScalar(mem_req_val),
    &req_rdy,
    vc_getScalar(mem_req_store),
    vc_4stVectorRef(mem_req_addr)->d,
    vc_4stVectorRef(mem_req_tag)->d,

    vc_getScalar(mem_req_data_val),
    &req_data_rdy,
    req_data,

    &resp_val,
    &resp_tag,
    resp_data
  );

  vc_putScalar(mem_req_rdy, req_rdy);
  vc_putScalar(mem_req_data_rdy, req_data_rdy);
  vc_putScalar(mem_resp_val, resp_val);

  vec32 t;
  t.c = 0;
  t.d = resp_tag;
  vc_put4stVector(mem_resp_tag, &t);

  vec32 d[MM_WORD_SIZE*refill_size/sizeof(uint32_t)];
  for (size_t i = 0; i < MM_WORD_SIZE*refill_size/sizeof(uint32_t); i++)
  {
    d[i].c = 0;
    d[i].d = ((uint32_t*)resp_data)[i];
  }
  vc_put4stVector(mem_resp_data, d);
}

void htif_init
(
  vc_handle fromhost,
  vc_handle tohost,
  vc_handle width,
  vc_handle loadmem
)
{
  uint64_t* mem = mm_init();

  vec32* fh = vc_4stVectorRef(fromhost);
  vec32* th = vc_4stVectorRef(tohost);
  vec32* w = vc_4stVectorRef(width);

  char loadmem_str[1024];
  vc_VectorToString(loadmem, loadmem_str);
  if (*loadmem_str)
    load_mem(mem, loadmem_str);

  assert(w->d <= 32); // htif_tick assumes data fits in a vec32
  htif_phy = new htif_phy_t(w->d, fh->d, th->d);
}

void htif_tick
(
  vc_handle htif_in_valid,
  vc_handle htif_in_ready,
  vc_handle htif_in_bits,
  vc_handle htif_out_valid,
  vc_handle htif_out_ready,
  vc_handle htif_out_bits
)
{
  vec32* ob = vc_4stVectorRef(htif_out_bits);
  htif_phy->tick(vc_getScalar(htif_in_ready), vc_getScalar(htif_out_valid), ob->d);

  vc_putScalar(htif_in_valid, htif_phy->in_valid());
  vc_putScalar(htif_out_ready, htif_phy->out_ready());

  vec32 ib;
  ib.c = 0;
  ib.d = htif_phy->in_bits();
  vc_put4stVector(htif_in_bits, &ib);
}

}
