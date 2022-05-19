// See LICENSE.SiFive for license details.

#include <fesvr/dtm.h>
#include <vpi_user.h>
#include <svdpi.h>

dtm_t* dtm;

extern "C" int debug_tick
(
  unsigned char* debug_req_valid,
  unsigned char  debug_req_ready,
  int*           debug_req_bits_addr,
  int*           debug_req_bits_op,
  int*           debug_req_bits_data,
  unsigned char  debug_resp_valid,
  unsigned char* debug_resp_ready,
  int            debug_resp_bits_resp,
  int            debug_resp_bits_data
)
{
  if (!dtm) {
    s_vpi_vlog_info info;
    if (!vpi_get_vlog_info(&info))
      abort();
      dtm = new dtm_t(info.argc, info.argv);
  }

  dtm_t::resp resp_bits;
  resp_bits.resp = debug_resp_bits_resp;
  resp_bits.data = debug_resp_bits_data;

  dtm->tick
  (
    debug_req_ready,
    debug_resp_valid,
    resp_bits
  );

  *debug_resp_ready = dtm->resp_ready();
  *debug_req_valid = dtm->req_valid();
  *debug_req_bits_addr = dtm->req_bits().addr;
  *debug_req_bits_op = dtm->req_bits().op;
  *debug_req_bits_data = dtm->req_bits().data;

  return dtm->done() ? (dtm->exit_code() << 1 | 1) : 0;
}
