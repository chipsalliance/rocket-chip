// See LICENSE.SiFive for license details.

#include <vpi_user.h>
#include <svdpi.h>
#include <cstdlib>
#include "remote_bitbang.h"

remote_bitbang_t* remote_bitbang;

extern "C" int jtag_tick
(
 bool * jtag_TCK,
 bool * jtag_TMS,
 bool * jtag_TDI,
 bool * jtag_TRSTn,
 bool jtag_TDO
)
{
  if (!remote_bitbang) {
    s_vpi_vlog_info info;
    if (!vpi_get_vlog_info(&info)) {
      abort();
    }
    // TODO: Pass in real port number
    remote_bitbang = new remote_bitbang_t(0);
  }
  
  remote_bitbang->tick(jtag_TCK, jtag_TMS, jtag_TDI, jtag_TRSTn, jtag_TDO);
  
  return remote_bitbang->done() ? (remote_bitbang->exit_code() << 1 | 1) : 0;

}
