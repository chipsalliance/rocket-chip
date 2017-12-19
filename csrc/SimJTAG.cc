// See LICENSE.SiFive for license details.

#include <vpi_user.h>
#include <svdpi.h>
#include <cstdlib>
#include "remote_bitbang.h"

remote_bitbang_t* jtag;

extern "C" int jtag_tick
(
 unsigned char * jtag_TCK,
 unsigned char * jtag_TMS,
 unsigned char * jtag_TDI,
 unsigned char * jtag_TRSTn,
 unsigned char jtag_TDO
)
{
  if (!jtag) {
    s_vpi_vlog_info info;
    if (!vpi_get_vlog_info(&info)) {
      abort();
    }
    // TODO: Pass in real port number
    jtag = new remote_bitbang_t(0);
  }

  jtag->tick(jtag_TCK, jtag_TMS, jtag_TDI, jtag_TRSTn, jtag_TDO);

  return jtag->done() ? (jtag->exit_code() << 1 | 1) : 0;

}
