#include <vpi_user.h>
#include <svdpi.h>
#include <vector>
#include <string>
#include "HurricaneTSI.h"

hurricane_tsi_t *tsi = NULL;

extern "C" int serial_tick(
        unsigned char out_valid,
        unsigned char *out_ready,
        unsigned int  out_bits,

        unsigned char *in_valid,
        unsigned char in_ready,
        unsigned int  *in_bits)
{
    if (!tsi) {
        s_vpi_vlog_info info;
        if (!vpi_get_vlog_info(&info))
          abort();
        tsi = new hurricane_tsi_t(std::vector<std::string>(info.argv + 1, info.argv + info.argc));
    }

    *out_ready = true;
    if (out_valid) {
        tsi->send_word(out_bits);
    }

    *in_valid = tsi->data_available();
    if (*in_valid && in_ready) {
        *in_bits = tsi->recv_word();
    }

    tsi->switch_to_host();

    return tsi->done() ? (tsi->exit_code() << 1 | 1) : 0;
}
