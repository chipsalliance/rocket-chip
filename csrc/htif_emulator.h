// See LICENSE for license details.

#ifndef _HTIF_EMULATOR_H
#define _HTIF_EMULATOR_H

#include <fesvr/htif_pthread.h>

class htif_emulator_t : public htif_pthread_t
{
 int memory_channel_mux_select;

 public:
  htif_emulator_t(uint32_t memsz_mb, const std::vector<std::string>& args)
    : htif_pthread_t(args),
      memory_channel_mux_select(0)
  {
    this->_memsz_mb = memsz_mb;

    for (const auto& arg: args) {
      if (!strncmp(arg.c_str(), "+memory_channel_mux_select=", 27))
        memory_channel_mux_select = atoi(arg.c_str()+27);
    }
 }

  void set_clock_divisor(int divisor, int hold_cycles)
  {
#ifdef UNCORE_SCR__HTIF_IO_CLOCK_DIVISOR__OFFSET
    /* We only want to write the HTIF clock divisor SCR on targets where it
     * actually exists (there isn't one on the FPGA, for example). */
    write_cr(-1, UNCORE_SCR__HTIF_IO_CLOCK_DIVISOR__OFFSET, divisor | hold_cycles << 16);
#endif
  }

  void start()
  {
    set_clock_divisor(5, 2);
    write_cr(-1, UNCORE_SCR__MEMORY_CHANNEL_MUX_SELECT__OFFSET, memory_channel_mux_select);
    htif_pthread_t::start();
  }

  uint32_t mem_mb() {
    uint32_t scr_mb = htif_pthread_t::mem_mb();
    return (_memsz_mb < scr_mb) ? _memsz_mb : scr_mb;
  }

 private:
  uint32_t _memsz_mb;
};

#endif
