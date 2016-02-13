// See LICENSE for license details.

#ifndef _HTIF_EMULATOR_H
#define _HTIF_EMULATOR_H

#include <fesvr/htif_pthread.h>

class htif_emulator_t : public htif_pthread_t
{
 public:
  htif_emulator_t(uint32_t memsz_mb, const std::vector<std::string>& args)
    : htif_pthread_t(args)
  {
    this->_memsz_mb = memsz_mb;
  }

  void set_clock_divisor(int divisor, int hold_cycles)
  {
    write_cr(-1, 63, divisor | hold_cycles << 16);
  }

  void start()
  {
    set_clock_divisor(5, 2);
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
