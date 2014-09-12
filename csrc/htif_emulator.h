// See LICENSE for license details.

#ifndef _HTIF_EMULATOR_H
#define _HTIF_EMULATOR_H

#include <fesvr/htif_pthread.h>

class htif_emulator_t : public htif_pthread_t
{
 public:
  htif_emulator_t(const std::vector<std::string>& args)
    : htif_pthread_t(args)
  {
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
};

#endif
