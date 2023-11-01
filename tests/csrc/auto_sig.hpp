#pragma once

#include <condition_variable>
#include <verilated.h>

#define AUTO_TYPE(msb, lsb)                                                    \
  typename std::conditional<                                                   \
      ((msb) - (lsb) + 1) <= 8, CData,                                         \
      typename std::conditional<                                               \
          ((msb) - (lsb) + 1) <= 16, SData,                                    \
          typename std::conditional<((msb) - (lsb) + 1) <= 32, IData,          \
                                    QData>::type>::type>::type

#define AUTO_SIG(name, msb, lsb) AUTO_TYPE(msb, lsb) name

#define AUTO_IN(name, msb, lsb) AUTO_SIG(name, msb, lsb)
#define AUTO_OUT(name, msb, lsb) AUTO_SIG(name, msb, lsb)