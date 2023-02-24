#include <VV.h>

namespace TlOpcode {
  constexpr int
      AcquireBlock = 6,
      Get = 4,
      AccessAckData = 1,
      PutFullData = 0,
      PutPartialData = 1,
      AccessAck = 4;
}

// the following macro helps us to access tl interface by dynamic index
#define TL_INTERFACE(type, name)                      \
  [[nodiscard]] inline type &get_tl_##name(VV &top) { \
    return top.memory_0_##name;                       \
  }


TL_INTERFACE(CData, a_ready);
TL_INTERFACE(CData, a_valid);
TL_INTERFACE(CData, a_bits_opcode);
TL_INTERFACE(CData, a_bits_param);
TL_INTERFACE(CData, a_bits_size);
TL_INTERFACE(CData, a_bits_mask);
TL_INTERFACE(CData, a_bits_corrupt);
TL_INTERFACE(CData, a_bits_source);
TL_INTERFACE(IData, a_bits_address);
TL_INTERFACE(QData, a_bits_data);

TL_INTERFACE(CData, d_ready);
TL_INTERFACE(CData, d_valid);
TL_INTERFACE(CData, d_bits_opcode);
TL_INTERFACE(CData, d_bits_param);
TL_INTERFACE(CData, d_bits_size);
TL_INTERFACE(CData, d_bits_denied);
TL_INTERFACE(CData, d_bits_corrupt);
TL_INTERFACE(CData, d_bits_source);
TL_INTERFACE(CData, d_bits_sink);
TL_INTERFACE(QData, d_bits_data);

#undef TL_INTERFACE
