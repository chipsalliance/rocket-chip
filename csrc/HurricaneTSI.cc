#include "HurricaneTSI.h"

hurricane_tsi_t::hurricane_tsi_t(const std::vector<std::string>& args) : tsi_t(args)
{
}

hurricane_tsi_t::~hurricane_tsi_t()
{
}

// Bringup the coreplex for general mmio to work then standard htif
void hurricane_tsi_t::start()
{
#ifdef SCRTOP_COREPLEX_RESET
  printf("BRINGING UP COREPLEX\n");
  uint64_t zero = 0;
  write_chunk(SCRTOP_COREPLEX_RESET, sizeof(uint64_t), &zero);
#endif
  tsi_t::start();
}

void hurricane_tsi_t::reset()
{
  printf("RESETING Hurricane\n");

  // change reset vector
  uint64_t start = 0x80000000;
#ifdef SCRTOP_RESET_VECTOR
  write_chunk(SCRTOP_RESET_VECTOR, sizeof(uint64_t), &start);
#endif

  // reset core(s)
  uint64_t zero = 0;
#ifdef SCRTOP_PMU_RESET
  write_chunk(SCRTOP_PMU_RESET, sizeof(uint64_t), &zero);
#endif
#ifdef SCRTOP_CORE_0_RESET
  write_chunk(SCRTOP_CORE_0_RESET, sizeof(uint64_t), &zero);
  write_chunk(SCRTOP_CORE_0_ROCC_RESET, sizeof(uint64_t), &zero);
#endif
  tsi_t::reset();
}
