#include "HurricaneTSI.h"

hurricane_tsi_t::hurricane_tsi_t(const std::vector<std::string>& args) : tsi_t(args)
{
}

hurricane_tsi_t::~hurricane_tsi_t()
{
}

void hurricane_tsi_t::reset()
{
  // prod reset signals here
  tsi_t::reset();
}
