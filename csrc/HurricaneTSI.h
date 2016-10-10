#ifndef __HURRICANE_TSI_H
#define __HURRICANE_TSI_H

#include <fesvr/tsi.h>

class hurricane_tsi_t : public tsi_t {
 public:
  hurricane_tsi_t(const std::vector<std::string>& args);
  ~hurricane_tsi_t();

 protected:
  virtual void reset() override;
};

#endif
