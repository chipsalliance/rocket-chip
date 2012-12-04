#ifndef MM_EMULATOR_H
#define MM_EMULATOR_H

#include "mm_param.h"
#include <stdint.h>
#include <queue>

class mm_t
{
 public:
  mm_t() : data(0), size(0) {}

  virtual void init(size_t sz);

  virtual bool req_cmd_ready() = 0;
  virtual bool req_data_ready() = 0;
  virtual bool resp_valid() = 0;
  virtual uint64_t resp_tag() = 0;
  virtual void* resp_data() = 0;

  virtual void tick
  (
    bool req_cmd_val,
    bool req_cmd_store,
    uint64_t req_cmd_addr,
    uint64_t req_cmd_tag,
    bool req_data_val,
    void* req_data_bits
  ) = 0;

  virtual void* get_data() { return data; }
  virtual size_t get_size() { return size; }

  virtual ~mm_t();

 protected:
  char* data;
  size_t size;
};

class mm_magic_t : public mm_t
{
 public:
  mm_magic_t() : store_inflight(false), store_count(0) {}

  virtual void init(size_t sz);

  virtual bool req_cmd_ready() { return !store_inflight; }
  virtual bool req_data_ready() { return store_inflight; }
  virtual bool resp_valid() { return !resp.empty(); }
  virtual uint64_t resp_tag() { return resp_valid() ? resp.front().first : 0; }
  virtual void* resp_data() { return resp_valid() ? &resp.front().second[0] : &dummy_data[0]; }

  virtual void tick
  (
    bool req_cmd_val,
    bool req_cmd_store,
    uint64_t req_cmd_addr,
    uint64_t req_cmd_tag,
    bool req_data_val,
    void* req_data_bits
  );

 protected:
  bool store_inflight;
  int store_count;
  uint64_t store_addr;
  std::vector<char> dummy_data;

  uint64_t cycle;
  std::queue<std::pair<uint64_t, std::vector<char>>> resp;
};

void load_mem(void* mem, const char* fn);
#endif
