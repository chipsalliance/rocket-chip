// See LICENSE for license details.

#ifndef MM_EMULATOR_H
#define MM_EMULATOR_H

#include <stdint.h>
#include <cstring>
#include <queue>

void write_masked_data(
    uint8_t *base, uint8_t *data, uint64_t strb, uint64_t size);

class mm_t
{
 public:
  mm_t() : data(0), size(0) {}

  virtual void init(size_t sz, int word_size, int line_size);

  virtual bool ar_ready() = 0;
  virtual bool aw_ready() = 0;
  virtual bool w_ready() = 0;
  virtual bool b_valid() = 0;
  virtual uint64_t b_resp() = 0;
  virtual uint64_t b_id() = 0;
  virtual bool r_valid() = 0;
  virtual uint64_t r_resp() = 0;
  virtual uint64_t r_id() = 0;
  virtual void *r_data() = 0;
  virtual bool r_last() = 0;

  virtual void tick
  (
    bool ar_valid,
    uint64_t ar_addr,
    uint64_t ar_id,
    uint64_t ar_size,
    uint64_t ar_len,

    bool aw_valid,
    uint64_t aw_addr,
    uint64_t aw_id,
    uint64_t aw_size,
    uint64_t aw_len,

    bool w_valid,
    uint64_t w_strb,
    void *w_data,
    bool w_last,

    bool r_ready,
    bool b_ready
  ) = 0;

  virtual void* get_data() { return data; }
  virtual size_t get_size() { return size; }
  virtual size_t get_word_size() { return word_size; }
  virtual size_t get_line_size() { return line_size; }

  void write(uint64_t addr, uint8_t *data, uint64_t strb, uint64_t size);
  std::vector<char> read(uint64_t addr, uint64_t size);

  virtual ~mm_t();

 protected:
  uint8_t* data;
  size_t size;
  int word_size;
  int line_size;
};

struct mm_rresp_t
{
  uint64_t id;
  std::vector<char> data;
  bool last;

  mm_rresp_t(uint64_t id, std::vector<char> data, bool last)
  {
    this->id = id;
    this->data = data;
    this->last = last;
  }

  mm_rresp_t()
  {
    this->id = 0;
    this->last = false;
  }
};

class mm_magic_t : public mm_t
{
 public:
  mm_magic_t() : store_inflight(false) {}

  virtual void init(size_t sz, int word_size, int line_size);

  virtual bool ar_ready() { return true; }
  virtual bool aw_ready() { return !store_inflight; }
  virtual bool w_ready() { return store_inflight; }
  virtual bool b_valid() { return !bresp.empty(); }
  virtual uint64_t b_resp() { return 0; }
  virtual uint64_t b_id() { return b_valid() ? bresp.front() : 0; }
  virtual bool r_valid() { return !rresp.empty(); }
  virtual uint64_t r_resp() { return 0; }
  virtual uint64_t r_id() { return r_valid() ? rresp.front().id: 0; }
  virtual void *r_data() { return r_valid() ? &rresp.front().data[0] : &dummy_data[0]; }
  virtual bool r_last() { return r_valid() ? rresp.front().last : false; }

  virtual void tick
  (
    bool ar_valid,
    uint64_t ar_addr,
    uint64_t ar_id,
    uint64_t ar_size,
    uint64_t ar_len,

    bool aw_valid,
    uint64_t aw_addr,
    uint64_t aw_id,
    uint64_t aw_size,
    uint64_t aw_len,

    bool w_valid,
    uint64_t w_strb,
    void *w_data,
    bool w_last,

    bool r_ready,
    bool b_ready
  );

 protected:
  bool store_inflight;
  uint64_t store_addr;
  uint64_t store_id;
  uint64_t store_size;
  uint64_t store_count;
  std::vector<char> dummy_data;
  std::queue<uint64_t> bresp;

  std::queue<mm_rresp_t> rresp;

  uint64_t cycle;
};

void load_mem(void** mems, const char* fn, int line_size, int nchannels);
#endif
