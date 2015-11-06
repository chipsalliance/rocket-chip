// See LICENSE for license details.

#include "mm.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <cassert>

void mm_t::write(uint64_t addr, uint8_t *data, uint64_t strb, uint64_t size)
{
  if (addr > this->size) {
    fprintf(stderr, "Invalid write address %lx\n", addr);
    exit(EXIT_FAILURE);
  }

  uint8_t *base = this->data + addr;
  for (int i = 0; i < size; i++) {
    if (strb & 1)
      base[i] = data[i];
    strb >>= 1;
  }
}

std::vector<char> mm_t::read(uint64_t addr, uint64_t size)
{
  if (addr > this->size) {
    fprintf(stderr, "Invalid read address %lx\n", addr);
    exit(EXIT_FAILURE);
  }

  uint8_t *base = this->data + addr;
  return std::vector<char>(base, base + size);
}

void mm_t::init(size_t sz, int wsz, int lsz)
{
  assert(wsz > 0 && lsz > 0 && (lsz & (lsz-1)) == 0 && lsz % wsz == 0);
  word_size = wsz;
  line_size = lsz;
  data = new uint8_t[sz];
  size = sz;
}

mm_t::~mm_t()
{
  delete [] data;
}

void mm_magic_t::init(size_t sz, int wsz, int lsz)
{
  mm_t::init(sz, wsz, lsz);
  dummy_data.resize(word_size);
}

void mm_magic_t::tick(
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
  bool b_ready)
{
  bool ar_fire = ar_valid && ar_ready();
  bool aw_fire = aw_valid && aw_ready();
  bool w_fire = w_valid && w_ready();
  bool r_fire = r_valid() && r_ready;
  bool b_fire = b_valid() && b_ready;

  if (ar_fire) {
    uint64_t word_size = (1 << ar_size);
    for (int i = 0; i <= ar_len; i++) {
      auto dat = read(ar_addr + i * word_size, word_size);
      rresp.push(mm_rresp_t(ar_id, dat, i == ar_len));
    }
  }

  if (aw_fire) {
    store_addr = aw_addr;
    store_size = (1 << aw_size);
    store_id = aw_id;
    store_inflight = true;
    store_count = aw_len + 1;
  }

  if (w_fire) {
    write(store_addr, (uint8_t *) w_data, w_strb, store_size);
    store_addr += store_size;
    store_count--;

    if (store_count == 0) {
      store_inflight = false;
      bresp.push(store_id);
      assert(w_last);
    }
  }

  if (b_fire)
    bresp.pop();

  if (r_fire)
    rresp.pop();

  cycle++;
}

void load_mem(void** mems, const char* fn, int line_size, int nchannels)
{
  char* m;
  ssize_t start = 0;
  std::ifstream in(fn);
  if (!in)
  {
    std::cerr << "could not open " << fn << std::endl;
    exit(-1);
  }

  std::string line;
  while (std::getline(in, line))
  {
    #define parse_nibble(c) ((c) >= 'a' ? (c)-'a'+10 : (c)-'0')
    for (ssize_t i = line.length()-2, j = 0; i >= 0; i -= 2, j++) {
      char data = (parse_nibble(line[i]) << 4) | parse_nibble(line[i+1]);
      ssize_t addr = start + j;
      int channel = (addr / line_size) % nchannels;
      m = (char *) mems[channel];
      addr = (addr / line_size / nchannels) * line_size + (addr % line_size);
      m[addr] = data;
    }
    start += line.length()/2;
  }
}
