// See LICENSE for license details.

#include "mm.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <cassert>

void mm_t::init(size_t sz, int wsz, int lsz)
{
  assert(wsz > 0 && lsz > 0 && (lsz & (lsz-1)) == 0 && lsz % wsz == 0);
  word_size = wsz;
  line_size = lsz;
  data = new char[sz];
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

void mm_magic_t::tick
(
  bool req_cmd_val,
  bool req_cmd_store,
  uint64_t req_cmd_addr,
  uint64_t req_cmd_tag,
  bool req_data_val,
  void* req_data_bits,
  bool resp_rdy
)
{
  bool req_cmd_fire = req_cmd_val && req_cmd_ready();
  bool req_data_fire = req_data_val && req_data_ready();
  bool resp_fire = resp_valid() && resp_rdy;
  assert(!(req_cmd_fire && req_data_fire));

  if (resp_fire)
    resp.pop();

  if (req_data_fire)
  {
    memcpy(data + store_addr + store_count*word_size, req_data_bits, word_size);

    store_count = (store_count + 1) % (line_size/word_size);
    if (store_count == 0)
      store_inflight = false;
  }

  if (req_cmd_fire)
  {
    auto byte_addr = req_cmd_addr * line_size;
    assert(byte_addr < size);

    if (req_cmd_store)
    {
      store_inflight = true;
      store_addr = byte_addr;
    }
    else for (int i = 0; i < line_size/word_size; i++)
    {
      auto base = data + byte_addr + i*word_size;
      auto dat = std::vector<char>(base, base + word_size);
      resp.push(std::make_pair(req_cmd_tag, dat));
    }
  }

  cycle++;
}

void load_mem(void* mem, const char* fn)
{
  char* m = (char*)mem;
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
    for (ssize_t i = line.length()-2, j = 0; i >= 0; i -= 2, j++)
      m[j] = (parse_nibble(line[i]) << 4) | parse_nibble(line[i+1]);
    m += line.length()/2;
  }
}
