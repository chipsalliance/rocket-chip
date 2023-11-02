#pragma once

#include "axi4_slave.hpp"
#include <fstream>
#include <iostream>
#include <linux/elf.h>

template <unsigned int A_WIDTH = 64, unsigned int D_WIDTH = 64,
          unsigned int ID_WIDTH = 4>
class axi4_mem : public axi4_slave<A_WIDTH, D_WIDTH, ID_WIDTH> {
public:
  axi4_mem(size_t size_bytes, bool allow_warp = false)
      : allow_warp(allow_warp) {
    if (size_bytes % (D_WIDTH / 8))
      size_bytes += 8 - (size_bytes % (D_WIDTH / 8));
    mem = new unsigned char[size_bytes];
    mem_size = size_bytes;
  }
  axi4_mem(size_t size_bytes, const uint8_t *init_binary,
           size_t init_binary_len, bool allow_warp = false)
      : axi4_mem(size_bytes, allow_warp) {
    assert(init_binary_len <= size_bytes);
    memcpy(mem, init_binary, init_binary_len);
  }
  axi4_mem(size_t size_bytes, const char *filename, bool allow_warp = false)
      : axi4_mem(size_bytes, allow_warp) {
    load_binary(filename);
  }
  ~axi4_mem() { delete[] mem; }
  bool read(off_t start_addr, size_t size, uint8_t *buffer) {
    return do_read(start_addr, size, buffer) == RESP_OKEY;
  }
  bool write(off_t start_addr, size_t size, const uint8_t *buffer) {
    return do_write(start_addr, size, buffer) == RESP_OKEY;
  }
  void load_binary(const char *init_file, uint64_t start_addr = 0) {
    std::ifstream fs(init_file, std::ios::binary);
    fs.exceptions(std::ios::failbit);

    Elf32_Ehdr ehdr;
    fs.read(reinterpret_cast<char *>(&ehdr), sizeof(ehdr));
    if (!(ehdr.e_machine == EM_RISCV && ehdr.e_type == ET_EXEC)) {
      std::cerr << "ehdr check failed when loading elf" << std::endl;
    }

    for (size_t i = 0; i < ehdr.e_phnum; i++) {
      auto phdr_offset = ehdr.e_phoff + i * ehdr.e_phentsize;
      Elf32_Phdr phdr;
      fs.seekg((long)phdr_offset)
          .read(reinterpret_cast<char *>(&phdr), sizeof(phdr));
      if (phdr.p_type == PT_LOAD) {
        if (phdr.p_paddr + phdr.p_filesz >= mem_size) {
          std::cerr << "phdr p_paddr + p_filesz check failed" << std::endl;
        }
        fs.seekg((long)phdr.p_offset)
            .read(reinterpret_cast<char *>(&mem[phdr.p_paddr]), phdr.p_filesz);
      }
    }
    std::cerr << "elf entry at " << ehdr.e_entry << std::endl;
    entry_addr = ehdr.e_entry;
  }
  uint32_t get_entry_addr() { return entry_addr; }

protected:
  axi_resp do_read(uint64_t start_addr, uint64_t size, uint8_t *buffer) {
    if (allow_warp)
      start_addr %= mem_size;
    if (start_addr + size <= mem_size) {
      memcpy(buffer, &mem[start_addr], size);
      return RESP_OKEY;
    } else
      return RESP_DECERR;
  }
  axi_resp do_write(uint64_t start_addr, uint64_t size, const uint8_t *buffer) {
    if (allow_warp)
      start_addr %= mem_size;
    if (start_addr + size <= mem_size) {
      memcpy(&mem[start_addr], buffer, size);
      return RESP_OKEY;
    } else
      return RESP_DECERR;
  }

private:
  uint8_t *mem;
  size_t mem_size;
  uint32_t entry_addr;
  bool allow_warp = false;
};
