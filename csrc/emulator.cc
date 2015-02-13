// See LICENSE for license details.

#include "htif_emulator.h"
#include "emulator.h"
#include "mm.h"
#include "mm_dramsim2.h"
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

htif_emulator_t* htif;
void handle_sigterm(int sig)
{
  htif->stop();
}

int main(int argc, char** argv)
{
  unsigned random_seed = (unsigned)time(NULL) ^ (unsigned)getpid();
  uint64_t max_cycles = -1;
  uint64_t trace_count = 0;
  int ret = 0;
  const char* vcd = NULL;
  const char* loadmem = NULL;
  FILE *vcdfile = NULL;
  bool dramsim2 = false;
  bool log = false;
  uint64_t memsz_mb = MEM_SIZE / (1024*1024);

  for (int i = 1; i < argc; i++)
  {
    std::string arg = argv[i];
    if (arg.substr(0, 2) == "-v")
      vcd = argv[i]+2;
    else if (arg.substr(0, 9) == "+memsize=")
      memsz_mb = atoll(argv[i]+9);
    else if (arg.substr(0, 2) == "-s")
      random_seed = atoi(argv[i]+2);
    else if (arg == "+dramsim")
      dramsim2 = true;
    else if (arg == "+verbose")
      log = true;
    else if (arg.substr(0, 12) == "+max-cycles=")
      max_cycles = atoll(argv[i]+12);
    else if (arg.substr(0, 9) == "+loadmem=")
      loadmem = argv[i]+9;
  }

  const int disasm_len = 24;
  if (vcd)
  {
    // Create a VCD file
    vcdfile = strcmp(vcd, "-") == 0 ? stdout : fopen(vcd, "w");
    assert(vcdfile);
    fprintf(vcdfile, "$scope module Testbench $end\n");
    fprintf(vcdfile, "$var reg %d NDISASM_WB wb_instruction $end\n", disasm_len*8);
    fprintf(vcdfile, "$var reg 64 NCYCLE cycle $end\n");
    fprintf(vcdfile, "$upscope $end\n");
  }


  // The chisel generated code
  Top_t tile;
  srand(random_seed);
  tile.init(random_seed);

  // Instantiate and initialize main memory
  mm_t* mm = dramsim2 ? (mm_t*)(new mm_dramsim2_t) : (mm_t*)(new mm_magic_t);
  try {
  	 mm->init(memsz_mb*1024*1024, tile.Top__io_mem_resp_bits_data.width()/8, LINE_SIZE);
  }
  catch (const std::bad_alloc& e) {
    fprintf(stderr,
  	   "Failed to allocate %ld bytes (%ld MiB) of memory\n"
  		"Set smaller amount of memory using +memsize=<N> (in MiB)\n" , memsz_mb*1024*1024, memsz_mb
  	 );
    exit(-1);
  }
  if (loadmem)
    load_mem(mm->get_data(), loadmem);

  // Instantiate HTIF
  htif = new htif_emulator_t(std::vector<std::string>(argv + 1, argv + argc));
  int htif_bits = tile.Top__io_host_in_bits.width();
  assert(htif_bits % 8 == 0 && htif_bits <= val_n_bits());

  signal(SIGTERM, handle_sigterm);

  // reset for one host_clk cycle to handle pipelined reset
  tile.Top__io_host_in_valid = LIT<1>(0);
  tile.Top__io_host_out_ready = LIT<1>(0);
  tile.Top__io_mem_backup_ctrl_en = LIT<1>(0);
  for (int i = 0; i < 3; i += tile.Top__io_host_clk_edge.to_bool())
  {
    tile.clock_lo(LIT<1>(1));
    tile.clock_hi(LIT<1>(1));
  }

  while (!htif->done() && trace_count < max_cycles && ret == 0)
  {
    tile.Top__io_mem_req_cmd_ready = LIT<1>(mm->req_cmd_ready());
    tile.Top__io_mem_req_data_ready = LIT<1>(mm->req_data_ready());
    tile.Top__io_mem_resp_valid = LIT<1>(mm->resp_valid());
    tile.Top__io_mem_resp_bits_tag = LIT<64>(mm->resp_tag());
    memcpy(tile.Top__io_mem_resp_bits_data.values, mm->resp_data(), tile.Top__io_mem_resp_bits_data.width()/8);

    try {
      tile.clock_lo(LIT<1>(0));
    } catch (std::runtime_error& e) {
      max_cycles = trace_count; // terminate cleanly after this cycle
      ret = 1;
      std::cerr << e.what() << std::endl;
    }

    mm->tick(
      tile.Top__io_mem_req_cmd_valid.lo_word(),
      tile.Top__io_mem_req_cmd_bits_rw.lo_word(),
      tile.Top__io_mem_req_cmd_bits_addr.lo_word(),
      tile.Top__io_mem_req_cmd_bits_tag.lo_word(),

      tile.Top__io_mem_req_data_valid.lo_word(),
      tile.Top__io_mem_req_data_bits_data.values,

      tile.Top__io_mem_resp_ready.to_bool()
    );

    if (tile.Top__io_host_clk_edge.to_bool())
    {
      static bool htif_in_valid = false;
      static val_t htif_in_bits;
      if (tile.Top__io_host_in_ready.to_bool() || !htif_in_valid)
        htif_in_valid = htif->recv_nonblocking(&htif_in_bits, htif_bits/8);
      tile.Top__io_host_in_valid = LIT<1>(htif_in_valid);
      tile.Top__io_host_in_bits = LIT<64>(htif_in_bits);

      if (tile.Top__io_host_out_valid.to_bool())
        htif->send(tile.Top__io_host_out_bits.values, htif_bits/8);
      tile.Top__io_host_out_ready = LIT<1>(1);
    }

    if (log)
      tile.print(stderr);

    if (vcd)
      tile.dump(vcdfile, trace_count);

    tile.clock_hi(LIT<1>(0));
    trace_count++;
  }

  if (vcd)
    fclose(vcdfile);

  if (htif->exit_code())
  {
    fprintf(stderr, "*** FAILED *** (code = %d, seed %d) after %lld cycles\n", htif->exit_code(), random_seed, (long long)trace_count);
    ret = htif->exit_code();
  }
  else if (trace_count == max_cycles)
  {
    fprintf(stderr, "*** FAILED *** (timeout, seed %d) after %lld cycles\n", random_seed, (long long)trace_count);
    ret = 2;
  }

  delete htif;

  return ret;
}
