// See LICENSE for license details.

#ifndef VERILATOR
#include "emulator.h"
#else
#include "verilated.h"
#if VM_TRACE
#include "verilated_vcd_c.h"
#endif
#endif
#include "mm.h"
#include "mm_dramsim2.h"
#include <fesvr/dtm.h>
#include <iostream>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define MEM_SIZE_BITS 3
#define MEM_LEN_BITS 8
#define MEM_RESP_BITS 2

#include "emulator_type.h"

static dtm_t* dtm;
bool verbose;

void handle_sigterm(int sig)
{
  dtm->stop();
}

int main(int argc, char** argv)
{
  unsigned random_seed = (unsigned)time(NULL) ^ (unsigned)getpid();
  uint64_t max_cycles = -1;
  uint64_t trace_count = 0;
  uint64_t start = 0;
  int ret = 0;
  const char* vcd = NULL;
  const char* loadmem = NULL;
  FILE *vcdfile = NULL;
  bool dramsim2 = false;
  bool print_cycles = false;
  uint64_t memsz_mb = MEM_SIZE / (1024*1024);
  mm_t *mm[N_MEM_CHANNELS];

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
      verbose = true;
    else if (arg.substr(0, 12) == "+max-cycles=")
      max_cycles = atoll(argv[i]+12);
    else if (arg.substr(0, 9) == "+loadmem=")
      loadmem = argv[i]+9;
    else if (arg.substr(0, 7) == "+start=")
      start = atoll(argv[i]+7);
    else if (arg.substr(0, 12) == "+cycle-count")
      print_cycles = true;
  }

  const int disasm_len = 24;

#ifndef VERILATOR
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
  tile.init(random_seed);
#else
  VTop tile;
#if VM_TRACE
  VerilatedVcdC *tfp = NULL;
  if (vcd) {
    tfp = new VerilatedVcdC;
    Verilated::traceEverOn(true); // Verilator must compute traced signals
    VL_PRINTF("Enabling waves... (%s)\n", vcd);
    tile.trace(tfp, 99);  // Trace 99 levels of hierarchy
    tfp->open(vcd); // Open the dump file
  }
#endif
#endif
  srand(random_seed);


  uint64_t mem_width = MEM_DATA_BITS / 8;

  // Instantiate and initialize main memory
  for (int i = 0; i < N_MEM_CHANNELS; i++) {
    mm[i] = dramsim2 ? (mm_t*)(new mm_dramsim2_t) : (mm_t*)(new mm_magic_t);
    try {
      mm[i]->init(memsz_mb*1024*1024 / N_MEM_CHANNELS, mem_width, CACHE_BLOCK_BYTES);
    } catch (const std::bad_alloc& e) {
      fprintf(stderr,
          "Failed to allocate %ld bytes (%ld MiB) of memory\n"
          "Set smaller amount of memory using +memsize=<N> (in MiB)\n",
              memsz_mb*1024*1024, memsz_mb);
      exit(-1);
    }
  }

  if (loadmem) {
    void *mems[N_MEM_CHANNELS];
    for (int i = 0; i < N_MEM_CHANNELS; i++)
      mems[i] = mm[i]->get_data();
    load_mem(mems, loadmem, CACHE_BLOCK_BYTES, N_MEM_CHANNELS);
  }

  dtm = new dtm_t(std::vector<std::string>(argv + 1, argv + argc));

  signal(SIGTERM, handle_sigterm);

  // reset for several cycles to handle pipelined reset
  for (int i = 0; i < 10; i++) {
#ifndef VERILATOR
    tile.clock_lo(LIT<1>(1));
    tile.clock_hi(LIT<1>(1));
#else
    tile.reset = 1;
    tile.clk = 0;
    tile.eval();
    tile.clk = 1;
    tile.eval();
    tile.reset = 0;
#endif
  }

  bool_t *mem_ar_valid[N_MEM_CHANNELS];
  bool_t *mem_ar_ready[N_MEM_CHANNELS];
  mem_addr_t *mem_ar_bits_addr[N_MEM_CHANNELS];
  mem_id_t *mem_ar_bits_id[N_MEM_CHANNELS];
  mem_size_t *mem_ar_bits_size[N_MEM_CHANNELS];
  mem_len_t *mem_ar_bits_len[N_MEM_CHANNELS];

  bool_t *mem_aw_valid[N_MEM_CHANNELS];
  bool_t *mem_aw_ready[N_MEM_CHANNELS];
  mem_addr_t *mem_aw_bits_addr[N_MEM_CHANNELS];
  mem_id_t *mem_aw_bits_id[N_MEM_CHANNELS];
  mem_size_t *mem_aw_bits_size[N_MEM_CHANNELS];
  mem_len_t *mem_aw_bits_len[N_MEM_CHANNELS];

  bool_t *mem_w_valid[N_MEM_CHANNELS];
  bool_t *mem_w_ready[N_MEM_CHANNELS];
  mem_data_t *mem_w_bits_data[N_MEM_CHANNELS];
  mem_strb_t *mem_w_bits_strb[N_MEM_CHANNELS];
  bool_t *mem_w_bits_last[N_MEM_CHANNELS];

  bool_t *mem_b_valid[N_MEM_CHANNELS];
  bool_t *mem_b_ready[N_MEM_CHANNELS];
  mem_resp_t *mem_b_bits_resp[N_MEM_CHANNELS];
  mem_id_t *mem_b_bits_id[N_MEM_CHANNELS];

  bool_t *mem_r_valid[N_MEM_CHANNELS];
  bool_t *mem_r_ready[N_MEM_CHANNELS];
  mem_resp_t *mem_r_bits_resp[N_MEM_CHANNELS];
  mem_id_t *mem_r_bits_id[N_MEM_CHANNELS];
  mem_data_t *mem_r_bits_data[N_MEM_CHANNELS];
  bool_t *mem_r_bits_last[N_MEM_CHANNELS];

#include TBFRAG

  while (!dtm->done() && (trace_count >> 1) < max_cycles && ret == 0)
  {
    for (int i = 0; i < N_MEM_CHANNELS; i++) {
      value(mem_ar_ready[i]) = mm[i]->ar_ready();
      value(mem_aw_ready[i]) = mm[i]->aw_ready();
      value(mem_w_ready[i]) = mm[i]->w_ready();

      value(mem_b_valid[i]) = mm[i]->b_valid();
      value(mem_b_bits_resp[i]) = mm[i]->b_resp();
      value(mem_b_bits_id[i]) = mm[i]->b_id();

      value(mem_r_valid[i]) = mm[i]->r_valid();
      value(mem_r_bits_resp[i]) = mm[i]->r_resp();
      value(mem_r_bits_id[i]) = mm[i]->r_id();
      value(mem_r_bits_last[i]) = mm[i]->r_last();

      memcpy(values(mem_r_bits_data[i]), mm[i]->r_data(), mem_width);
    }

    value(field(io_debug_resp_ready)) = dtm->resp_ready();
    value(field(io_debug_req_valid)) = dtm->req_valid();
    value(field(io_debug_req_bits_addr)) = dtm->req_bits().addr;
    value(field(io_debug_req_bits_op)) = dtm->req_bits().op;
    value(field(io_debug_req_bits_data)) = dtm->req_bits().data;

    try {
#ifndef VERILATOR
      tile.clock_lo(LIT<1>(0));
#else
      tile.clk = 0;
      tile.eval();
      // make sure we dump on cycle 0 to get dump_init
#if VM_TRACE
      if (tfp && ((trace_count >> 1) == 0 || (trace_count >> 1) >= start))
        tfp->dump(trace_count);
#endif
#endif
      trace_count++;
    } catch (std::runtime_error& e) {
      max_cycles = trace_count >> 1; // terminate cleanly after this cycle
      ret = 1;
      std::cerr << e.what() << std::endl;
    }

    dtm_t::resp debug_resp_bits;
    debug_resp_bits.resp = value(field(io_debug_resp_bits_resp));
    debug_resp_bits.data = value(field(io_debug_resp_bits_data));

    dtm->tick(
      value(field(io_debug_req_ready)),
      value(field(io_debug_resp_valid)),
      debug_resp_bits
    );

    for (int i = 0; i < N_MEM_CHANNELS; i++) {
      mm[i]->tick(
        value(mem_ar_valid[i]),
        value(mem_ar_bits_addr[i]) - MEM_BASE,
        value(mem_ar_bits_id[i]),
        value(mem_ar_bits_size[i]),
        value(mem_ar_bits_len[i]),

        value(mem_aw_valid[i]),
        value(mem_aw_bits_addr[i]) - MEM_BASE,
        value(mem_aw_bits_id[i]),
        value(mem_aw_bits_size[i]),
        value(mem_aw_bits_len[i]),

        value(mem_w_valid[i]),
        value(mem_w_bits_strb[i]),
        values(mem_w_bits_data[i]),
        value(mem_w_bits_last[i]),

        value(mem_r_ready[i]),
        value(mem_b_ready[i])
      );
    }

#ifndef VERILATOR
    if (verbose && (trace_count >> 1) >= start)
      tile.print(stderr);

    // make sure we dump on cycle 0 to get dump_init
    if (vcd && ((trace_count >> 1) == 0 || (trace_count >> 1) >= start))
      tile.dump(vcdfile, trace_count >> 1);

    tile.clock_hi(LIT<1>(0));
#else
    tile.clk = 1;
    tile.eval();
#if VM_TRACE
    if (tfp && ((trace_count >> 1) == 0 || (trace_count >> 1) >= start))
      tfp->dump(trace_count);
#endif
#endif
    trace_count++;
  }

#ifndef VERILATOR
  if (vcd) fclose(vcdfile);
#else
#if VM_TRACE
  if (tfp) tfp->close();
  delete tfp;
#endif
#endif

  if (dtm->exit_code())
  {
    fprintf(stderr, "*** FAILED *** (code = %d, seed %d) after %ld cycles\n", dtm->exit_code(), random_seed, trace_count >> 1);
    ret = dtm->exit_code();
  }
  else if ((trace_count >> 1) == max_cycles)
  {
    fprintf(stderr, "*** FAILED *** (timeout, seed %d) after %ld cycles\n", random_seed, trace_count >> 1);
    ret = 2;
  }
  else if (verbose || print_cycles)
  {
    fprintf(stderr, "Completed after %ld cycles\n", trace_count >> 1);
  }

  delete dtm;

  return ret;
}
