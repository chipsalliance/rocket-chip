#include "htif_phy.h"
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <map>
#include "common.h"
#include "emulator.h"
#include "mm_emulator.cc"
#include "Top.h" // chisel-generated code...
#include "disasm.h"

int main(int argc, char** argv)
{
  int fromhost_fd = -1, tohost_fd = -1;
  unsigned random_seed = (unsigned)time(NULL) ^ (unsigned)getpid();
  uint64_t max_cycles = 0;
  uint64_t trace_count = 0;
  int start = 0;
  bool log = false;
  bool quiet = false;
  const char* vcd = NULL;
  const char* loadmem = NULL;
  FILE *vcdfile = NULL, *logfile = stderr;
  const char* failure = NULL;

  // for disassembly
  disassembler disasm;
  char if_inst_str[1024];
  char id_inst_str[1024];
  char ex_inst_str[1024];
  char mem_inst_str[1024];
  char wb_inst_str[1024];

  // used to register values from EX stage for trace
  uint64_t mem_reg_raddr1 = 0, mem_reg_raddr2 = 0;
  uint64_t wb_reg_raddr1 = 0, wb_reg_raddr2 = 0;
  uint64_t mem_reg_rs1 = 0, mem_reg_rs2 = 0, mem_reg_inst = 0, ex_reg_inst = 0;
  uint64_t wb_reg_rs1 = 0, wb_reg_rs2 = 0, wb_reg_inst = 0;
  uint64_t id_icache_miss = 0, if_icache_req = 0, id_itlb_miss = 0;

  for (int i = 1; i < argc; i++)
  {
    std::string arg = argv[i];
    if (arg == "-l")
      log = true;
    else if (arg == "-q")
      quiet = true;
    else if (arg.substr(0, 2) == "-v")
      vcd = argv[i]+2;
    else if (arg.substr(0, 2) == "-m")
      max_cycles = atoll(argv[i]+2);
    else if (arg.substr(0, 2) == "-s")
      random_seed = atoi(argv[i]+2);
    else if (arg.substr(0, 10) == "+fromhost=")
      fromhost_fd = atoi(argv[i]+10);
    else if (arg.substr(0, 8) == "+tohost=")
      tohost_fd = atoi(argv[i]+8);
    else if (arg.substr(0, 9) == "+loadmem=")
      loadmem = argv[i]+9;
    else
    {
      fprintf(stderr, "unknown option: %s\n", argv[i]);
      exit(1);
    }
  }

  demand(fcntl(fromhost_fd,F_GETFD) >= 0, "fromhost file not open");
  demand(fcntl(tohost_fd,F_GETFD) >= 0, "tohost file not open");

  if (vcd)
  {
    // Create a VCD file
    vcdfile = strcmp(vcd, "-") == 0 ? stdout : fopen(vcd, "w");
    assert(vcdfile);
    fprintf(vcdfile, "$scope module Testbench $end\n");
    fprintf(vcdfile, "$var reg 256 NDISASM_IF if_instruction $end\n");
    fprintf(vcdfile, "$var reg 256 NDISASM_ID id_instruction $end\n");
    fprintf(vcdfile, "$var reg 256 NDISASM_EX ex_instruction $end\n");
    fprintf(vcdfile, "$var reg 256 NDISASM_MEM mem_instruction $end\n");
    fprintf(vcdfile, "$var reg 16 NCYCLE cycle $end\n");
    fprintf(vcdfile, "$upscope $end\n");
  }

  // basic fixed latency memory model
  uint64_t* mem = mm_init();
  if (loadmem != NULL)
    load_mem(mem, loadmem);

  // The chisel generated code
  Top_t tile;
  srand(random_seed);
  tile.init(random_seed != 0);

  // reset for a few cycles to support pipelined reset
  tile.Top__io_host_in_valid = LIT<1>(0);
  tile.Top__io_host_out_ready = LIT<1>(0);
  for (int i = 0; i < 10; i++)
  {
    tile.clock_lo(LIT<1>(1));
    tile.clock_hi(LIT<1>(1));
  }

  htif_phy_t htif_phy(tile.Top__io_host_in_bits.width(), fromhost_fd, tohost_fd);

  while (max_cycles == 0 || trace_count < max_cycles)
  {
    // memory model
    mm_tick_emulator (
      tile.Top__io_mem_req_cmd_valid.lo_word(),
      &tile.Top__io_mem_req_cmd_ready.values[0],
      tile.Top__io_mem_req_cmd_bits_rw.lo_word(),
      tile.Top__io_mem_req_cmd_bits_addr.lo_word(),
      tile.Top__io_mem_req_cmd_bits_tag.lo_word(),

      tile.Top__io_mem_req_data_valid.lo_word(),
      &tile.Top__io_mem_req_data_ready.values[0],
      &tile.Top__io_mem_req_data_bits_data.values[0],

      &tile.Top__io_mem_resp_valid.values[0],
      &tile.Top__io_mem_resp_bits_tag.values[0],
      &tile.Top__io_mem_resp_bits_data.values[0]
    );

    tile.Top__io_host_in_valid = LIT<1>(htif_phy.in_valid());
    tile.Top__io_host_in_bits = LIT<64>(htif_phy.in_bits());
    tile.Top__io_host_out_ready = LIT<1>(htif_phy.out_ready());

    tile.clock_lo(LIT<1>(0));

    htif_phy.tick(tile.Top__io_host_in_ready.lo_word(),
                  tile.Top__io_host_out_valid.lo_word(),
                  tile.Top__io_host_out_bits.lo_word());

  
    if (tile.Top__io_debug_error_mode.lo_word())
    {
      failure = "entered error mode";
      break;
    }

    if (log || (quiet && trace_count % 10000 == 0))
    {
      insn_t insn;
      insn.bits = wb_reg_inst;
      strcpy(wb_inst_str, disasm.disassemble(insn).c_str());

      fprintf(logfile, "C: %10lld [%ld] pc=[%011lx] W[r%2ld=%016lx][%ld] R[r%2ld=%016lx] R[r%2ld=%016lx] inst=[%08lx] %-32s\n", \
              (long long)trace_count, tile.Top_Tile_cpu_ctrl__wb_reg_valid.lo_word(), tile.Top_Tile_cpu_dpath__wb_reg_pc.lo_word(), \
              tile.Top_Tile_cpu_dpath_rfile__io_w0_addr.lo_word(), tile.Top_Tile_cpu_dpath_rfile__io_w0_data.lo_word(), tile.Top_Tile_cpu_dpath_rfile__io_w0_en.lo_word(),
              wb_reg_raddr1, wb_reg_rs1, wb_reg_raddr2, wb_reg_rs2, wb_reg_inst, wb_inst_str);
    }

    if (vcd)
    {
      tile.dump(vcdfile, trace_count); // dump all signals to vcd
      
      #define dump_disasm(inst, name) do { \
        insn_t insn; \
        insn.bits = inst; \
        std::string dasm = disasm.disassemble(insn); \
        int namelen = strlen(name), pos = 0; \
        char str[1 + dasm.length()*8 + 1 + namelen + 1 + 1]; \
        str[pos++] = 'b'; \
        for (size_t i = 0; i < dasm.length()*8; i++) \
          str[pos++] = ((dasm[i/8] >> (7-(i%8))) & 1) + '0'; \
        str[pos++] = ' '; \
        memcpy(str + pos, name, namelen); pos += namelen; \
        str[pos++] = '\n'; \
        str[pos] = 0; \
        fputs(str, vcdfile); \
      } while(0)
      dump_disasm(tile.Top_Tile_cpu_dpath__id_reg_inst_shadow.lo_word(), "NDISASM_IF");
      dump_disasm(tile.Top_Tile_cpu_dpath__id_reg_inst.lo_word(), "NDISASM_ID");
      dump_disasm(ex_reg_inst, "NDISASM_EX");
      dump_disasm(mem_reg_inst, "NDISASM_MEM");

      dat_dump(vcdfile, dat_t<64>(trace_count), "NCYCLE\n");
    }

    // delay values from ex stage for trace output on the following cycle
    wb_reg_inst = mem_reg_inst;
    wb_reg_raddr1 = mem_reg_raddr1;
    wb_reg_raddr2 = mem_reg_raddr2;
    wb_reg_rs1 = mem_reg_rs1;
    wb_reg_rs2 = mem_reg_rs2;

    mem_reg_inst = ex_reg_inst;
    mem_reg_raddr1 = (mem_reg_inst >> 22) & 0x1f;
    mem_reg_raddr2 = (mem_reg_inst >> 17) & 0x1f;
    mem_reg_rs1 = tile.Top_Tile_cpu_dpath__ex_reg_rs1.lo_word();
    mem_reg_rs2 = tile.Top_Tile_cpu_dpath__ex_reg_rs2.lo_word();

    ex_reg_inst = tile.Top_Tile_cpu_dpath__id_reg_inst.lo_word();

    tile.clock_hi(LIT<1>(0));
    trace_count++;

    if (trace_count == max_cycles)
    {
      failure = "timeout";
      break;
    }
  }

  if (vcd)
    fclose(vcdfile);

  if (failure)
  {
    fprintf(logfile, "*** FAILED *** (%s) after %lld cycles\n", failure, (long long)trace_count);
    return -1;
  }

  close(tohost_fd);
  close(fromhost_fd);

  return 0;
}
