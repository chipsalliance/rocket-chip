#include "htif_phy.h"
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <map>
#include "common.h"
#include "emulator.h"
//#include "mm_emulator.cc"
#include "mm_emulator_dramsim2.cc"
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
  disassembler disasm;

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

  // basic fixed latency memory model
  /*uint64_t* mem = mm_init();*/
  uint64_t* mm_mem = dramsim2_init();
  if (loadmem != NULL)
    load_mem(mm_mem, loadmem);


  // The chisel generated code
  Top_t tile;
  srand(random_seed);
  tile.init(random_seed != 0);

  // reset for a few cycles to support pipelined reset
  tile.Top__io_host_in_valid = LIT<1>(0);
  tile.Top__io_host_out_ready = LIT<1>(0);
  tile.Top__io_mem_backup_en = LIT<1>(0);
  for (int i = 0; i < 10; i++)
  {
    tile.clock_lo(LIT<1>(1));
    tile.clock_hi(LIT<1>(1));
  }

  htif_phy_t htif_phy(tile.Top__io_host_in_bits.width(), fromhost_fd, tohost_fd);

  while (max_cycles == 0 || trace_count < max_cycles)
  {
//    fprintf(stderr, "trace count: %ld\n", trace_count);
    // memory model
//    mm_tick_emulator(
    dramsim2_tick_emulator (
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
//    fprintf(stderr, "trace count: %ld (after dramsim2_tick_emulator)\n", trace_count);

    tile.Top__io_host_in_valid = LIT<1>(htif_phy.in_valid());
    tile.Top__io_host_in_bits = LIT<64>(htif_phy.in_bits());
    tile.Top__io_host_out_ready = LIT<1>(htif_phy.out_ready());

    tile.clock_lo(LIT<1>(0));

    if (tile.Top__io_host_clk_edge.to_bool())
    {
      htif_phy.tick(tile.Top__io_host_in_ready.lo_word(),
                    tile.Top__io_host_out_valid.lo_word(),
                    tile.Top__io_host_out_bits.lo_word());
    }

  
    if (tile.Top__io_debug_error_mode.lo_word())
    {
      failure = "entered error mode";
      break;
    }

    if (log || vcd)
    {
      val_t wb_reg_inst = tile.Top_Tile_core_dpath__wb_reg_inst.lo_word();
      val_t wb_waddr = wb_reg_inst >> 27;
      val_t wb_reg_raddr1 = (wb_reg_inst >> 22) & 0x1f;
      val_t wb_reg_raddr2 = (wb_reg_inst >> 17) & 0x1f;
      val_t wb_reg_rs1 =  tile.Top_Tile_core_dpath__wb_reg_rs1.lo_word();
      val_t wb_reg_rs2 =  tile.Top_Tile_core_dpath__wb_reg_rs2.lo_word();

      insn_t wb_insn;
      wb_insn.bits = wb_reg_inst;
      std::string wb_disasm = disasm.disassemble(wb_insn);
      
      if (log || (quiet && trace_count % 10000 == 0))
      {
        fprintf(logfile, "C: %10lld [%ld] pc=[%011lx] W[r%2ld=%016lx][%ld] R[r%2ld=%016lx] R[r%2ld=%016lx] inst=[%08lx] %-32s\n", \
                (long long)trace_count, tile.Top_Tile_core_ctrl__wb_reg_valid.lo_word(), tile.Top_Tile_core_dpath__wb_reg_pc.lo_word(), \
                tile.Top_Tile_core_dpath__wb_reg_waddr.lo_word(), tile.Top_Tile_core_dpath__wb_wdata.lo_word(), tile.Top_Tile_core_dpath__wb_wen.lo_word(),
                wb_reg_raddr1, wb_reg_rs1, wb_reg_raddr2, wb_reg_rs2, wb_reg_inst, wb_disasm.c_str());
      }

      if (vcd)
      {
        wb_disasm.resize(disasm_len, ' ');
        dat_t<disasm_len*8> disasm_dat;
        for (int i = 0; i < disasm_len; i++)
          disasm_dat = disasm_dat << 8 | LIT<8>(wb_disasm[i]);

        tile.dump(vcdfile, trace_count);
        dat_dump(vcdfile, disasm_dat, "NDISASM_WB");
        dat_dump(vcdfile, dat_t<64>(trace_count), "NCYCLE\n");
      }
    }

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
