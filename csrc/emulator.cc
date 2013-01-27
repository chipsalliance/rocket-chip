#include "htif_emulator.h"
#include "common.h"
#include "emulator.h"
#include "mm.h"
#include "mm_dramsim2.h"
#include "disasm.h"
#include "Top.h" // chisel-generated code...
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

htif_emulator_t* htif;
void handle_sigterm(int sig)
{
  htif->stop();
}

int main(int argc, char** argv)
{
  unsigned random_seed = (unsigned)time(NULL) ^ (unsigned)getpid();
  uint64_t max_cycles = 0;
  uint64_t trace_count = 0;
  int start = 0;
  bool log = false;
  const char* vcd = NULL;
  const char* loadmem = NULL;
  FILE *vcdfile = NULL, *logfile = stderr;
  const char* failure = NULL;
  disassembler disasm;
  bool dramsim2 = false;

  for (int i = 1; i < argc; i++)
  {
    std::string arg = argv[i];
    if (arg.substr(0, 2) == "-v")
      vcd = argv[i]+2;
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

  mm_t* mm = dramsim2 ? (mm_t*)(new mm_dramsim2_t) : (mm_t*)(new mm_magic_t);
  mm->init(MEM_SIZE);
  if (loadmem)
    load_mem(mm->get_data(), loadmem);


  // The chisel generated code
  Top_t tile;
  srand(random_seed);
  tile.init(random_seed != 0);

  // Instantiate HTIF
  htif = new htif_emulator_t(std::vector<std::string>(argv + 1, argv + argc));
  int htif_bits = tile.Top__io_host_in_bits.width();
  assert(htif_bits % 8 == 0 && htif_bits <= val_n_bits());

  signal(SIGTERM, handle_sigterm);

  // reset for a few cycles to support pipelined reset
  tile.Top__io_host_in_valid = LIT<1>(0);
  tile.Top__io_host_out_ready = LIT<1>(0);
  tile.Top__io_mem_backup_en = LIT<1>(0);
  for (int i = 0; i < 10; i++)
  {
    tile.clock_lo(LIT<1>(1));
    tile.clock_hi(LIT<1>(1));
  }

  while (!htif->done())
  {
    tile.Top__io_mem_req_cmd_ready = LIT<1>(mm->req_cmd_ready());
    tile.Top__io_mem_req_data_ready = LIT<1>(mm->req_data_ready());
    tile.Top__io_mem_resp_valid = LIT<1>(mm->resp_valid());
    tile.Top__io_mem_resp_bits_tag = LIT<64>(mm->resp_tag());
    memcpy(&tile.Top__io_mem_resp_bits_data, mm->resp_data(), tile.Top__io_mem_resp_bits_data.width()/8);

    tile.clock_lo(LIT<1>(0));

    mm->tick(
      tile.Top__io_mem_req_cmd_valid.lo_word(),
      tile.Top__io_mem_req_cmd_bits_rw.lo_word(),
      tile.Top__io_mem_req_cmd_bits_addr.lo_word(),
      tile.Top__io_mem_req_cmd_bits_tag.lo_word(),

      tile.Top__io_mem_req_data_valid.lo_word(),
      &tile.Top__io_mem_req_data_bits_data.values[0]
    );

    if (tile.Top__io_host_clk_edge.to_bool())
    {
      bool in_valid = tile.Top__io_host_in_ready.to_bool() &&
                      htif->recv_nonblocking(&tile.Top__io_host_in_bits.values[0], htif_bits/8);
      tile.Top__io_host_in_valid = LIT<1>(in_valid);
      tile.Top__io_host_out_ready = LIT<1>(1);

      if (tile.Top__io_host_out_valid.to_bool())
        htif->send(&tile.Top__io_host_out_bits.values[0], htif_bits/8);
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
      
      if (log)
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

    if (max_cycles != 0 && trace_count == max_cycles)
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

  return 0;
}
