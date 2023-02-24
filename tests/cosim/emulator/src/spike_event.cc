#include <fmt/core.h>
#include <glog/logging.h>

#include "disasm.h"
#include "exceptions.h"
#include "glog_exception_safe.h"
#include "spike_event.h"
#include "tl_interface.h"
#include "util.h"

std::string SpikeEvent::describe_insn() const {
  return fmt::format("pc={:08X}, bits={:08X}, disasm='{}'", pc, inst_bits, proc.get_disassembler()->disassemble(inst_bits));
}

void SpikeEvent::pre_log_arch_changes() {
  if (is_store | is_load | is_amo) {
    uint64_t address = target_mem;
    uint64_t addr_align = address & 0xFFFFFFC0;
    // record mem block for cache
    for (int i = 0; i < 8; i++) {
      uint64_t data = 0;
      //scan 8 bytes to data
      for (int j = 0; j < 8; ++j) {
        data += (uint64_t) impl->load(addr_align + j + i * 8) << (j * 8);
      }
      block.blocks[i] = data;
      block.addr = addr_align;
      block.remaining = true;
    }
    LOG(INFO) << fmt::format("spike pre_log mem access on:{:08X} ; block_addr={:08X}", address, addr_align);
  }
}

void SpikeEvent::log_arch_changes() {

  state_t *state = proc.get_state();

  for (auto [write_idx, data]: state->log_reg_write) {
    // in spike, log_reg_write is arrange:
    // xx0000 <- x
    // xx0001 <- f
    // xx0010 <- vreg
    // xx0011 <- vec
    // xx0100 <- csr

    if ((write_idx & 0xf) == 0b0000) {// scalar rf
      uint64_t rd_should_be_bits = proc.get_state()->XPR[rd_idx];
      if (rd_new_bits != rd_should_be_bits) {
        rd_new_bits = rd_should_be_bits;
        is_rd_written = true;
        LOG(INFO) << fmt::format("Log Spike {:08X} with scalar rf change: x[{}] from {:08X} to {:08X}", pc, rd_idx, rd_old_bits, rd_new_bits);
      }
    }
  }

  for (auto mem_write: state->log_mem_write) {
    uint64_t address = std::get<0>(mem_write);
    if (address != target_mem) {
      LOG(FATAL) << fmt::format("Error! spike detect mem_write at= {:08X}; target mem = {:08X}", address, target_mem);
    }
    uint64_t value = std::get<1>(mem_write);
    // Byte size_bytes
    uint8_t size_by_byte = std::get<2>(mem_write);
    LOG(INFO) << fmt::format("spike detect mem write {:08X} on mem:{:08X} with size={}byte", value, address, size_by_byte);
    mem_access_record.all_writes[address] = {.size_by_byte = size_by_byte, .val = value};
  }
  // since log_mem_read doesn't record mem data, we need to load manually
  for (auto mem_read: state->log_mem_read) {
    uint64_t address = std::get<0>(mem_read);
    if (address != target_mem) {
      LOG(FATAL) << fmt::format("Error! spike detect mem_read at= {:08X}; target mem = {:08X}", address, target_mem);
    }
    // Byte size_bytes
    uint8_t size_by_byte = std::get<2>(mem_read);
    uint64_t value = 0;
    // record mem target
    for (int i = 0; i < size_by_byte; ++i) {
      value += (uint64_t) impl->load(address + i) << (i * 8);
    }
    LOG(INFO) << fmt::format("spike detect mem read {:08X} on mem:{:08X} with size={}byte", value, address, size_by_byte);
    mem_access_record.all_reads[address] = {.size_by_byte = size_by_byte, .val = value};
  }

  // record root page table
  if (satp_mode == 0x8 && block.addr == -1) {
    uint64_t root_addr = satp_ppn << 12;
    for (int i = 0; i < 8; i++) {
      uint64_t data = 0;
      //scan 8 bytes to data
      for (int j = 0; j < 8; ++j) {
        data += (uint64_t) impl->load(root_addr + j + i * 8) << (j * 8);
      }
      block.blocks[i] = data;
      block.addr = root_addr;
      block.remaining = true;
    }
  }

  state->log_reg_write.clear();
  state->log_mem_read.clear();
  state->log_mem_write.clear();
}

SpikeEvent::SpikeEvent(processor_t &proc, insn_fetch_t &fetch, VBridgeImpl *impl) : proc(proc), impl(impl) {
  auto &xr = proc.get_state()->XPR;
  pc = proc.get_state()->pc;
  inst_bits = fetch.insn.bits();
  target_mem = -1;
  is_committed = false;// j insn should be committed immediately cause it doesn't have wb stage.

  // extension depending parameter
  is_compress = false;
  if (fetch.insn.length() == 2) {
    is_compress = true;
  }
  if (!is_compress) {
    rs1_bits = xr[fetch.insn.rs1()];
    rs2_bits = xr[fetch.insn.rs2()];
    rd_idx = fetch.insn.rd();
    opcode = clip(inst_bits, 0, 6);
    // for j insn for x0;
    if (opcode == 0b1101111 && rd_idx == 0) {
      is_committed = true;
    }
    // for integer/double load/store
    is_load = (opcode == 0b11) || (opcode == 0b0000111);
    is_store = opcode == 0b100011 || (opcode == 0b0100111);
    is_amo = opcode == 0b0101111;

    if (is_load) {
      target_mem = rs1_bits + fetch.insn.i_imm();
    }
    if (is_store) target_mem = rs1_bits + fetch.insn.s_imm();
    if (is_amo) target_mem = rs1_bits;
  } else {
    uint8_t op = inst_bits & 0b11;
    uint8_t func3 = (inst_bits & 0xE000) >> 13;
    uint64_t rs1s_bits = xr[fetch.insn.rvc_rs1s()];
    uint64_t sp_bits = xr[2];
    switch (op) {
      case 0:
        rd_idx = 8 + ((inst_bits & 0b11100) >> 2);// todo: rd_idx for store insn?
        if (func3 >= 5) {
          is_store = true;
          switch (func3) {
            case 5:// C.FSD
              target_mem = rs1s_bits + fetch.insn.rvc_ld_imm();
              break;
            case 6:// C.SW
              target_mem = rs1s_bits + fetch.insn.rvc_lw_imm();
              break;
            case 7:// C.SD
              target_mem = rs1s_bits + fetch.insn.rvc_ld_imm();
              break;
            default:
              LOG(FATAL) << fmt::format("unknown compress func3");
          }
        } else if (func3 >= 1 && func3 <= 3) {// include all 0-> the illegal insn
          is_load = true;
          switch (func3) {
            case 1:// C.FLD
              target_mem = rs1s_bits + fetch.insn.rvc_ld_imm();
              break;
            case 2:// C.LW
              target_mem = rs1s_bits + fetch.insn.rvc_lw_imm();
              break;
            case 3:// C.LD
              target_mem = rs1s_bits + fetch.insn.rvc_ld_imm();
              break;
            default:
              LOG(FATAL) << fmt::format("unknown compress func3");
          }
        } else {
        }
        // for store insn, no matter rd_idx
        break;
      case 1:// no load/store insn; 2 types rd_idx format
        if (func3 <= 3) rd_idx = fetch.insn.rd();
        else {
          rd_idx = fetch.insn.rvc_rs1s();
        }
        break;
      case 2:
        if (func3 >= 5) {
          is_store = true;
          rd_idx = 0;// todo: set rd_idx to 0
          switch (func3) {
            case 5:// C.FSDSP
              target_mem = sp_bits + fetch.insn.rvc_sdsp_imm();
              break;
            case 6:// C.SWSP
              target_mem = sp_bits + fetch.insn.rvc_swsp_imm();
              break;
            case 7:// C.SDSP
              target_mem = sp_bits + fetch.insn.rvc_sdsp_imm();
              break;
            default:
              LOG(FATAL) << fmt::format("unknown compress func3");
          }
        } else if (func3 >= 1 && func3 <= 3) {// for C.LWSP etc.
          is_load = true;
          rd_idx = fetch.insn.rd();
          switch (func3) {
            case 1:// C.FLDSP
              target_mem = sp_bits + fetch.insn.rvc_ldsp_imm();
              break;
            case 2:// C.LWSP
              target_mem = sp_bits + fetch.insn.rvc_lwsp_imm();
              break;
            case 3:// C.LDSP
              target_mem = sp_bits + fetch.insn.rvc_ldsp_imm();
              break;
            default:
              LOG(FATAL) << fmt::format("unknown compress func3");
          }

        } else if (func3 == 4 && (((inst_bits & 0x1000) >> 12) == 1) && (fetch.insn.rvc_rs1() != 0) && (fetch.insn.rvc_rs2() == 0)) {
          // C.JALR
          LOG(INFO) << fmt::format("find C.JALR");
          rd_idx = 1;// write x2

        } else {
          rd_idx = fetch.insn.rd();
        }
        break;
      default:
        LOG(FATAL) << fmt::format("unknown compress opcode");
    }
  }
  rd_old_bits = proc.get_state()->XPR[rd_idx];
  is_csr = opcode == 0b1110011;
  is_issued = false;
  is_trap = false;

  satp = proc.get_state()->satp->read();
  satp_ppn = satp & 0xFFFFFFFFFFF;
  satp_mode = clip(satp, 60, 63);

  block.addr = -1;
  disasm = proc.get_disassembler()->disassemble(fetch.insn);
}
