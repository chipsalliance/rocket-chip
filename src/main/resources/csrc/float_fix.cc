// See LICENSE.Berkeley for license details.

#include <assert.h>
#include <cinttypes>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>


// float_fix - Scott Beamer, 2015

// This program post-processes commit logs from rocket-chip to make it easier
// to diff against commit logs from spike. Rocket internally uses recoded
// floating point numbers, but this is normally hidden because the commit
// logging tool unrecodes (undoes the recoding) those numbers. This tool is for
// the corner case (single-precision float stored and loaded from memory as a
// double) when the commit logging tool is unable to recode properly on its
// own. Given both a rocket commit log (already processed by comlog) and commit
// log from spike, this tools attempts to fix that corner case. This tool will
// only overwrite the log to hold the unrecoded float if that change will cause
// it to match with the spike log (conservative).


// Returns the bits in x[high:low] in the lowest positions
uint64_t BitRange(uint64_t x, int high, int low) {
  int high_gap = 63 - high;
  return x << high_gap >> (low + high_gap);
}


// Returns uint64_t from the hex encoding within s offset by index
uint64_t UIntFromHexSubstring(std::string s, int index) {
  uint64_t x = strtoull(s.c_str() + index, nullptr, 16);
  assert(errno == 0);
  return x;
}


// Is commit line for a fld instruction?
bool LineIsFLDInst(std::string line) {
  uint32_t inst_bits = UIntFromHexSubstring(line, 22);
  uint32_t width_field = (inst_bits >> 12) & 7;
  uint32_t opcode_field = inst_bits & 127;
  return (width_field == 3) && (opcode_field == 7);
}


// Is number possibly a recoded float inside double (upper 31 bits set)?
bool NestedFloatPossible(uint64_t raw_input) {
  const uint64_t mask = 0xfffffffe00000000;
  return (raw_input & mask) == mask;
}


// Unrecodes a single float within a double
//   uses magic numbers since can only handle float
//   logic from berkeley-hardfloat/src/main/scala/recodedFloatNToFloatN.scala
uint64_t UnrecodeFloatFromDouble(uint64_t raw_input) {
  uint64_t recoded_float = raw_input & 0x1ffffffff;  // lower 33 bits
  uint64_t sign = BitRange(recoded_float, 32, 32);
  uint64_t exp_in = BitRange(recoded_float, 31, 23);
  uint64_t sig_in = BitRange(recoded_float, 22, 0);

  bool is_high_subnormal_in = BitRange(exp_in, 6, 0) < 2;
  bool is_subnormal = (BitRange(exp_in, 8, 6) == 1) ||
                     ((BitRange(exp_in, 8, 7) == 1) && is_high_subnormal_in);
  bool is_normal = (BitRange(exp_in, 8, 7) == 1) && !is_high_subnormal_in ||
                   (BitRange(exp_in, 8, 7) == 2);
  bool is_special = BitRange(exp_in, 8, 7) == 3;
  bool is_NaN = is_special && BitRange(exp_in, 6, 6);

  uint64_t denorm_shift_dist = 2 - BitRange(exp_in, 4, 0);
  uint64_t subnormal_sig_out = (0x400000 | sig_in) >> denorm_shift_dist;
  uint8_t normal_exp_out = BitRange(exp_in, 7, 0) - 129;

  uint64_t exp_out = is_normal ? normal_exp_out : (is_special ? 255 : 0);
  uint64_t sig_out = is_normal || is_NaN ? sig_in :
                     is_subnormal ? subnormal_sig_out : 0;

  uint64_t raw_output64 = (sign << 31) | (exp_out << 23) | sig_out;
  // assert((raw_output64 & 0xffffffff00000000) == uint64_t(0));
  // If this is not a recoded float, this will return gibberish, however,
  // the output will not match spike and thus the replacement will not happen.
  return raw_output64;
}


// Best effort at replacing the float writeback with unrecoded version
//   will only replace if (all of following met):
//   - log lines differ between rocket and lspike
//   - log line is a fld instruction
//   - unrecoding the writeback data as a single float makes them match
void DiffAndFix(std::string rocket_filename, std::string lspike_filename) {
  if (rocket_filename == "-")
    rocket_filename = "/dev/stdin";
  std::ifstream rocket_log(rocket_filename);
  if (!rocket_log.is_open()) {
    std::cout << "Couldn't open file " << rocket_filename << std::endl;
    std::exit(-2);
  }
  std::ifstream lspike_log(lspike_filename);
  if (!lspike_log.is_open()) {
    std::cout << "Couldn't open file " << lspike_filename << std::endl;
    std::exit(-2);
  }
  std::string rocket_line, lspike_line;
  while (getline(rocket_log, rocket_line)) {
    if (getline(lspike_log, lspike_line) && (rocket_line != lspike_line) &&
        LineIsFLDInst(rocket_line)) {
      std::string fixed_line(rocket_line.c_str());  // deep copy
      uint64_t raw_fp = UIntFromHexSubstring(fixed_line, 40);
      if (NestedFloatPossible(raw_fp)) {
        snprintf(const_cast<char*>(fixed_line.data()) + 40, 17,
                 "%016" PRIx64, UnrecodeFloatFromDouble(raw_fp));
        if (fixed_line == lspike_line)
          rocket_line = fixed_line;
      }
    }
    printf("%s\n", rocket_line.c_str());
  }
  rocket_log.close();
  lspike_log.close();
}


int main(int argc, char** argv) {
  if (argc != 3) {
    std::cout << "Usage: float_fix rocket_output lspike_output" << std::endl;
    return -1;
  }
  DiffAndFix(std::string(argv[1]), std::string(argv[2]));
  return 0;
}
