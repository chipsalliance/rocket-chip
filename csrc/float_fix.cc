#include <assert.h>
#include <cinttypes>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <sstream>


// returns the bits in x[high:low] in the lowest positions
uint64_t BitRange(uint64_t x, int high, int low) {
  int high_gap = 64 - high;
  return x << high_gap >> (low + high_gap);
}


// will "unrecode" a single float within a double
// uses magic numbers since can only handle float
// logic from berkeley-hardfloat/src/main/scala/recodedFloatNToFloatN.scala
uint64_t UnrecodeFloatFromDouble(uint64_t raw_input) {
  uint64_t recoded_float = raw_input & 0x1ffffffff;  // lower 33 bits
  uint64_t sign = BitRange(recoded_float, 31, 31);
  uint64_t exp_in = BitRange(recoded_float, 30, 22);
  uint64_t sig_in = BitRange(recoded_float, 21, 0);

  bool is_high_subnormal_in = BitRange(exp_in, 6, 0) < 2;
  bool is_subnormal = (BitRange(exp_in, 8, 6) == 1) ||
                     ((BitRange(exp_in, 8, 7) == 1) && is_high_subnormal_in);
  bool is_normal = ((BitRange(exp_in, 8, 7) == 1) && !is_high_subnormal_in) ||
                    (BitRange(exp_in, 8, 7) == 2);
  bool is_special = BitRange(exp_in, 8, 7) == 3;
  bool is_NaN = is_special && BitRange(exp_in, 6, 6);

  uint64_t denorm_shift_dist = 2 - BitRange(exp_in, 4, 0);
  uint64_t subnormal_sig_out = (0x400000 | sig_in) >> denorm_shift_dist;
  uint64_t normal_exp_out = BitRange(exp_in, 7, 0) - 129;

  uint64_t exp_out = is_normal ? normal_exp_out : (is_special ? 255 : 0);
  uint64_t sig_out = is_normal || is_NaN ? sig_in :
                      is_subnormal ? subnormal_sig_out : 0;

  uint64_t raw_output64 = (sign << 63) | (exp_out << 23) | sig_out;
  // make sure nothing leaked over the top
  assert((raw_output64 & 0xffffffff00000000) == uint64_t(0));
  return raw_output64;
}


// checks if possibly recoded float inside double (upper 31 bits set)
bool NestedFloatPossible(uint64_t raw_input) {
  const uint64_t mask = 0xfffffffe00000000;
  return (raw_input & mask) == mask;
}


// best effort at replacing the float writeback with unrecoded version
// will only replace if (all of following met):
//   - log lines differ between rocket and lspike
//   - log line contains a floating point inst
//   - unrecoding the writeback data as a single float makes them match
void DiffAndFix(std::string rocket_filename, std::string lspike_filename) {
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
  std::stringstream ss;
  ss << std::hex;
  std::string rocket_line, lspike_line;
  while (getline(rocket_log,rocket_line) && getline(lspike_log,lspike_line)) {
    if ((rocket_line != lspike_line) &&
        (rocket_line.find(" f") != std::string::npos)) {
      std::string fixed_line(rocket_line.c_str());  // deep copy
      std::string fp_wb_str = fixed_line.substr(40, 16);
      uint64_t raw_fp;
      ss << fp_wb_str;
      ss >> raw_fp;
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
  // std::cout << NestedFloatPossible(0xf0f0f0ff0f000000) << std::endl;
  DiffAndFix(std::string(argv[1]), std::string(argv[2]));
  return 0;
}
