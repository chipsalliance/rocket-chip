#pragma once

namespace consts {

  // simulation arch config
  constexpr int vlen_in_bits = 1024;
  constexpr int vlen_in_bytes = vlen_in_bits / 8;
  constexpr int elen = 32;
  constexpr int numVRF = 32;

  // const as default value
  constexpr int lsuIdxDefault = 255;

  // rtl parameters
  constexpr int numTL = 2;
  constexpr int numMSHR = 3;
  constexpr int numLanes = 8;

}// namespace consts
