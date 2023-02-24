#pragma once

#include <cstddef>
#include <memory>
#include <string>

struct VBridgeImpl;

class VBridge {
public:
  VBridge();

  ~VBridge();

  void setup(const std::string &bin, const std::string &ebin, const std::string &vcd, uint64_t reset_vector, uint64_t cycles) const;

  void loop() const;

  void configure_simulator(int argc, char **argv) const;

private:
  std::unique_ptr<VBridgeImpl> impl;
};
