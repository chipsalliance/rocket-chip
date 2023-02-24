#include "vbridge.h"
#include "vbridge_impl.h"

void VBridge::setup(const std::string &bin, const std::string &ebin, const std::string &wave, uint64_t reset_vector, uint64_t cycles) const {
  impl->setup(bin, ebin, wave, reset_vector, cycles);
}

VBridge::VBridge() : impl(std::make_unique<VBridgeImpl>()) {}

VBridge::~VBridge() {
}

void VBridge::loop() const {
  impl->run();
}

void VBridge::configure_simulator(int argc, char **argv) const {
  impl->configure_simulator(argc, argv);
}
