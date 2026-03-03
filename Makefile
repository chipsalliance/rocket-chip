base_dir=$(abspath ./)

CHISEL_VERSION=3.6.0
MODEL ?= TestHarness
PROJECT ?= freechips.rocketchip.system
CFG_PROJECT ?= $(PROJECT)
CONFIG ?= $(CFG_PROJECT).DefaultConfig
TEST ?= rv64mi-p
MILL ?= mill

verilog:
	cd $(base_dir) && $(MILL) emulator[freechips.rocketchip.system.TestHarness,$(CONFIG)].mfccompiler.compile

emulator:
	cd $(base_dir) && $(MILL) emulator[freechips.rocketchip.system.TestHarness,$(CONFIG)].emulator.elf

test:
	cd $(base_dir) && $(MILL) runnable-riscv-test[freechips.rocketchip.system.TestHarness,$(CONFIG),$(TEST),none].run

clean:
	rm -rf out/
