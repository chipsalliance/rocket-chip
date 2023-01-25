TESTHARNESS ?= freechips.rocketchip.system.TestHarness
CONFIG ?= freechips.rocketchip.system.DefaultConfig
RISCV_TESTS_SUITE ?= rv64mi-p
RISCV_TESTS_EXCLUDE ?= none
RISCV_ARCH_TESTS_XLEN ?= 64
RISCV_ARCH_TESTS_ISA ?= RV64IMAFDCZicsr_Zifencei

init:
	git submodule update --init

compile:
	mill -i rocketchip.compile

bsp:
	mill -i mill.bsp.BSP/install

emulator-chirrtl:
	mill -i show emulator[$(TESTHARNESS),$(CONFIG)].elaborate.chirrtl

emulator-systemverilog:
	mill -i show emulator[$(TESTHARNESS),$(CONFIG)].mfccompile.rtls

emulator-elf:
	mill -i show emulator[$(TESTHARNESS),$(CONFIG)].elf

run-riscv-tests:
	mill -i runnable-test[$(TESTHARNESS),$(CONFIG),$(RISCV_TESTS_SUITE),$(RISCV_TESTS_EXCLUDE)].run

run-riscv-arch-tests:
	mill -i runnable-arch-test[$(TESTHARNESS),$(CONFIG),$(RISCV_ARCH_TESTS_XLEN),$(RISCV_ARCH_TESTS_ISA )].run
