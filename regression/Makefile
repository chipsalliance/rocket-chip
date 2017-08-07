base_dir = $(abspath ..)
generated_dir = $(abspath ./generated-src)

# The default target, which runs all regression targets.
regression: vsim-regression emulator-regression

# Regression targets for the various simulators.
%-regression: %-asm-tests %-bmark-tests

# Some targets can run torture
vsim-regression: vsim-torture
emulator-regression: emulator-torture

# The torture configuration to use
TORTURE_CONFIG ?= default

# The top-level directory that contains rocket-chip
TOP ?= ..

# The hash of the tools that we're using
TOOLS_HASH ?= $(shell git -C $(TOP) ls-tree HEAD -- riscv-tools | xargs echo | cut -d' ' -f3)
$(info Using riscv-tools of $(TOOLS_HASH))

# The directory that the tools get built into.
RISCV ?= install/$(TOOLS_HASH)

# Torture saves the failing tests into a directory, which defaults to just somehing inside the regressions directory.
TORTURE_SAVE_DIR ?= torture-failures

# Include top-level makefrag for options like rocketchip_addons
include $(TOP)/Makefrag

# Removes all the build stamps from the current config
.PHONY: clean
clean:
	rm -rf stamps $(abspath $(RISCV))
	$(MAKE) RISCV=$(RISCV) -C $(abspath $(TOP)/vsim) clean
	$(MAKE) RISCV=$(RISCV) -C $(abspath $(TOP)/emulator) clean

ifeq ($(SUITE),)
$(error Set SUITE to the regression suite you want to run)
endif

ifeq ($(SUITE),RocketSuiteA)
PROJECT=freechips.rocketchip.system
CONFIGS=DefaultConfig
endif

ifeq ($(SUITE),RocketSuiteB)
PROJECT=freechips.rocketchip.system
CONFIGS=DefaultBufferlessConfig
endif

ifeq ($(SUITE),RocketSuiteC)
PROJECT=freechips.rocketchip.system
CONFIGS=TinyConfig
endif

ifeq ($(SUITE),UnittestSuite)
PROJECT=freechips.rocketchip.unittest
CONFIGS=AMBAUnitTestConfig TLSimpleUnitTestConfig TLWidthUnitTestConfig
endif

ifeq ($(SUITE), JtagDtmSuite)
CONFIGS_32=WithJtagDTM_DefaultRV32Config
CONFIGS_64=WithJtagDTM_DefaultConfig
CONFIGS += $(CONFIGS_32)
CONFIGS += $(CONFIGS_64)
endif

# These are the named regression targets.  While it's expected you run them in
# this order, since there's dependencies for everything it doesn't actually
# matter.  They're here to make running the various targets from the
# commandline a bit cleaner.
submodules: stamps/other-submodules.stamp
tools: $(RISCV)/install.stamp
verilator:
	$(MAKE) RISCV=$(RISCV) -C $(abspath $(TOP)/emulator) verilator

EMU_DEBUG_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/emulator-debug.stamp)
EMU_NDEBUG_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/emulator-ndebug.stamp)
EMU_ASM_TEST_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/emulator-asm-tests.stamp)
EMU_BMARK_TEST_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/emulator-bmark-tests.stamp)
EMU_REGRESSION_TEST_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/emulator-regression-tests.stamp)
EMU_TORTURE_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/emulator-torture-$(TORTURE_CONFIG).stamp)

emulator-debug: $(EMU_DEBUG_STAMPS)
emulator-ndebug: $(EMU_NDEBUG_STAMPS)
emulator-asm-tests: $(EMU_ASM_TEST_STAMPS)
emulator-bmark-tests: $(EMU_BMARK_TEST_STAMPS)
emulator-regression-tests: $(EMU_REGRESSION_TEST_STAMPS)
emulator-torture: $(EMU_TORTURE_STAMPS)

VSIM_VERILOG_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/vsim-verilog.stamp)
VSIM_DEBUG_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/vsim-debug.stamp)
VSIM_NDEBUG_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/vsim-ndebug.stamp)
VSIM_ASM_TEST_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/vsim-asm-tests.stamp)
VSIM_BMARK_TEST_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/vsim-bmark-tests.stamp)
VSIM_REGRESSION_TEST_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/vsim-regression-tests.stamp)
VSIM_TORTURE_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/vsim-torture-$(TORTURE_CONFIG).stamp)

vsim-verilog: $(VSIM_VERILOG_STAMPS)
vsim-debug: $(VSIM_DEBUG_STAMPS)
vsim-ndebug: $(VSIM_NDEBUG_STAMPS)
vsim-asm-tests: $(VSIM_ASM_TEST_STAMPS)
vsim-bmark-tests: $(VSIM_BMARK_TEST_STAMPS)
vsim-regression-tests: $(VSIM_REGRESSION_TEST_STAMPS)
vsim-torture: $(VSIM_TORTURE_STAMPS)

submodule_names = chisel3 firrtl torture hardfloat $(ROCKETCHIP_ADDONS)

# Checks out all the rocket-chip submodules
stamps/other-submodules.stamp:
	mkdir -p $(dir $@)
	git -C $(abspath $(TOP)) submodule update --init --recursive $(submodule_names)
	date > $@

$(RISCV)/install.stamp:
	mkdir -p $(dir $@)
	git -C $(abspath $(TOP)) submodule update --init riscv-tools
	rm -f $(abspath $(TOP))/riscv-tools/.travis.yml
	git -C $(abspath $(TOP))/riscv-tools submodule update --init --recursive riscv-gnu-toolchain
	git -C $(abspath $(TOP))/riscv-tools submodule update --init --recursive riscv-isa-sim
	git -C $(abspath $(TOP))/riscv-tools submodule update --init --recursive riscv-fesvr
	git -C $(abspath $(TOP))/riscv-tools submodule update --init --recursive riscv-opcodes
	git -C $(abspath $(TOP))/riscv-tools submodule update --init --recursive riscv-pk
	git -C $(abspath $(TOP))/riscv-tools submodule update --init --recursive riscv-tests
	git -C $(abspath $(TOP))/riscv-tools submodule update --init --recursive riscv-openocd
	+cd $(abspath $(TOP))/riscv-tools; RISCV=$(abspath $(RISCV)) ./build.sh
	date > $@

# Builds the various simulators
stamps/%/emulator-verilog.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/emulator PROJECT=$(PROJECT) CONFIG=$* RISCV=$(abspath $(RISCV)) verilog
	date > $@

stamps/%/emulator-ndebug.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/emulator PROJECT=$(PROJECT) CONFIG=$* RISCV=$(abspath $(RISCV))
	date > $@

stamps/%/emulator-debug.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/emulator PROJECT=$(PROJECT) CONFIG=$* RISCV=$(abspath $(RISCV)) debug
	date > $@

stamps/%/vsim-verilog.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/vsim PROJECT=$(PROJECT) CONFIG=$* RISCV=$(abspath $(RISCV)) verilog
	date > $@

stamps/%/vsim-ndebug.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/vsim PROJECT=$(PROJECT) CONFIG=$* RISCV=$(abspath $(RISCV))
	date > $@

stamps/%/vsim-debug.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/vsim PROJECT=$(PROJECT) CONFIG=$* RISCV=$(abspath $(RISCV)) debug
	date > $@

# Runs tests on one of the simulators
stamps/%/emulator-asm-tests.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/emulator PROJECT=$(PROJECT) CONFIG=$* RISCV=$(abspath $(RISCV)) run-asm-tests-fast
	date > $@

stamps/%/emulator-bmark-tests.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/emulator PROJECT=$(PROJECT) CONFIG=$* RISCV=$(abspath $(RISCV)) run-bmark-tests-fast
	date > $@

stamps/%/emulator-regression-tests.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/emulator PROJECT=$(PROJECT) CONFIG=$* RISCV=$(abspath $(RISCV)) clean-run-output
	$(MAKE) -C $(abspath $(TOP))/emulator PROJECT=$(PROJECT) CONFIG=$* RISCV=$(abspath $(RISCV)) run-regression-tests-fast
	date > $@

stamps/%/vsim-asm-tests.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/vsim PROJECT=$(PROJECT) CONFIG=$* RISCV=$(abspath $(RISCV)) run-asm-tests-fast
	date > $@

stamps/%/vsim-bmark-tests.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/vsim PROJECT=$(PROJECT) CONFIG=$* RISCV=$(abspath $(RISCV)) run-bmark-tests-fast
	date > $@

stamps/%/vsim-regression-tests.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/vsim PROJECT=$(PROJECT) CONFIG=$* RISCV=$(abspath $(RISCV)) clean-run-output
	$(MAKE) -C $(abspath $(TOP))/vsim PROJECT=$(PROJECT) CONFIG=$* RISCV=$(abspath $(RISCV)) run-regression-tests-fast
	date > $@

# The torture tests run subtly differently on the different targets, so they
# don't have pattern rules like everything else does.
stamps/%/vsim-torture-$(TORTURE_CONFIG).stamp: stamps/%/vsim-debug.stamp stamps/%/vsim-ndebug.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/torture rnight RTL_CONFIG=$* RISCV=$(abspath $(RISCV)) PATH="$(abspath $(RISCV)/bin:$(PATH))" OPTIONS="-C $(abspath $(TOP)/torture/config/$(TORTURE_CONFIG).config) -p $(abspath $(TORTURE_SAVE_DIR)) -m 30 -t 10"
	date > $@

stamps/%/emulator-torture-$(TORTURE_CONFIG).stamp: stamps/%/emulator-debug.stamp stamps/%/emulator-ndebug.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/torture cnight RTL_CONFIG=$* RISCV=$(abspath $(RISCV)) PATH="$(abspath $(RISCV)/bin:$(PATH))" OPTIONS="-C $(abspath $(TOP)/torture/config/$(TORTURE_CONFIG).config) -p $(abspath $(TORTURE_SAVE_DIR)) -m 30 -t 10"
	date > $@


# Targets for JTAG DTM full-chain simulation

OPENOCD_DIR ?= $(RISCV)

# If this is defined empty, then all tests would run.
JTAG_DTM_TEST ?= MemTest64

ifdef DEBUG
JTAG_STAMP_SUFFIX=-debug
JTAG_DEBUG_SUFFIX=-debug
JTAG_VCDPLUS_32= +vcdplusfile=regression32.vcd
JTAG_VCDPLUS_64= +vcdplusfile=regression64.vcd
OPENOCD_DEBUG= -d
else
JTAG_STAMP_SUFFIX=-ndebug
endif

stamps/%/jtag-dtm-32-$(JTAG_DTM_TEST).stamp: stamps/%/vsim$(JTAG_STAMP_SUFFIX).stamp
	export RISCV=$(RISCV) && $(abspath $(TOP))/riscv-tools/riscv-tests/debug/gdbserver.py \
	--sim_cmd "$(abspath $(TOP))/vsim/simv-$(PROJECT)-$*$(JTAG_DEBUG_SUFFIX) +verbose $(JTAG_VCDPLUS_32)" \
	--server_cmd="$(OPENOCD_DIR)/bin/openocd $(OPENOCD_DEBUG) \
	--s $(OPENOCD_DIR)/share/openocd/scripts" \
	--freedom-e300-sim \
	$(JTAG_DTM_TEST)
	date > $@

stamps/%/jtag-dtm-64-$(JTAG_DTM_TEST).stamp: stamps/%/vsim$(JTAG_STAMP_SUFFIX).stamp
	export RISCV=$(RISCV) && $(abspath $(TOP))/riscv-tools/riscv-tests/debug/gdbserver.py \
	--sim_cmd "$(abspath $(TOP))/vsim/simv-$(PROJECT)-$*$(JTAG_DEBUG_SUFFIX) +verbose $(JTAG_VCDPLUS_64)" \
	--server_cmd="$(OPENOCD_INSTALL)_$(OPENOCD_VERSION)/bin/openocd $(OPENOCD_DEBUG) \
	--s $(OPENOCD_INSTALL)_$(OPENOCD_VERSION)/share/openocd/scripts" \
	--freedom-u500-sim \
	$(JTAG_DTM_TEST)
	date > $@

JTAG_DTM_32_TEST_STAMPS=$(foreach config,$(CONFIGS_32),stamps/$(config)/jtag-dtm-32-$(JTAG_DTM_TEST).stamp)
JTAG_DTM_64_TEST_STAMPS=$(foreach config,$(CONFIGS_64),stamps/$(config)/jtag-dtm-64-$(JTAG_DTM_TEST).stamp)

jtag-dtm-tests-32 : $(JTAG_DTM_32_TEST_STAMPS)
jtag-dtm-tests-64 : $(JTAG_DTM_64_TEST_STAMPS)

# Targets for JTAG DTM full-chain simulation
jtag-dtm-regression: jtag-dtm-tests-32 jtag-dtm-tests-64
