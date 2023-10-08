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

# The directory that tools get checked out into
RISCV_TOOLS = $(abspath $(TOP))/rocket-tools

# The hash of the tools that we're using
TOOLS_HASH ?= $(shell cat $(TOP)/riscv-tools.hash)
$(info Using rocket-tools of $(TOOLS_HASH))

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
CONFIGS=$(PROJECT).DefaultConfig
endif

ifeq ($(SUITE),RocketSuiteB)
PROJECT=freechips.rocketchip.system
CONFIGS=$(PROJECT).DefaultBufferlessConfig
endif

ifeq ($(SUITE),RocketSuiteC)
PROJECT=freechips.rocketchip.system
CONFIGS=$(PROJECT).TinyConfig
endif

ifeq ($(SUITE),UnittestSuite)
PROJECT=freechips.rocketchip.unittest
CONFIGS=$(PROJECT).AMBAUnitTestConfig $(PROJECT).TLSimpleUnitTestConfig $(PROJECT).TLWidthUnitTestConfig
endif

ifeq ($(SUITE), JtagDtmSuite)
PROJECT=freechips.rocketchip.system

export JTAG_DTM_ENABLE_SBA ?= off
ifeq ($(JTAG_DTM_ENABLE_SBA), off)
CONFIGS_32=$(PROJECT).WithJtagDTMSystem,$(PROJECT).DefaultRV32Config
CONFIGS_64=$(PROJECT).WithJtagDTMSystem,$(PROJECT).DefaultConfig
endif

ifeq ($(JTAG_DTM_ENABLE_SBA), on)
CONFIGS_32=$(PROJECT).WithJtagDTMSystem,$(PROJECT).WithDebugSBASystem,$(PROJECT).DefaultRV32Config
CONFIGS_64=$(PROJECT).WithJtagDTMSystem,$(PROJECT).WithDebugSBASystem,$(PROJECT).DefaultConfig
endif

CONFIGS += $(CONFIGS_32)
CONFIGS += $(CONFIGS_64)
endif

# Miscellaneous configs cover any remaining configurations not tested
# above, but are included in the freechips.rocketchip.system package.
# These are here to prevent regressions at the compilation level and
# expected to be built, but not executed.
ifeq ($(SUITE), Miscellaneous)
PROJECT=freechips.rocketchip.system
CONFIGS=\
	$(PROJECT).DefaultSmallConfig \
	$(PROJECT).DualBankConfig \
	$(PROJECT).DualChannelConfig \
	$(PROJECT).DualChannelDualBankConfig \
	$(PROJECT).RoccExampleConfig \
	$(PROJECT).Edge128BitConfig \
	$(PROJECT).Edge32BitConfig \
	$(PROJECT).QuadChannelBenchmarkConfig \
	$(PROJECT).EightChannelConfig \
	$(PROJECT).DualCoreConfig \
	$(PROJECT).MemPortOnlyConfig \
	$(PROJECT).MMIOPortOnlyConfig \
	$(PROJECT).CloneTileConfig
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

submodule_names = torture hardfloat cde $(ROCKETCHIP_ADDONS)

# Checks out all the rocket-chip submodules
stamps/other-submodules.stamp:
	mkdir -p $(dir $@)
	git -C $(abspath $(TOP)) submodule update --init --recursive $(submodule_names)
	date > $@

stamps/riscv-tests.stamp:
	git -C $(abspath $(TOP)) clone -n https://github.com/freechipsproject/rocket-tools.git
	git -C $(RISCV_TOOLS) checkout $(TOOLS_HASH)
	git -C $(RISCV_TOOLS) submodule update --init --recursive riscv-tests
	mkdir -p $(dir $@)
	date > $@

stamps/rocket-tools_checkout.stamp: | stamps/riscv-tests.stamp
	git -C $(RISCV_TOOLS) submodule update --init --recursive riscv-isa-sim riscv-pk riscv-openocd fsf-binutils-gdb
	git -C $(RISCV_TOOLS) submodule update --init riscv-gnu-toolchain
	git -C $(RISCV_TOOLS)/riscv-gnu-toolchain submodule update --init binutils gcc gdb glibc newlib
	rm -f $(RISCV_TOOLS)/.travis.yml
	mkdir -p $(dir $@)
	date > $@

# rocket-tools_checkout.stamp is an order-only prerequisite:
# https://www.gnu.org/software/make/manual/make.html#Prerequisite-Types
# This means that the rule will be executed, but it will /not/ be
# evaluated as a depenency for install.stamp. This is needed because
# the timestamp on install.stamp will be older than the checkout stamp
# anytime we restore the installation area from cache in CI.
$(RISCV)/install.stamp: | stamps/rocket-tools_checkout.stamp
	mkdir -p $(dir $@)
	+cd $(RISCV_TOOLS); RISCV=$(abspath $(RISCV)) ./build.sh
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

# If this is defined empty, then all tests would run.
JTAG_DTM_TEST ?= MemTest64
GDBSERVER = $(abspath $(TOP))/rocket-tools/riscv-tests/debug/gdbserver.py \
	    --print-failures \
	    --print-log-names

ifdef DEBUG
JTAG_STAMP_SUFFIX=-debug
JTAG_DEBUG_SUFFIX=-debug
VSIM_JTAG_VCDPLUS_32= +vcdplusfile=regression32.vcd
VSIM_JTAG_VCDPLUS_64= +vcdplusfile=regression64.vcd
EMULATOR_JTAG_VCDPLUS_32= -v regression32.vcd
EMULATOR_JTAG_VCDPLUS_64= -v regression64.vcd
OPENOCD_DEBUG= -d
else
JTAG_STAMP_SUFFIX=-ndebug
endif

ifdef SEED
SEED_ARG = --seed $(SEED)
endif

JTAG_DTM_SIM_ARGS = +jtag_rbb_enable=1 $(SEED_ARG)

stamps/%/vsim-jtag-dtm-32-$(JTAG_DTM_TEST).stamp: stamps/%/vsim$(JTAG_STAMP_SUFFIX).stamp stamps/riscv-tests.stamp
	RISCV=$(RISCV) PATH="$(abspath $(RISCV)/bin:$(PATH))" $(GDBSERVER) \
	--sim_cmd="$(abspath $(TOP))/vsim/simv-$(PROJECT)-$*$(JTAG_DEBUG_SUFFIX) $(JTAG_DTM_SIM_ARGS) $(VSIM_JTAG_VCDPLUS_32)" \
	--server_cmd="$(RISCV)/bin/openocd $(OPENOCD_DEBUG) \
	-s $(RISCV)/share/openocd/scripts" \
	--32 \
	$(abspath $(TOP))/scripts/RocketSim32.py \
	$(JTAG_DTM_TEST)
	date > $@

stamps/%/vsim-jtag-dtm-64-$(JTAG_DTM_TEST).stamp: stamps/%/vsim$(JTAG_STAMP_SUFFIX).stamp stamps/riscv-tests.stamp
	RISCV=$(RISCV) PATH="$(abspath $(RISCV)/bin:$(PATH))" $(GDBSERVER) \
	--sim_cmd="$(abspath $(TOP))/vsim/simv-$(PROJECT)-$*$(JTAG_DEBUG_SUFFIX) $(JTAG_DTM_SIM_ARGS) $(VSIM_JTAG_VCDPLUS_64)" \
	--server_cmd="$(RISCV)/bin/openocd $(OPENOCD_DEBUG) \
	-s $(RISCV)/share/openocd/scripts" \
	--64 \
	$(abspath $(TOP))/scripts/RocketSim64.py \
	$(JTAG_DTM_TEST)
	date > $@

stamps/%/emulator-jtag-dtm-32-$(JTAG_DTM_TEST).stamp:  stamps/%/emulator$(JTAG_STAMP_SUFFIX).stamp stamps/riscv-tests.stamp
	RISCV=$(RISCV) PATH="$(abspath $(RISCV)/bin:$(PATH))" $(GDBSERVER) \
	--sim_cmd="$(abspath $(TOP))/emulator/emulator-$(PROJECT)-$*$(JTAG_DEBUG_SUFFIX) $(JTAG_DTM_SIM_ARGS) $(EMULATOR_JTAG_VCDPLUS_32) dummybin" \
	--server_cmd="$(RISCV)/bin/openocd $(OPENOCD_DEBUG) \
	-s $(RISCV)/share/openocd/scripts" \
	--32 \
	$(abspath $(TOP))/scripts/RocketSim32.py \
	$(JTAG_DTM_TEST)
	date > $@

stamps/%/emulator-jtag-dtm-64-$(JTAG_DTM_TEST).stamp:  stamps/%/emulator$(JTAG_STAMP_SUFFIX).stamp stamps/riscv-tests.stamp
	RISCV=$(RISCV) PATH="$(abspath $(RISCV)/bin:$(PATH))" $(GDBSERVER) \
	--sim_cmd="$(abspath $(TOP))/emulator/emulator-$(PROJECT)-$*$(JTAG_DEBUG_SUFFIX) $(JTAG_DTM_SIM_ARGS) $(EMULATOR_JTAG_VCDPLUS_64) dummybin" \
	--server_cmd="$(RISCV)/bin/openocd $(OPENOCD_DEBUG) \
	-s $(RISCV)/share/openocd/scripts" \
	--64 \
	$(abspath $(TOP))/scripts/RocketSim64.py \
	$(JTAG_DTM_TEST)
	date > $@

VSIM_JTAG_DTM_32_TEST_STAMPS=$(foreach config,$(CONFIGS_32),stamps/$(config)/vsim-jtag-dtm-32-$(JTAG_DTM_TEST).stamp)
VSIM_JTAG_DTM_64_TEST_STAMPS=$(foreach config,$(CONFIGS_64),stamps/$(config)/vsim-jtag-dtm-64-$(JTAG_DTM_TEST).stamp)

EMULATOR_JTAG_DTM_32_TEST_STAMPS=$(foreach config,$(CONFIGS_32),stamps/$(config)/emulator-jtag-dtm-32-$(JTAG_DTM_TEST).stamp)
EMULATOR_JTAG_DTM_64_TEST_STAMPS=$(foreach config,$(CONFIGS_64),stamps/$(config)/emulator-jtag-dtm-64-$(JTAG_DTM_TEST).stamp)

vsim-jtag-dtm-tests-32 : $(VSIM_JTAG_DTM_32_TEST_STAMPS)
vsim-jtag-dtm-tests-64 : $(VSIM_JTAG_DTM_64_TEST_STAMPS)

emulator-jtag-dtm-tests-32 : $(EMULATOR_JTAG_DTM_32_TEST_STAMPS)
emulator-jtag-dtm-tests-64 : $(EMULATOR_JTAG_DTM_64_TEST_STAMPS)

# Targets for JTAG DTM full-chain simulation
vsim-jtag-dtm-regression: vsim-jtag-dtm-tests-32 vsim-jtag-dtm-tests-64
emulator-jtag-dtm-regression: emulator-jtag-dtm-tests-32 emulator-jtag-dtm-tests-64
