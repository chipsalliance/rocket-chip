# The default target, which runs all regression targets.
regression: vsim-regression fsim-regression emulator-regression

# Regression targets for the various simulators.
%-regression: %-asm-tests %-bmark-tests

# Some targets can run torture
vsim-regression: vsim-torture
emulator-regression: emulator-torture

ifeq ($(TORTURE_CONFIG),)
$(error Set TORTURE_CONFIG to the torture configuration to run)
endif

# The version of Chisel to use
CHISEL_VERSION ?= 2

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
	$(MAKE) RISCV=$(RISCV) -C $(abspath $(TOP)/fsim) clean
	$(MAKE) RISCV=$(RISCV) -C $(abspath $(TOP)/emulator) clean

ifeq ($(SUITE),)
$(error Set SUITE to the regression suite you want to run)
endif

ifeq ($(SUITE),RocketSuite)
CONFIGS=DefaultConfig DefaultL2Config DefaultBufferlessConfig TinyConfig RoccExampleConfig
endif

ifeq ($(SUITE),GroundtestSuite)
CONFIGS=MemtestConfig MemtestBufferlessConfig MemtestStatelessConfig FancyMemtestConfig \
	BroadcastRegressionTestConfig BufferlessRegressionTestConfig CacheRegressionTestConfig \
	ComparatorConfig ComparatorBufferlessConfig ComparatorL2Config ComparatorStatelessConfig \
	UnitTestConfig
endif

# These are the named regression targets.  While it's expected you run them in
# this order, since there's dependencies for everything it doesn't actually
# matter.  They're here to make running the various targets from the
# commandline a bit cleaner.
submodules: stamps/other-submodules.stamp
tools: $(RISCV)/install.stamp

EMU_DEBUG_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/emulator-debug.stamp)
EMU_NDEBUG_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/emulator-ndebug.stamp)
EMU_ASM_TEST_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/emulator-asm-tests.stamp)
EMU_BMARK_TEST_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/emulator-bmark-tests.stamp)
EMU_TORTURE_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/emulator-torture-$(TORTURE_CONFIG).stamp)

emulator-debug: $(EMU_DEBUG_STAMPS)
emulator-ndebug: $(EMU_NDEBUG_STAMPS)
emulator-asm-tests: $(EMU_ASM_TEST_STAMPS)
emulator-bmark-tests: $(EMU_BMARK_TEST_STAMPS)
emulator-torture: $(EMU_TORTURE_STAMPS)

VSIM_VERILOG_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/vsim-verilog.stamp)
VSIM_DEBUG_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/vsim-debug.stamp)
VSIM_NDEBUG_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/vsim-ndebug.stamp)
VSIM_ASM_TEST_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/vsim-asm-tests.stamp)
VSIM_BMARK_TEST_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/vsim-bmark-tests.stamp)
VSIM_TORTURE_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/vsim-torture-$(TORTURE_CONFIG).stamp)

vsim-verilog: $(VSIM_VERILOG_STAMPS)
vsim-debug: $(VSIM_DEBUG_STAMPS)
vsim-ndebug: $(VSIM_NDEBUG_STAMPS)
vsim-asm-tests: $(VSIM_ASM_TEST_STAMPS)
vsim-bmark-tests: $(VSIM_BMARK_TEST_STAMPS)
vsim-torture: $(VSIM_TORTURE_STAMPS)

FSIM_VERILOG_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/fsim-verilog.stamp)
FSIM_DEBUG_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/fsim-debug.stamp)
FSIM_NDEBUG_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/fsim-ndebug.stamp)
FSIM_ASM_TEST_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/fsim-asm-tests.stamp)
FSIM_BMARK_TEST_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/fsim-bmark-tests.stamp)
FSIM_TORTURE_STAMPS=$(foreach config,$(CONFIGS),stamps/$(config)/fsim-torture-$(TORTURE_CONFIG).stamp)

fsim-verilog: $(FSIM_VERILOG_STAMPS)
fsim-debug: $(FSIM_DEBUG_STAMPS)
fsim-ndebug: $(FSIM_NDEBUG_STAMPS)
fsim-asm-tests: $(FSIM_ASM_TEST_STAMPS)
fsim-bmark-tests: $(FSIM_BMARK_TEST_STAMPS)
fsim-torture: $(FSIM_TORTURE_STAMPS)

submodule_names = chisel2 chisel3 context-dependent-environments dramsim2 firrtl groundtest hardfloat junctions rocket torture uncore $(ROCKETCHIP_ADDONS)

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
	+cd $(abspath $(TOP))/riscv-tools; RISCV=$(abspath $(RISCV)) ./build.sh
	date > $@

# Builds the various simulators
stamps/%/emulator-verilog.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/emulator CONFIG=$* RISCV=$(abspath $(RISCV)) CHISEL_VERSION=$(CHISEL_VERSION) verilog
	date > $@

stamps/%/emulator-ndebug.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/emulator CONFIG=$* RISCV=$(abspath $(RISCV)) CHISEL_VERSION=$(CHISEL_VERSION)
	date > $@

stamps/%/emulator-debug.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/emulator CONFIG=$* RISCV=$(abspath $(RISCV)) CHISEL_VERSION=$(CHISEL_VERSION) debug
	date > $@

stamps/%/vsim-verilog.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/vsim CONFIG=$* RISCV=$(abspath $(RISCV)) CHISEL_VERSION=$(CHISEL_VERSION) verilog
	date > $@

stamps/%/vsim-ndebug.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/vsim CONFIG=$* RISCV=$(abspath $(RISCV)) CHISEL_VERSION=$(CHISEL_VERSION)
	date > $@

stamps/%/vsim-debug.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/vsim CONFIG=$* RISCV=$(abspath $(RISCV)) CHISEL_VERSION=$(CHISEL_VERSION) debug
	date > $@

stamps/%/fsim-verilog.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/fsim CONFIG=$* RISCV=$(abspath $(RISCV)) CHISEL_VERSION=$(CHISEL_VERSION) verilog
	date > $@

stamps/%/fsim-ndebug.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/fsim CONFIG=$* RISCV=$(abspath $(RISCV)) CHISEL_VERSION=$(CHISEL_VERSION)
	date > $@

stamps/%/fsim-debug.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/fsim CONFIG=$* RISCV=$(abspath $(RISCV)) CHISEL_VERSION=$(CHISEL_VERSION) debug
	date > $@

# Runs tests on one of the simulators
stamps/%/emulator-asm-tests.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/emulator CONFIG=$* RISCV=$(abspath $(RISCV)) CHISEL_VERSION=$(CHISEL_VERSION) run-asm-tests-fast
	date > $@

stamps/%/emulator-bmark-tests.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/emulator CONFIG=$* RISCV=$(abspath $(RISCV)) CHISEL_VERSION=$(CHISEL_VERSION) run-bmark-tests-fast
	date > $@

stamps/%/vsim-asm-tests.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/vsim CONFIG=$* RISCV=$(abspath $(RISCV)) CHISEL_VERSION=$(CHISEL_VERSION) run-asm-tests-fast
	date > $@

stamps/%/vsim-bmark-tests.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/vsim CONFIG=$* RISCV=$(abspath $(RISCV)) CHISEL_VERSION=$(CHISEL_VERSION) run-bmark-tests-fast
	date > $@

stamps/%/fsim-asm-tests.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/fsim CONFIG=$* RISCV=$(abspath $(RISCV)) CHISEL_VERSION=$(CHISEL_VERSION) run-asm-tests-fast
	date > $@

stamps/%/fsim-bmark-tests.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/fsim CONFIG=$* RISCV=$(abspath $(RISCV)) CHISEL_VERSION=$(CHISEL_VERSION) run-bmark-tests-fast
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
