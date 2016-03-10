# The default target, which runs all regression targets.
regression: vsim-regression fsim-regression emulator-regression

# Regression targets for the various simulators.
%-regression: %-asm-tests %-bmark-tests

# Some targets can run torture
vsim-regression: vsim-torture
emulator-regression: emulator-torture

ifeq ($(CONFIG),)
$(error Set CONFIG to the rocket-chip configuration to elaborate and test)
endif

ifeq ($(TORTURE_CONFIG),)
$(error Set TORTURE_CONFIG to the torture configuration to run)
endif

# The top-level directory that contains rocket-chip
TOP ?= ..

# The hash of the tools that we're using
TOOLS_HASH ?= $(shell git -C $(TOP) ls-tree HEAD -- riscv-tools | xargs echo | cut -d' ' -f3)
$(info Using riscv-tools of $(TOOLS_HASH))

# The directory that the tools get built into.
RISCV ?= install/$(TOOLS_HASH)

# Torture saves the failing tests into a directory, which defaults to just somehing inside the regressions directory.
TORTURE_SAVE_DIR ?= torture-failures

# Removes all the build stamps from the current config
.PHONY: clean
clean:
	rm -rf stamps $(abspath $(RISCV))
	$(MAKE) RISCV=$(RISCV) -C $(abspath $(TOP)/vsim) clean
	$(MAKE) RISCV=$(RISCV) -C $(abspath $(TOP)/fsim) clean
	$(MAKE) RISCV=$(RISCV) -C $(abspath $(TOP)/emulator) clean

# These are the named regression targets.  While it's expected you run them in
# this order, since there's dependencies for everything it doesn't actually
# matter.  They're here to make running the various targets from the
# commandline a bit cleaner.
submodules: stamps/other-submodules.stamp
tools: $(RISCV)/install.stamp

emulator-debug: stamps/$(CONFIG)/emulator-debug.stamp
emulator-ndebug: stamps/$(CONFIG)/emulator-ndebug.stamp
emulator-asm-tests: stamps/$(CONFIG)/emulator-asm-tests.stamp
emulator-bmark-tests: stamps/$(CONFIG)/emulator-bmark-tests.stamp
emulator-torture: stamps/$(CONFIG)/emulator-torture-$(TORTURE_CONFIG).stamp

vsim-verilog: stamps/$(CONFIG)/vsim-verilog.stamp
vsim-debug: stamps/$(CONFIG)/vsim-debug.stamp
vsim-ndebug: stamps/$(CONFIG)/vsim-ndebug.stamp
vsim-asm-tests: stamps/$(CONFIG)/vsim-asm-tests.stamp
vsim-bmark-tests: stamps/$(CONFIG)/vsim-bmark-tests.stamp
vsim-torture: stamps/$(CONFIG)/vsim-torture-$(TORTURE_CONFIG).stamp

fsim-verilog: stamps/$(CONFIG)/fsim-verilog.stamp
fsim-debug: stamps/$(CONFIG)/fsim-debug.stamp
fsim-ndebug: stamps/$(CONFIG)/fsim-ndebug.stamp
fsim-asm-tests: stamps/$(CONFIG)/fsim-asm-tests.stamp
fsim-bmark-tests: stamps/$(CONFIG)/fsim-bmark-tests.stamp
fsim-torture: stamps/$(CONFIG)/fsim-torture-$(TORTURE_CONFIG).stamp

submodule_names = chisel context-dependent-environments dramsim2 groundtest hardfloat junctions rocket torture uncore zscale $(ROCKETCHIP_ADDONS)

# Checks out all the rocket-chip submodules
stamps/other-submodules.stamp:
	mkdir -p $(dir $@)
	git -C $(abspath $(TOP)) submodule update --init --recursive $(submodule_names)
	date > $@

$(RISCV)/install.stamp:
	mkdir -p $(dir $@)
	git -C $(abspath $(TOP)) submodule update --init riscv-tools
	git -C $(abspath $(TOP))/riscv-tools submodule update --init --recursive riscv-gnu-toolchain
	git -C $(abspath $(TOP))/riscv-tools submodule update --init --recursive riscv-isa-sim
	git -C $(abspath $(TOP))/riscv-tools submodule update --init --recursive riscv-fesvr
	git -C $(abspath $(TOP))/riscv-tools submodule update --init --recursive riscv-opcodes
	git -C $(abspath $(TOP))/riscv-tools submodule update --init --recursive riscv-pk
	git -C $(abspath $(TOP))/riscv-tools submodule update --init --recursive riscv-tests
	+cd $(abspath $(TOP))/riscv-tools; RISCV=$(abspath $(RISCV)) ./build.sh
	date > $@

# Builds the various simulators
stamps/$(CONFIG)/%-verilog.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/$(patsubst stamps/$(CONFIG)/%-verilog.stamp,%,$@) CONFIG=$(CONFIG) RISCV=$(abspath $(RISCV)) verilog
	date > $@	

stamps/$(CONFIG)/%-ndebug.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/$(patsubst stamps/$(CONFIG)/%-ndebug.stamp,%,$@) CONFIG=$(CONFIG) RISCV=$(abspath $(RISCV))
	date > $@

stamps/$(CONFIG)/%-debug.stamp: stamps/other-submodules.stamp $(RISCV)/install.stamp
	mkdir -p $(dir $@)
	+flock -x $(dir $@)/chisel-lock $(MAKE) -C $(abspath $(TOP))/$(patsubst stamps/$(CONFIG)/%-debug.stamp,%,$@) CONFIG=$(CONFIG) RISCV=$(abspath $(RISCV)) debug
	date > $@

# Runs tests on one of the simulators
stamps/$(CONFIG)/%-asm-tests.stamp: stamps/$(CONFIG)/%-ndebug.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/$(patsubst stamps/$(CONFIG)/%-asm-tests.stamp,%,$@) CONFIG=$(CONFIG) RISCV=$(abspath $(RISCV)) run-asm-tests
	date > $@

stamps/$(CONFIG)/%-bmark-tests.stamp: stamps/$(CONFIG)/%-ndebug.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/$(patsubst stamps/$(CONFIG)/%-bmark-tests.stamp,%,$@) CONFIG=$(CONFIG) RISCV=$(abspath $(RISCV)) run-bmark-tests
	date > $@

# The torture tests run subtly differently on the different targets, so they
# don't have pattern rules like everything else does.
stamps/$(CONFIG)/vsim-torture-$(TORTURE_CONFIG).stamp: stamps/$(CONFIG)/vsim-debug.stamp stamps/$(CONFIG)/vsim-ndebug.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/torture rnight RTL_CONFIG=$(CONFIG) RISCV=$(abspath $(RISCV)) PATH="$(abspath $(RISCV)/bin:$(PATH))" OPTIONS="-C $(abspath $(TOP)/torture/config/$(TORTURE_CONFIG).config) -p $(abspath $(TORTURE_SAVE_DIR)) -m 30 -t 10"
	date > $@

stamps/$(CONFIG)/emulator-torture-$(TORTURE_CONFIG).stamp: stamps/$(CONFIG)/emulator-debug.stamp stamps/$(CONFIG)/emulator-debug.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/torture cnight RTL_CONFIG=$(CONFIG) RISCV=$(abspath $(RISCV)) PATH="$(abspath $(RISCV)/bin:$(PATH))" OPTIONS="-C $(abspath $(TOP)/torture/config/$(TORTURE_CONFIG).config) -p $(abspath $(TORTURE_SAVE_DIR)) -m 30 -t 10"
	date > $@
