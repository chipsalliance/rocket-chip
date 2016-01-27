# The default target
regression: vsim-asm-tests vsim-bmark-tests torture

ifeq ($(CONFIG),)
$(error Set CONFIG to the configuration to build)
endif

# The top-level directory that contains rocket-chip
TOP ?= ..

# The directory that the tools get built into.
RISCV ?= install

# These are the named regression targets.  While it's expected you run them in
# this order, since there's dependencies for everything it doesn't actually
# matter.
submodules: stamps/submodules.stamp
tools: stamps/tools.stamp
vsim: stamps/vsim.stamp
vsim-asm-tests: stamps/vsim-asm-tests.stamp
vsim-bmark-tests: stamps/vsim-bmark-tests.stamp
torture: stamps/torture.stamp

# Checks out all the rocket-chip submodules
stamps/submodules.stamp:
	mkdir -p $(dir $@)
	git -C $(abspath $(TOP)) submodule update --init --recursive
	touch $@

# Builds the RISC-V toolchain
stamps/tools.stamp: stamps/submodules.stamp
	mkdir -p $(dir $@)
	+cd $(abspath $(TOP))/riscv-tools; RISCV=$(abspath $(RISCV)) ./build.sh
	touch $@

# Builds the verilog RTL simulator
stamps/vsim.stamp: stamps/submodules.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/vsim CONFIG=$(CONFIG) RISCV=$(abspath $(RISCV))
	touch $@

# Runs some tests using the verilog RTL simulator
stamps/vsim-asm-tests.stamp: stamps/vsim.stamp stamps/tools.stamp stamps/submodules.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/vsim CONFIG=$(CONFIG) RISCV=$(abspath $(RISCV)) run-asm-tests
	touch $@

stamps/vsim-bmark-tests.stamp: stamps/vsim.stamp stamps/tools.stamp stamps/submodules.stamp
	mkdir -p $(dir $@)
	$(MAKE) -C $(abspath $(TOP))/vsim CONFIG=$(CONFIG) RISCV=$(abspath $(RISCV)) run-bmark-tests
	touch $@
