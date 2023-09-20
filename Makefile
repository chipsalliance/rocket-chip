base_dir=$(abspath ./)

CHISEL_VERSION=3.6.0
MODEL ?= TestHarness
PROJECT ?= freechips.rocketchip.system
CFG_PROJECT ?= $(PROJECT)
CONFIG ?= $(CFG_PROJECT).DefaultConfig
MILL ?= mill

scala_srcs := $(shell find $(base_dir) -name "*.scala" -o -name "*.sc")
resource_dirs := $(shell find $(base_dir) -type d -path "*/src/main/resources")
resources := $(foreach d,$(resource_dirs),$(shell find $(d) -type f))
all_srcs := $(scala_srcs) $(resources)

ROCKET_CHIP_JAR := $(base_dir)/out/rocketchip/assembly.dest/out.jar
$(ROCKET_CHIP_JAR): $(all_srcs)
	cd $(base_dir) && $(MILL) rocketchip[$(CHISEL_VERSION)].assembly

verilog:
	cd $(base_dir) && $(MILL) emulator[freechips.rocketchip.system.TestHarness,$(CONFIG)].mfccompiler.compile

clean:
	rm -rf out/
