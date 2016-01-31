#=======================================================================
# Makefile for Verilog simulation w/ VCS
#-----------------------------------------------------------------------
# Yunsup Lee (yunsup@cs.berkeley.edu)
#
# This makefile will build a rtl simulator and run various tests to
# verify proper functionality.
#

default: all

base_dir = $(abspath ..)
generated_dir = $(abspath ./generated-src)
mem_gen = $(base_dir)/fsim/fpga_mem_gen
sim_dir = .
output_dir = $(sim_dir)/output

BACKEND ?= fpga
CONFIG ?= DefaultFPGAConfig
TB ?= rocketTestHarness

include $(base_dir)/Makefrag
include $(sim_dir)/Makefrag
ifneq ($(MAKECMDGOALS),clean)
-include $(generated_dir)/$(MODEL).$(CONFIG).d
endif
include $(base_dir)/vsim/Makefrag-verilog

all: $(simv)
debug: $(simv_debug)

clean:
	rm -rf $(junk) simv* csrc *.key DVE* *.h *.a *.daidir $(generated_dir)

.PHONY: default all debug clean
