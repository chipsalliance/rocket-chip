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
mem_gen = $(VLSI_MEM_GEN)
rom_gen = $(VLSI_ROM_GEN)
sim_dir = .
output_dir = $(sim_dir)/output

BACKEND ?= v
TB ?= TestDriver

include $(base_dir)/Makefrag
include $(sim_dir)/Makefrag
ifeq ($(filter clean verilog,$(MAKECMDGOALS)),)
ifeq ($(findstring n,$(MAKEFLAGS)),)
-include $(generated_dir)/$(long_name).d
endif
endif
include $(base_dir)/bootrom/fsbl.mk
include $(base_dir)/vsim/Makefrag-verilog

all: $(simv)
debug: $(simv_debug)

clean:
	rm -rf $(junk) simv* csrc *.key DVE* *.h *.a *.daidir $(generated_dir)

.PHONY: default all debug clean
