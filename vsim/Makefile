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
sim_dir = .
output_dir = $(sim_dir)/output

BACKEND ?= v
TB ?= TestDriver

include $(base_dir)/Makefrag
include $(base_dir)/Makefrag.generate_sources
include $(sim_dir)/Makefrag
ifneq ($(filter run% %.run %.out %.vpd %.vcd,$(MAKECMDGOALS)),)
-include $(generated_dir)/$(long_name).d
endif

all: $(simv)
debug: $(simv_debug)

clean:
	rm -rf $(junk) simv* csrc *.key DVE* *.h *.a *.daidir $(generated_dir)

.PHONY: default all debug clean

sim_vsrcs = $(generated_verilog) $(verilog)

sim_csrcs = $(generated_cppfiles)              \
			$(base_dir)/csrc/remote_bitbang.cc \
			$(base_dir)/csrc/SimJTAG.cc

headers = $(generated_headers)                 \
		  $(base_dir)/csrc/remote_bitbang.h
