default: all

base_dir = $(abspath ..)
generated_dir = $(abspath ./generated-src)
mem_gen = $(VLSI_MEM_GEN)    # VLSI_MEM_GEN is set in rocket-chip/Makefrag
sim_dir = .
output_dir = $(sim_dir)/output
RTL_HOME = $(abspath ../../)
ROCKET_HOME = $(abspath ../xm_run)

BACKEND ?= v	

include $(base_dir)/Makefrag
include $(sim_dir)/Makefrag
ifneq ($(filter run% %.run %.out %.vpd %.vcd,$(MAKECMDGOALS)),)  
-include $(generated_dir)/$(long_name).d
endif
include $(ROCKET_HOME)/Makefrag-verilog


all: $(xrun)
debug: $(xrun_debug)

clean:
	rm -rf $(junk) *.log csrc *.h *.a $(generated_dir)


 .PHONY: default all debug clean






