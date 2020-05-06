default: all

base_dir = $(abspath ..)
generated_dir = $(abspath ./generated-src)
generated_dir_debug = $(abspath ./generated-src-debug)
sim_dir = .
output_dir = $(sim_dir)/output

include $(base_dir)/Makefrag

CXXSRCS := emulator SimDTM SimJTAG remote_bitbang
CXXFLAGS := $(CXXFLAGS) -std=c++11 -I$(RISCV)/include
LDFLAGS := $(LDFLAGS) -L$(RISCV)/lib -Wl,-rpath,$(RISCV)/lib -L$(abspath $(sim_dir)) -lfesvr -lpthread

emu = emulator-$(PROJECT)-$(CONFIG)
emu_debug = emulator-$(PROJECT)-$(CONFIG)-debug

include $(sim_dir)/Makefrag-verilator

all: $(emu)
debug: $(emu_debug)
verilog: $(verilog)
firrtl: $(firrtl)

clean:
	rm -rf *.o *.a emulator-* $(generated_dir) $(generated_dir_debug) DVEfiles $(output_dir)

.PHONY: default all debug clean

#--------------------------------------------------------------------
# Run assembly tests and benchmarks
#--------------------------------------------------------------------

ifneq ($(filter run% %.run %.out %.vpd %.vcd,$(MAKECMDGOALS)),)
-include $(generated_dir)/$(long_name).d
endif

$(output_dir)/%.run: $(output_dir)/% $(emu)
	./$(emu) +max-cycles=$(timeout_cycles) $< 2> /dev/null 2> $@ && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.out: $(output_dir)/% $(emu)
	./$(emu) +max-cycles=$(timeout_cycles) +verbose $< $(disasm) $@ && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.vcd: $(output_dir)/% $(emu_debug)
	./$(emu_debug) +max-cycles=$(timeout_cycles) +verbose -v$@ $< $(disasm) $(patsubst %.vcd,%.out,$@) && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.vpd: $(output_dir)/% $(emu_debug)
	rm -rf $@.vcd && mkfifo $@.vcd
	vcd2vpd $@.vcd $@ > /dev/null &
	./$(emu_debug) +max-cycles=$(timeout_cycles) +verbose -v$@.vcd $< $(disasm) $(patsubst %.vpd,%.out,$@) && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.fst: $(output_dir)/% $(emu_debug)
	rm -rf $@.vcd && mkfifo $@.vcd
	vcd2fst -Z $@.vcd $@ &
	./$(emu_debug) +max-cycles=$(timeout_cycles) +verbose -v$@.vcd $< $(disasm) $(patsubst %.fst,%.out,$@) && [ $$PIPESTATUS -eq 0 ]

run: run-asm-tests run-bmark-tests
run-debug: run-asm-tests-debug run-bmark-tests-debug
run-fast: run-asm-tests-fast run-bmark-tests-fast

.PHONY: run-asm-tests run-bmark-tests
.PHONY: run-asm-tests-debug run-bmark-tests-debug
.PHONY: run run-debug run-fast
