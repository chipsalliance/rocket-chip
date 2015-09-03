default: all

base_dir = $(abspath ..)
generated_dir = $(abspath ./generated-src)
sim_dir = .
output_dir = $(sim_dir)/output

BACKEND = c
CONFIG ?= DefaultCPPConfig

include $(base_dir)/Makefrag
-include $(generated_dir)/$(MODEL).$(CONFIG).d

CXXSRCS := emulator mm mm_dramsim2
CXXFLAGS := $(CXXFLAGS) -std=c++11 -I$(RISCV)/include -I$(base_dir)/csrc -I$(base_dir)/dramsim2
LDFLAGS := $(LDFLAGS) -L$(RISCV)/lib -Wl,-rpath,$(RISCV)/lib -L. -ldramsim -lfesvr -lpthread
OBJS := $(addsuffix .o,$(CXXSRCS) $(MODEL).$(CONFIG))

$(MODEL).$(CONFIG).o: %.o: $(generated_dir)/%.h
	$(MAKE) -j $(patsubst %.cpp,%.o,$(shell ls $(generated_dir)/$(MODEL).$(CONFIG)-*.cpp))
	$(LD) -r $(patsubst %.cpp,%.o,$(shell ls $(generated_dir)/$(MODEL).$(CONFIG)-*.cpp)) -o $@

$(generated_dir)/%.o: $(generated_dir)/%.cpp $(generated_dir)/%.h
	$(CXX) $(CXXFLAGS) -I$(generated_dir) -c -o $@ $<

$(addsuffix .o,$(CXXSRCS)): %.o: $(base_dir)/csrc/%.cc $(base_dir)/csrc/*.h $(generated_dir)/$(MODEL).$(CONFIG).h
	$(CXX) $(CXXFLAGS) -include $(generated_dir)/$(MODEL).$(CONFIG).h -I$(generated_dir) -c -o $@ $<

$(generated_dir)/nodebug.d $(generated_dir)/$(MODEL).$(CONFIG).d : $(chisel_srcs)
	rm -f $(generated_dir)/debug.d $(shell ls $(generated_dir)/$(MODEL).$(CONFIG)*)
	cd $(base_dir) && $(SBT) "project $(PROJECT)" "run $(CHISEL_ARGS) --noIoDebug"
	touch $(generated_dir)/nodebug.d

$(generated_dir)/debug.d : $(chisel_srcs)
	rm -f $(generated_dir)/nodebug.d $(shell ls $(generated_dir)/$(MODEL).$(CONFIG)*)
	cd $(base_dir) && $(SBT) "project $(PROJECT)" "run $(CHISEL_ARGS)  --debug --vcd --ioDebug"
	touch $(generated_dir)/debug.d

emu = emulator-$(MODEL)-$(CONFIG)
$(emu): $(generated_dir)/nodebug.d $(OBJS) libdramsim.a
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS)

emu_debug = emulator-$(MODEL)-$(CONFIG)-debug
$(emu_debug): $(generated_dir)/debug.d $(OBJS) libdramsim.a
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS)
 
all: $(emu)
debug: $(emu_debug)

clean:
	rm -rf *.o *.a emulator-* $(generated_dir) DVEfiles $(output_dir)

test:
	cd $(base_dir) && $(SBT) "~make $(CURDIR) run-fast $(CHISEL_ARGS)"

.PHONY: default all debug clean test

#--------------------------------------------------------------------
# Run assembly tests and benchmarks
#--------------------------------------------------------------------

$(output_dir)/%.run: $(output_dir)/%.hex emulator-$(MODEL)-$(CONFIG)
	./$(emu) +dramsim +max-cycles=$(timeout_cycles) +loadmem=$< none 2> /dev/null 2> $@ && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.out: $(output_dir)/%.hex emulator-$(MODEL)-$(CONFIG)
	./$(emu) +dramsim +max-cycles=$(timeout_cycles) +verbose +loadmem=$< none $(disasm) $@ && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.vcd: $(output_dir)/%.hex emulator-$(MODEL)-$(CONFIG)-debug
	./$(emu_debug) +dramsim +max-cycles=$(timeout_cycles) +verbose -v$@ +loadmem=$< none $(disasm) $(patsubst %.vcd,%.out,$@) && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.vpd: $(output_dir)/%.hex emulator-$(MODEL)-$(CONFIG)-debug
	rm -rf $@.vcd && mkfifo $@.vcd
	vcd2vpd $@.vcd $@ > /dev/null &
	./$(emu_debug) +dramsim +max-cycles=$(timeout_cycles) +verbose -v$@.vcd +loadmem=$< none $(disasm) $(patsubst %.vpd,%.out,$@) && [ $$PIPESTATUS -eq 0 ]

run: run-asm-tests run-bmark-tests
run-debug: run-asm-tests-debug run-bmark-tests-debug
run-fast: run-asm-tests-fast run-bmark-tests-fast

.PHONY: run-asm-tests run-bmarks-test
.PHONY: run-asm-tests-debug run-bmark-tests-debug
.PHONY: run run-debug run-fast
