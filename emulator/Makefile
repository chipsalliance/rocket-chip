default: all

base_dir = $(abspath ..)
generated_dir = $(abspath ./generated-src)
generated_src_debug = $(abspath ./generated-src-debug)
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
DEBUG_OBJS := $(addsuffix .debug.o,$(CXXSRCS) $(MODEL).$(CONFIG))

$(MODEL).$(CONFIG).o: %.o: $(generated_dir)/%.h
	$(MAKE) -j $(patsubst %.cpp,%.o,$(shell ls $(generated_dir)/$(MODEL).$(CONFIG)-*.cpp))
	$(LD) -r $(patsubst %.cpp,%.o,$(shell ls $(generated_dir)/$(MODEL).$(CONFIG)-*.cpp)) -o $@

$(MODEL).$(CONFIG).debug.o: %.debug.o: $(generated_src_debug)/%.h
	$(MAKE) -j $(patsubst %.cpp,%.o,$(shell ls $(generated_src_debug)/$(MODEL).$(CONFIG)-*.cpp))
	$(LD) -r $(patsubst %.cpp,%.o,$(shell ls $(generated_src_debug)/$(MODEL).$(CONFIG)-*.cpp)) -o $@

$(generated_dir)/%.o: $(generated_dir)/%.cpp $(generated_dir)/%.h
	$(CXX) $(CXXFLAGS) -I$(generated_dir) -c -o $@ $<

$(generated_src_debug)/%.o: $(generated_src_debug)/%.cpp $(generated_src_debug)/%.h
	$(CXX) $(CXXFLAGS) -I$(generated_src_debug) -c -o $@ $<

$(addsuffix .o,$(CXXSRCS)): %.o: $(base_dir)/csrc/%.cc $(base_dir)/csrc/*.h $(generated_dir)/$(MODEL).$(CONFIG).h
	$(CXX) $(CXXFLAGS) -include $(generated_dir)/$(MODEL).$(CONFIG).h -I$(generated_dir) -c -o $@ $<

$(addsuffix .debug.o,$(CXXSRCS)): %.debug.o: $(base_dir)/csrc/%.cc $(base_dir)/csrc/*.h $(generated_src_debug)/$(MODEL).$(CONFIG).h
	$(CXX) $(CXXFLAGS) -include $(generated_src_debug)/$(MODEL).$(CONFIG).h -I$(generated_src_debug) -c -o $@ $<

$(generated_dir)/$(MODEL).$(CONFIG).d $(generated_dir)/$(MODEL).$(CONFIG).h : $(chisel_srcs)
	cd $(base_dir) && $(SBT) "project $(PROJECT)" "run $(CHISEL_ARGS) --noIoDebug"

$(generated_src_debug)/$(MODEL).$(CONFIG).h : $(chisel_srcs)
	cd $(base_dir) && $(SBT) "project $(PROJECT)" "run $(CHISEL_ARGS)-debug  --debug --vcd --ioDebug"

emu = emulator-$(MODEL)-$(CONFIG)
$(emu): $(generated_dir)/$(MODEL).$(CONFIG).h $(OBJS) libdramsim.a
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS)

emu_debug = emulator-$(MODEL)-$(CONFIG)-debug
$(emu_debug): $(generated_dir)/$(MODEL).$(CONFIG).d $(generated_src_debug)/$(MODEL).$(CONFIG).h $(DEBUG_OBJS) libdramsim.a
	$(CXX) $(CXXFLAGS) -o $@ $(DEBUG_OBJS) $(LDFLAGS)

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
