base_dir = $(abspath ..)
sim_dir = .
output_dir = $(sim_dir)/output
PROJECT = rocketchip
CONFIG ?= DefaultCPPConfig
SHELL=/bin/bash


all: emulator-$(CONFIG)
debug: emulator-$(CONFIG)-debug

include $(base_dir)/Makefrag
-include generated-src/Makefrag-tests.$(CONFIG)

CXXFLAGS := $(CXXFLAGS) -std=c++11 -I$(RISCV)/include

CXXSRCS := emulator mm mm_dramsim2
CXXFLAGS := $(CXXFLAGS) -I$(base_dir)/csrc -I$(base_dir)/dramsim2

LDFLAGS := $(LDFLAGS) -L$(RISCV)/lib -Wl,-rpath,$(RISCV)/lib -L. -ldramsim -lfesvr -lpthread

OBJS := $(addsuffix .o,$(CXXSRCS) $(MODEL).$(CONFIG))
DEBUG_OBJS := $(addsuffix .debug.o,$(CXXSRCS) $(MODEL).$(CONFIG))

CHISEL_ARGS := $(MODEL) --W0W --noIoDebug --backend c --configInstance $(PROJECT).$(CONFIG) --compileInitializationUnoptimized --targetDir emulator/generated-src
CHISEL_ARGS_DEBUG := $(CHISEL_ARGS)-debug --debug --vcd --ioDebug

generated-src/$(MODEL).$(CONFIG).h: $(chisel_srcs)
	cd $(base_dir) && $(SBT) "project $(PROJECT)" "run $(CHISEL_ARGS)"

generated-src-debug/$(MODEL).$(CONFIG).h: $(chisel_srcs)
	cd $(base_dir) && $(SBT) "project $(PROJECT)" "run $(CHISEL_ARGS_DEBUG)"

$(MODEL).$(CONFIG).o: %.o: generated-src/%.h
	$(MAKE) -j $(patsubst %.cpp,%.o,$(shell ls generated-src/$(MODEL).$(CONFIG)-*.cpp))
	$(LD) -r $(patsubst %.cpp,%.o,$(shell ls generated-src/$(MODEL).$(CONFIG)-*.cpp)) -o $@

$(MODEL).$(CONFIG).debug.o: %.debug.o: generated-src-debug/%.h
	$(MAKE) -j $(patsubst %.cpp,%.o,$(shell ls generated-src-debug/$(MODEL).$(CONFIG)-*.cpp))
	$(LD) -r $(patsubst %.cpp,%.o,$(shell ls generated-src-debug/$(MODEL).$(CONFIG)-*.cpp)) -o $@

generated-src/%.o: generated-src/%.cpp generated-src/$(MODEL).$(CONFIG).h
	$(CXX) $(CXXFLAGS) -Igenerated-src -c -o $@ $<

generated-src-debug/%.o: generated-src-debug/%.cpp generated-src-debug/$(MODEL).$(CONFIG).h
	$(CXX) $(CXXFLAGS) -Igenerated-src-debug -c -o $@ $<

$(addsuffix .o,$(CXXSRCS)): %.o: $(base_dir)/csrc/%.cc $(base_dir)/csrc/*.h generated-src/$(MODEL).$(CONFIG).h
	$(CXX) $(CXXFLAGS) -include generated-src/$(MODEL).$(CONFIG).h -Igenerated-src -c -o $@ $<

$(addsuffix .debug.o,$(CXXSRCS)): %.debug.o: $(base_dir)/csrc/%.cc $(base_dir)/csrc/*.h generated-src-debug/$(MODEL).$(CONFIG).h
	$(CXX) $(CXXFLAGS) -include generated-src-debug/$(MODEL).$(CONFIG).h -Igenerated-src-debug -c -o $@ $<

emulator-$(CONFIG): $(OBJS) libdramsim.a
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS)

emulator-$(CONFIG)-debug: $(DEBUG_OBJS) libdramsim.a
	$(CXX) $(CXXFLAGS) -o $@ $(DEBUG_OBJS) $(LDFLAGS)
 
clean:
	rm -rf *.o *.a emulator-* emulator-*-debug generated-src generated-src-debug DVEfiles $(output_dir)

test:
	cd $(base_dir) && $(SBT) "~make $(CURDIR) run-fast $(CHISEL_ARGS)"

#--------------------------------------------------------------------
# Run assembly tests and benchmarks
#--------------------------------------------------------------------

$(output_dir)/%.run: $(output_dir)/%.hex emulator-$(CONFIG)
	./emulator-$(CONFIG) +dramsim +max-cycles=$(timeout_cycles) +loadmem=$< none 2> /dev/null 2> $@ && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.out: $(output_dir)/%.hex emulator-$(CONFIG)
	./emulator-$(CONFIG) +dramsim +max-cycles=$(timeout_cycles) +verbose +loadmem=$< none $(disasm) $@ && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.vcd: $(output_dir)/%.hex emulator-$(CONFIG)-debug
	./emulator-$(CONFIG)-debug +dramsim +max-cycles=$(timeout_cycles) +verbose -v$@ +loadmem=$< none $(disasm) $(patsubst %.vcd,%.out,$@) && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.vpd: $(output_dir)/%.hex emulator-$(CONFIG)-debug
	rm -rf $@.vcd && mkfifo $@.vcd
	vcd2vpd $@.vcd $@ > /dev/null &
	./emulator-$(CONFIG)-debug +dramsim +max-cycles=$(timeout_cycles) +verbose -v$@.vcd +loadmem=$< none $(disasm) $(patsubst %.vpd,%.out,$@) && [ $$PIPESTATUS -eq 0 ]

run: run-asm-tests run-bmark-tests
run-debug: run-asm-tests-debug run-bmark-tests-debug
run-fast: run-asm-tests-fast run-bmark-tests-fast

.PHONY: run-asm-tests run-bmarks-test
.PHONY: run-asm-tests-debug run-bmark-tests-debug
.PHONY: run run-debug run-fast
