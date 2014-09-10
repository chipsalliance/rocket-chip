all: emulator

base_dir = $(abspath ..)
sim_dir = .
output_dir = $(sim_dir)/output
PROJECT = rocketchip
CONFIG = DefaultCPPConfig

include $(base_dir)/Makefrag

CXXFLAGS := $(CXXFLAGS) -std=c++11 -I$(RISCV)/include

CXXSRCS := emulator mm mm_dramsim2
CXXFLAGS := $(CXXFLAGS) -I$(base_dir)/csrc -I$(base_dir)/dramsim2

LDFLAGS := $(LDFLAGS) -L$(RISCV)/lib -Wl,-rpath,$(RISCV)/lib -L. -ldramsim -lfesvr -lpthread

OBJS := $(addsuffix .o,$(CXXSRCS) $(MODEL))
DEBUG_OBJS := $(addsuffix -debug.o,$(CXXSRCS) $(MODEL))

CHISEL_ARGS := $(MODEL) --noIoDebug --backend c --configInstance $(PROJECT).$(CONFIG) --targetDir emulator/generated-src
CHISEL_ARGS_DEBUG := $(CHISEL_ARGS)-debug --debug --vcd --ioDebug

generated-src/$(MODEL).h: $(chisel_srcs)
	cd $(base_dir) && $(SBT) "project $(PROJECT)" "elaborate $(CHISEL_ARGS)"

generated-src-debug/$(MODEL).h: $(chisel_srcs)
	cd $(base_dir) && $(SBT) "project $(PROJECT)" "elaborate $(CHISEL_ARGS_DEBUG)"

$(MODEL).o: %.o: generated-src/%.h
	$(MAKE) -j $(patsubst %.cpp,%.o,$(shell ls generated-src/$(MODEL)-*.cpp))
	$(LD) -r $(patsubst %.cpp,%.o,$(shell ls generated-src/$(MODEL)-*.cpp)) -o $@

$(MODEL)-debug.o: %-debug.o: generated-src-debug/%.h
	$(MAKE) -j $(patsubst %.cpp,%.o,$(shell ls generated-src-debug/$(MODEL)-*.cpp))
	$(LD) -r $(patsubst %.cpp,%.o,$(shell ls generated-src-debug/$(MODEL)-*.cpp)) -o $@

$(wildcard generated-src/*.o): %.o: %.cpp generated-src/$(MODEL).h
	$(CXX) $(CXXFLAGS) -Igenerated-src -c -o $@ $<

$(wildcard generated-src-debug/*.o): %.o: %.cpp generated-src-debug/$(MODEL).h
	$(CXX) $(CXXFLAGS) -Igenerated-src-debug -c -o $@ $<

$(addsuffix .o,$(CXXSRCS)): %.o: $(base_dir)/csrc/%.cc $(base_dir)/csrc/*.h generated-src/$(MODEL).h
	$(CXX) $(CXXFLAGS) -Igenerated-src -c -o $@ $<

$(addsuffix -debug.o,$(CXXSRCS)): %-debug.o: $(base_dir)/csrc/%.cc $(base_dir)/csrc/*.h generated-src-debug/$(MODEL).h
	$(CXX) $(CXXFLAGS) -Igenerated-src-debug -c -o $@ $<

emulator: $(OBJS) libdramsim.a
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS)

emulator-debug: $(DEBUG_OBJS) libdramsim.a
	$(CXX) $(CXXFLAGS) -o $@ $(DEBUG_OBJS) $(LDFLAGS)
 
clean:
	rm -rf *.o *.a emulator emulator-debug generated-src generated-src-debug DVEfiles $(output_dir)

test:
	cd $(base_dir) && $(SBT) "~make $(CURDIR) run-fast $(CHISEL_ARGS)"

#--------------------------------------------------------------------
# Run assembly tests and benchmarks
#--------------------------------------------------------------------

$(output_dir)/%.run: $(output_dir)/%.hex emulator
	./emulator +dramsim +max-cycles=$(timeout_cycles) +loadmem=$< none 2> /dev/null 2> $@ && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.out: $(output_dir)/%.hex emulator
	./emulator +dramsim +max-cycles=$(timeout_cycles) +verbose +loadmem=$< none $(disasm) $@ && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.vcd: $(output_dir)/%.hex emulator-debug
	./emulator-debug +dramsim +max-cycles=$(timeout_cycles) +verbose -v$@ +loadmem=$< none $(disasm) $(patsubst %.vcd,%.out,$@) && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.vpd: $(output_dir)/%.hex emulator-debug
	rm -rf $@.vcd && mkfifo $@.vcd
	vcd2vpd $@.vcd $@ > /dev/null &
	./emulator-debug +dramsim +max-cycles=$(timeout_cycles) +verbose -v$@.vcd +loadmem=$< none $(disasm) $(patsubst %.vpd,%.out,$@) && [ $$PIPESTATUS -eq 0 ]

run-asm-tests: $(addprefix $(output_dir)/, $(addsuffix .out, $(asm_p_tests) $(asm_v_tests)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $^; echo;
run-bmarks-test: $(addprefix $(output_dir)/, $(addsuffix .out, $(bmarks)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $^; echo;
run-mt-tests: $(addprefix $(output_dir)/, $(addsuffix .out, $(mt_bmarks)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $^; echo;

run-asm-tests-debug: $(addprefix $(output_dir)/, $(addsuffix .vpd, $(asm_p_tests) $(asm_v_tests)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $(patsubst %.vpd,%.out,$^); echo;
run-bmarks-test-debug: $(addprefix $(output_dir)/, $(addsuffix .vpd, $(bmarks)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $(patsubst %.vpd,%.out,$^); echo;
run-mt-tests-debug: $(addprefix $(output_dir)/, $(addsuffix .vpd, $(mt_bmarks)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $(patsubst %.vpd,%.out,$^); echo;

# UCB use only
-include Makefrag-refchip

run: run-asm-tests run-bmarks-test
run-debug: run-asm-tests-debug run-bmarks-test-debug
run-fast: $(addprefix $(output_dir)/, $(addsuffix .run, $(asm_p_tests) $(asm_v_tests) $(bmarks)))

.PHONY: run-asm-tests run-bmarks-test run-mt-tests
.PHONY: run-asm-tests-debug run-bmarks-test-debug run-mt-tests-debug
.PHONY: run run-debug run-fast
