all: emulator

base_dir = ..
sim_dir = .

include $(base_dir)/Makefrag

CXXFLAGS := $(CXXFLAGS) -std=c++11 -I$(RISCV)/include

CXXSRCS := emulator disasm mm mm_dramsim2
CXXFLAGS := $(CXXFLAGS) -I$(base_dir)/csrc -I$(base_dir)/chisel/csrc -I$(base_dir)/dramsim2

LDFLAGS := $(LDFLAGS) -L$(RISCV)/lib -L. -ldramsim -lfesvr -lpthread

OBJS := $(addsuffix .o,$(CXXSRCS) $(MODEL))
DEBUG_OBJS := $(addsuffix -debug.o,$(CXXSRCS) $(MODEL))

CHISEL_ARGS := $(MODEL) --noIoDebug --backend c --targetDir $(base_dir)/emulator/generated-src
CHISEL_ARGS_DEBUG := $(CHISEL_ARGS)-debug --debug --vcd --ioDebug

generated-src/$(MODEL).cpp: $(base_dir)/riscv-rocket/src/*.scala $(base_dir)/riscv-hwacha/src/*.scala $(base_dir)/chisel/src/main/scala/* $(base_dir)/uncore/src/*.scala $(base_dir)/src/*.scala
	cd $(base_dir)/sbt && $(SBT) "project referencechip" "elaborate $(CHISEL_ARGS)"

generated-src-debug/$(MODEL).cpp: $(base_dir)/riscv-rocket/src/*.scala $(base_dir)/riscv-hwacha/src/*.scala $(base_dir)/chisel/src/main/scala/* $(base_dir)/uncore/src/*.scala $(base_dir)/src/*.scala
	cd $(base_dir)/sbt && $(SBT) "project referencechip" "elaborate $(CHISEL_ARGS_DEBUG)"

$(MODEL).o: %.o: generated-src/%.cpp
	$(CXX) $(CXXFLAGS) -Igenerated-src -c -o $@ $<

$(MODEL)-debug.o: %-debug.o: generated-src-debug/%.cpp
	$(CXX) $(CXXFLAGS) -Igenerated-src-debug -c -o $@ $<

$(addsuffix .o,$(CXXSRCS)): %.o: $(base_dir)/csrc/%.cc $(base_dir)/csrc/*.h generated-src/$(MODEL).cpp
	$(CXX) $(CXXFLAGS) -Igenerated-src -c -o $@ $<

$(addsuffix -debug.o,$(CXXSRCS)): %-debug.o: $(base_dir)/csrc/%.cc $(base_dir)/csrc/*.h generated-src-debug/$(MODEL).cpp
	$(CXX) $(CXXFLAGS) -Igenerated-src-debug -c -o $@ $<

emulator: $(OBJS) libdramsim.a
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS)

emulator-debug: $(DEBUG_OBJS) libdramsim.a
	$(CXX) $(CXXFLAGS) -o $@ $(DEBUG_OBJS) $(LDFLAGS)
 
clean:
	rm -rf *.o *.a emulator emulator-debug generated-src generated-src-debug DVEfiles output

test:
	cd $(base_dir)/sbt && $(SBT) "project referencechip" "~make $(CURDIR) run-fast $(CHISEL_ARGS)"

#--------------------------------------------------------------------
# Run assembly tests and benchmarks
#--------------------------------------------------------------------

%.hex:
	$(MAKE) -C $(dir $@) $(notdir $@)

%.riscv.hex: %
	$(MAKE) -C $(dir $@) $(notdir $@)

$(addprefix output/, $(addsuffix .hex, $(asm_p_tests) $(asm_v_tests) $(vecasm_p_tests) $(vecasm_v_tests) $(vecasm_pt_tests))): output/%.hex: $(tstdir)/%.hex
	mkdir -p output
	ln -fs ../$< $@

$(addprefix output/, $(addsuffix .hex, $(bmarks))): output/%.hex: $(bmarkdir)/%.hex
	mkdir -p output
	ln -fs ../$< $@

$(addprefix output/, $(addsuffix .hex, $(mt_bmarks))): output/%.hex: $(mt_bmarkdir)/%.hex
	mkdir -p output
	ln -fs ../$< $@

output:
	mkdir -p $@

output/%.run: output/%.hex emulator
	./emulator +dramsim +max-cycles=30000000 +loadmem=$< none 2> /dev/null

output/%.out: output/%.hex emulator
	./emulator +dramsim +max-cycles=30000000 +verbose +coremap-random +loadmem=$< none 2> $@

output/%.vpd: output/%.hex emulator-debug
	rm -rf $@.vcd && mkfifo $@.vcd
	vcd2vpd $@.vcd $@ > /dev/null &
	./emulator-debug +dramsim +max-cycles=30000000 +verbose -v$@.vcd +coremap-random +loadmem=$< none 2> $(patsubst %.vpd,%.out,$@)

run-asm-tests: $(addprefix output/, $(addsuffix .out, $(asm_p_tests) $(asm_v_tests)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $^; echo;
run-vecasm-tests: $(addprefix output/, $(addsuffix .out, $(vecasm_p_tests) $(vecasm_v_tests)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $^; echo;
run-vecasm-timer-tests: $(addprefix output/, $(addsuffix .out, $(vecasm_pt_tests)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $^; echo;
run-bmarks-test: $(addprefix output/, $(addsuffix .out, $(bmarks)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $^; echo;
run-mt-tests: $(addprefix output/, $(addsuffix .out, $(mt_bmarks)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $^; echo;

run-asm-tests-debug: $(addprefix output/, $(addsuffix .vpd, $(asm_p_tests) $(asm_v_tests)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $(patsubst %.vpd,%.out,$^); echo;
run-vecasm-tests-debug: $(addprefix output/, $(addsuffix .vpd, $(vecasm_p_tests) $(vecasm_v_tests)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $(patsubst %.vpd,%.out,$^); echo;
run-vecasm-timer-tests-debug: $(addprefix output/, $(addsuffix .vpd, $(vecasm_pt_tests)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $(patsubst %.vpd,%.out,$^); echo;
run-bmarks-test-debug: $(addprefix output/, $(addsuffix .vpd, $(bmarks)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $(patsubst %.vpd,%.out,$^); echo;
run-mt-tests-debug: $(addprefix output/, $(addsuffix .vpd, $(mt_bmarks)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $(patsubst %.vpd,%.out,$^); echo;

run: run-asm-tests run-vecasm-tests run-vecasm-timer-tests run-bmarks-test
run-debug: run-asm-tests-debug run-vecasm-tests-debug run-vecasm-timer-tests-debug run-bmarks-test-debug
run-fast: $(addprefix output/, $(addsuffix .run, $(asm_p_tests) $(asm_v_tests) $(bmarks)))
