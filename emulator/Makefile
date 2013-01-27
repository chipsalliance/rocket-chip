all: emulator

basedir = ..
include ../Makefrag

CXX := g++
CXXFLAGS := $(CXXFLAGS) -O1 -std=c++0x -I$(RISCV)/include

CXXSRCS := emulator disasm mm mm_dramsim2
CXXFLAGS := $(CXXFLAGS) -I$(basedir)/csrc -I$(basedir)/chisel/csrc -I$(basedir)/dramsim2

LDFLAGS := $(LDFLAGS) -L$(RISCV)/lib -L. -ldramsim -lfesvr -lpthread

OBJS := $(addsuffix .o,$(CXXSRCS) $(MODEL))
DEBUG_OBJS := $(addsuffix -debug.o,$(CXXSRCS) $(MODEL))

CHISEL_ARGS := $(MODEL) --noIoDebug --backend c --targetDir $(basedir)/emulator/generated-src
CHISEL_ARGS_DEBUG := $(CHISEL_ARGS)-debug --debug --vcd --ioDebug

generated-src/$(MODEL).cpp: $(basedir)/riscv-rocket/src/*.scala $(basedir)/riscv-hwacha/src/*.scala $(basedir)/chisel/src/main/scala/* $(basedir)/uncore/src/*.scala $(basedir)/src/*.scala
	cd $(basedir)/sbt && $(SBT) "project referencechip" "elaborate $(CHISEL_ARGS)"

generated-src-debug/$(MODEL).cpp: $(basedir)/riscv-rocket/src/*.scala $(basedir)/riscv-hwacha/src/*.scala $(basedir)/chisel/src/main/scala/* $(basedir)/uncore/src/*.scala $(basedir)/src/*.scala
	cd $(basedir)/sbt && $(SBT) "project referencechip" "elaborate $(CHISEL_ARGS_DEBUG)"

$(MODEL).o: %.o: generated-src/%.cpp
	$(CXX) $(CXXFLAGS) -Igenerated-src -c -o $@ $<

$(MODEL)-debug.o: %-debug.o: generated-src-debug/%.cpp
	$(CXX) $(CXXFLAGS) -Igenerated-src-debug -c -o $@ $<

$(addsuffix .o,$(CXXSRCS)): %.o: $(basedir)/csrc/%.cc $(basedir)/csrc/*.h generated-src/$(MODEL).cpp
	$(CXX) $(CXXFLAGS) -Igenerated-src -c -o $@ $<

$(addsuffix -debug.o,$(CXXSRCS)): %-debug.o: $(basedir)/csrc/%.cc $(basedir)/csrc/*.h generated-src-debug/$(MODEL).cpp
	$(CXX) $(CXXFLAGS) -Igenerated-src-debug -c -o $@ $<

emulator: $(OBJS) libdramsim.a
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS)

emulator-debug: $(DEBUG_OBJS) libdramsim.a
	$(CXX) $(CXXFLAGS) -o $@ $(DEBUG_OBJS) $(LDFLAGS)
 
clean:
	rm -rf *.o *.a emulator emulator-debug generated-src generated-src-debug DVEfiles output

test:
	cd $(basedir)/sbt && $(SBT) "project referencechip" "~make $(CURDIR) run-fast $(CHISEL_ARGS)"

#--------------------------------------------------------------------
# Run assembly tests and benchmarks
#--------------------------------------------------------------------

%.hex:
	$(MAKE) -C $(dir $@) $(notdir $@)

%.riscv.hex: %
	$(MAKE) -C $(dir $@) $(notdir $@)

$(addprefix output/, $(addsuffix .hex, $(global_asm_tests) $(global_asm_vm_tests) $(global_vecasm_tests) $(global_vecasm_vm_tests) $(global_vecasm_timer_tests))): output/%.hex: $(global_tstdir)/%.hex
	mkdir -p output
	ln -fs ../$< $@

$(addprefix output/, $(addsuffix .hex, $(global_bmarks))): output/%.hex: $(global_bmarkdir)/%.hex
	mkdir -p output
	ln -fs ../$< $@

output:
	mkdir -p $@

output/%.run: output/%.hex emulator
	./emulator +dramsim +max-cycles=3000000 +coremap-random +loadmem=$< none 2> /dev/null

output/%.out: output/%.hex emulator
	./emulator +dramsim +max-cycles=3000000 +verbose +coremap-random +loadmem=$< none 2> $@

output/%.vpd: output/%.hex emulator-debug
	./emulator-debug +dramsim +max-cycles=3000000 +verbose -v- +coremap-random +loadmem=$< none 2> $(patsubst %.vpd,%.out,$@) | vcd2vpd - $@ > /dev/null && [ $$PIPESTATUS -eq 0 ]

run-asm-tests: $(addprefix output/, $(addsuffix .out, $(global_asm_tests) $(global_asm_vm_tests)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $^; echo;
run-vecasm-tests: $(addprefix output/, $(addsuffix .out, $(global_vecasm_tests) $(global_vecasm_vm_tests)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $^; echo;
run-vecasm-timer-tests: $(addprefix output/, $(addsuffix .out, $(global_vecasm_timer_tests)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $^; echo;
run-bmarks-test: $(addprefix output/, $(addsuffix .out, $(global_bmarks)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $^; echo;

run-asm-tests-debug: $(addprefix output/, $(addsuffix .vpd, $(global_asm_tests) $(global_asm_vm_tests)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $(patsubst %.vpd,%.out,$^); echo;
run-vecasm-tests-debug: $(addprefix output/, $(addsuffix .vpd, $(global_vecasm_tests) $(global_vecasm_vm_tests)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $(patsubst %.vpd,%.out,$^); echo;
run-vecasm-timer-tests-debug: $(addprefix output/, $(addsuffix .vpd, $(global_vecasm_timer_tests)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $(patsubst %.vpd,%.out,$^); echo;
run-bmarks-test-debug: $(addprefix output/, $(addsuffix .vpd, $(global_bmarks)))
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' $(patsubst %.vpd,%.out,$^); echo;

run: run-asm-tests run-vecasm-tests run-vecasm-timer-tests run-bmarks-test
run-debug: run-asm-tests-debug run-vecasm-tests-debug run-vecasm-timer-tests-debug run-bmarks-test-debug
run-fast: $(addprefix output/, $(addsuffix .run, $(global_asm_tests) $(global_asm_vm_tests) $(global_bmarks)))
