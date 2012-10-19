basedir = ..
include ../Makefrag

CXX := g++
CXXFLAGS := -O1

all: emulator

CXXSRCS := emulator disasm
CXXFLAGS := $(CXXFLAGS) -Itestbench -I$(basedir)/chisel/csrc

DRAMSIM2_PATH := ../dramsim2/
CXXFLAGS := $(CXXFLAGS) -I$(DRAMSIM2_PATH) -L$(DRAMSIM2_PATH) -ldramsim -Wl,-rpath=$(DRAMSIM2_PATH) 
DRAMSIM2_LIB := libdramsim.so

generated-src/$(MODEL).cpp: $(basedir)/riscv-rocket/src/*.scala $(basedir)/riscv-hwacha/src/*.scala $(basedir)/chisel/src/main/scala/* $(basedir)/uncore/src/*.scala
	cd $(basedir)/sbt && $(SBT) "project referencechip" "run ReferenceChip.Top --backend c --noIoDebug --targetDir ../emulator/generated-src"

generated-src-debug/$(MODEL).cpp: $(basedir)/riscv-rocket/src/*.scala $(basedir)/riscv-hwacha/src/*.scala $(basedir)/chisel/src/main/scala/* $(basedir)/uncore/src/*.scala
	cd $(basedir)/sbt && $(SBT) "project referencechip" "run ReferenceChip.Top --backend c --debug --vcd --targetDir ../emulator/generated-src-debug"

$(MODEL).o: %.o: generated-src/%.cpp
	$(CXX) $(CXXFLAGS) -Igenerated-src -c -o $@ $<

$(MODEL)-debug.o: %-debug.o: generated-src-debug/%.cpp
	$(CXX) $(CXXFLAGS) -Igenerated-src-debug -c -o $@ $<

$(addsuffix .o,$(CXXSRCS)): %.o: testbench/%.cc testbench/* generated-src/$(MODEL).cpp
	$(CXX) $(CXXFLAGS) -Igenerated-src -c -o $@ $<

$(addsuffix -debug.o,$(CXXSRCS)): %-debug.o: testbench/%.cc testbench/* generated-src-debug/$(MODEL).cpp
	$(CXX) $(CXXFLAGS) -Igenerated-src-debug -c -o $@ $<

emulator: $(addsuffix .o,$(CXXSRCS)) $(MODEL).o
	$(CXX) $(CXXFLAGS) -o $@ $^

emulator-debug: $(addsuffix -debug.o,$(CXXSRCS)) $(MODEL)-debug.o
	$(CXX) $(CXXFLAGS) -o $@ $^

dramsim2:
	cd $(DRAMSIM2_PATH) ; make $(DRAMSIM2_LIB)

all: emulator
 
clean:
	rm -rf *.o emulator emulator-debug generated-src generated-src-debug DVEfiles

#--------------------------------------------------------------------
# Run assembly tests and benchmarks
#--------------------------------------------------------------------

global_asm_tests_out = $(addsuffix .out, $(global_asm_tests) $(global_asm_vm_tests))
global_vecasm_tests_out = $(addsuffix .out, $(global_vecasm_tests) $(global_vecasm_vm_tests))
global_vecasm_timer_tests_out = $(addsuffix .out, $(global_vecasm_timer_tests))
global_bmarks_out = $(addsuffix .out, $(global_bmarks))
global_asm_tests_vpd = $(addsuffix .vpd, $(global_asm_tests))
global_vecasm_tests_vpd = $(addsuffix .vpd, $(global_vecasm_tests))
global_vecasm_timer_tests_vpd = $(addsuffix .vpd, $(global_vecasm_timer_tests))
global_bmarks_vpd = $(addsuffix .vpd, $(global_bmarks))

$(global_asm_tests_out): %.out: $(global_tstdir)/%.hex emulator
	fesvr -c -testrun -m2000000 -l +loadmem=$< none 2> $@

$(global_vecasm_tests_out): %.out: $(global_tstdir)/%.hex emulator
	fesvr -c -testrun -m2000000 -l +loadmem=$< none 2> $@

$(global_vecasm_timer_tests_out): %.out: $(global_tstdir)/%.hex emulator
	fesvr -c -testrun -m3000000 -l +loadmem=$< none 2> $@

$(global_bmarks_out): %.out: $(global_bmarkdir)/%.hex emulator
	fesvr -c -testrun -m1000000 -l +loadmem=$< none 2> $@

$(global_asm_tests_vpd): %.vpd: $(global_tstdir)/%.hex emulator-debug
	fesvr -c./emulator-debug -testrun -m1000000 -l -v- +loadmem=$< none 2> $(patsubst %.vpd,%.out,$@) | vcd2vpd - $@ > /dev/null

$(global_vecasm_tests_vpd): %.vpd: $(global_tstdir)/%.hex emulator-debug
	fesvr -c./emulator-debug -testrun -m1000000 -l -v- +loadmem=$< none 2> $(patsubst %.vpd,%.out,$@) | vcd2vpd - $@ > /dev/null

$(global_vecasm_timer_tests_vpd): %.vpd: $(global_tstdir)/%.hex emulator-debug
	fesvr -c./emulator-debug -testrun -m3000000 -l -v- +loadmem=$< none 2> $(patsubst %.vpd,%.out,$@) | vcd2vpd - $@ > /dev/null

$(global_bmarks_vpd): %.vpd: $(global_bmarkdir)/%.hex emulator-debug
	fesvr -c./emulator-debug -testrun -m1000000 -l -v- +loadmem=$< none 2> $(patsubst %.vpd,%.out,$@) | vcd2vpd - $@ > /dev/null

run-asm-tests: $(global_asm_tests_out)
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' \
          $(global_asm_tests_out); echo;

run-vecasm-tests: $(global_vecasm_tests_out)
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' \
	       $(global_vecasm_tests_out); echo;

run-vecasm-timer-tests: $(global_vecasm_timer_tests_out)
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' \
	       $(global_vecasm_timer_tests_out); echo;

run-bmarks-test: $(global_bmarks_out)
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' \
	       $(global_bmarks_out); echo;

run-asm-tests-debug: $(global_asm_tests_vpd)
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' \
          $(global_asm_tests_out); echo;

run-vecasm-tests-debug: $(global_vecasm_tests_vpd)
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' \
          $(global_vecasm_tests_out); echo;

run-vecasm-timer-tests-debug: $(global_vecasm_timer_tests_vpd)
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' \
          $(global_vecasm_timer_tests_out); echo;

run-bmarks-test-debug: $(global_bmarks_vpd)
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' \
	       $(global_bmarks_out); echo;

run: run-asm-tests run-vecasm-tests run-vecasm-timer-tests run-bmarks-test
run-debug: run-asm-tests-debug run-vecasm-tests-debug run-vecasm-timer-tests-debug run-bmarks-test-debug
