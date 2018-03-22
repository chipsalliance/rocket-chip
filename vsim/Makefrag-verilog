#--------------------------------------------------------------------
# Verilog Generation
#--------------------------------------------------------------------
firrtl = $(generated_dir)/$(long_name).fir
verilog = $(generated_dir)/$(long_name).v

# If I don't mark these as .SECONDARY then make will delete these internal
# files.
.SECONDARY: $(firrtl) $(verilog)

$(generated_dir)/%.fir $(generated_dir)/%.d: $(FIRRTL_JAR) $(chisel_srcs) $(bootrom_img)
	mkdir -p $(dir $@)
	cd $(base_dir) && $(SBT) "runMain $(PROJECT).Generator $(generated_dir) $(PROJECT) $(MODEL) $(CFG_PROJECT) $(CONFIG)"

$(generated_dir)/%.v $(generated_dir)/%.conf: $(generated_dir)/%.fir $(FIRRTL_JAR)
	mkdir -p $(dir $@)
	$(FIRRTL) -i $< -o $(generated_dir)/$*.v -X verilog --infer-rw $(MODEL) --repl-seq-mem -c:$(MODEL):-o:$(generated_dir)/$*.conf -faf $(generated_dir)/$*.anno.json -td $(generated_dir)/$(long_name)/

$(generated_dir)/$(long_name).behav_srams.v : $(generated_dir)/$(long_name).conf $(mem_gen)
	cd $(generated_dir) && \
	rm -f $@ && \
	$(mem_gen) $(generated_dir)/$(long_name).conf >> $@.tmp && \
	mv $@.tmp $@

#--------------------------------------------------------------------
# Run
#--------------------------------------------------------------------
.PRECIOUS: $(output_dir)/%.vpd

$(output_dir)/%.run: $(output_dir)/% $(simv)
	cd $(sim_dir) && $(exec_simv) +permissive +max-cycles=$(timeout_cycles) +permissive-off $< 2> /dev/null 2> $@ && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.out: $(output_dir)/% $(simv)
	cd $(sim_dir) && $(exec_simv) +permissive +verbose +max-cycles=$(timeout_cycles) +permissive-off $< $(disasm) $@ && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.vcd: $(output_dir)/% $(simv_debug)
	cd $(sim_dir) && $(exec_simv_debug) +permissive +verbose +vcdfile=$@ +max-cycles=$(timeout_cycles) +permissive-off $< $(disasm) $(patsubst %.vcd,%.out,$@) && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.vpd: $(output_dir)/% $(simv_debug)
	cd $(sim_dir) && $(exec_simv_debug) +permissive +verbose +vcdplusfile=$@ +max-cycles=$(timeout_cycles) +permissive-off $< $(disasm) $(patsubst %.vpd,%.out,$@) && [ $$PIPESTATUS -eq 0 ]

$(output_dir)/%.saif: $(output_dir)/% $(simv_debug)
	cd $(sim_dir) && rm -f $(output_dir)/pipe-$*.vcd && vcd2saif -input $(output_dir)/pipe-$*.vcd -pipe "$(exec_simv_debug) +permissive +verbose +vcdfile=$(output_dir)/pipe-$*.vcd +max-cycles=$(bmark_timeout_cycles) +permissive-off $<" -output $@ > $(patsubst %.saif,%.out,$@) 2>&1

run: run-asm-tests run-bmark-tests
run-debug: run-asm-tests-debug run-bmark-tests-debug
run-fast: run-asm-tests-fast run-bmark-tests-fast

.PHONY: run-asm-tests run-bmark-tests
.PHONY: run-asm-tests-debug run-bmark-tests-debug
.PHONY: run run-debug run-fast

junk += $(output_dir)
