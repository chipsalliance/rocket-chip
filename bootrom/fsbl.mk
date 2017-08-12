GCC=$(RISCV)/bin/riscv64-unknown-elf-gcc
OBJCOPY=$(RISCV)/bin/riscv64-unknown-elf-objcopy

SECOND_STAGE ?= 0x80000000
dtb = $(generated_dir)/$(CONFIG).dtb
FSBL_FLAGS=-T$(base_dir)/bootrom/linker.ld -nostdlib -O2 -nostartfiles -march=rv32imac -mabi=ilp32 -fno-common -DSECOND_STAGE=$(SECOND_STAGE) -static -Wl,--no-gc-sections

# Build the code only (no dtb) for bootroms
$(bootrom_img): $(base_dir)/bootrom/bootrom.S $(base_dir)/bootrom/linker.ld
	$(eval EMPTY_FILE := $(shell mktemp))
	touch $(EMPTY_FILE)
	$(GCC) $(FSBL_FLAGS) -DDEVICE_TREE='"$(EMPTY_FILE)"' $< -o $@.elf
	$(OBJCOPY) -O binary $@.elf $@.bin
	dd if=$@.bin of=$@ bs=128 count=1
	rm $(EMPTY_FILE)


%.dtb: %.dts
	dtc --in-format dts --out-format dtb --out $@ $<

$(generated_dir)/%.fsbl.img: $(generated_dir)/%.fsbl.bin
	dd if=$< of=$@ bs=128 count=1

$(generated_dir)/%.fsbl.hx: $(generated_dir)/%.fsbl.bin
	od -t x4 -An -w4 -v $< > $@

$(generated_dir)/%.fsbl.bin: $(generated_dir)/%.fsbl.elf
	$(OBJCOPY) -O binary $< $@

$(generated_dir)/%.fsbl.elf: $(base_dir)/bootrom/bootrom.S $(base_dir)/bootrom/linker.ld $(dtb)
	$(GCC) $(FSBL_FLAGS) -DDEVICE_TREE='"$(dtb)"' $< -o $@
