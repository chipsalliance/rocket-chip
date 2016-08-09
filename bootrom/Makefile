bootrom_img = bootrom.img

GCC=riscv64-unknown-elf-gcc
OBJCOPY=riscv64-unknown-elf-objcopy

all: $(bootrom_img)

%.img: %.elf
	$(OBJCOPY) -O binary --change-addresses=-0x1000 --only-section .text $< $@

%.elf: %.S linker.ld
	$(GCC) -Tlinker.ld $< -nostdlib -static -o $@
