SECTIONS
{
    DRAM_BASE = 0x80000000;
    ROM_BASE = 0x1000;

    . = ROM_BASE;
    .text.start : { *(.text.start) }
    . = ROM_BASE + 0x40;
    .text.hang : { *(.text.hang) }
    . = ROM_BASE + 0x80;
    .rodata.dtb : { *(.rodata.dtb) }
}
