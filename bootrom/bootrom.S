.section .text.start, "ax", @progbits
.globl _start
_start:
  la s0, DRAM_BASE
  csrr a0, mhartid
  la a1, _dtb
  jr s0

.section .text.hang, "ax", @progbits
.globl _hang
_hang:
  wfi
  j _hang

.section .rodata.dtb, "a", @progbits
.globl _dtb
.align 5, 0
_dtb:
.ascii "DTB goes here"
