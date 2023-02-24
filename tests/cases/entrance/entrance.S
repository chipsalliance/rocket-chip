.global _start
_start:
    auipc t1, 0x0
    addi a1, t1, 32
    csrr a0, mhartid
    lui t0, 0x80000
    slli t0,t0,32
    srli t0,t0,32
    jr t0