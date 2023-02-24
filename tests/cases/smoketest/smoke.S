.global _start
_start:
    add t0,t0,x0
    addi t0, x0, 0x1
    addi t1, x0, 0x0
    addi t3, x0, 0xa
loop:
    add t1, t1, t0
    add t0, t0, 0x1
    add t3, t3, -1
    bne t3, x0, loop
    sd t1, 0x10(x0)
    ld t2, 0x10(x0)
    addi x20, x0, 0x1
    addi x21, x0, 0x1
    addi x22, x0, 0x1
    addi x23, x0, 0x1
    addi x24, x0, 0x1
    addi x25, x0, 0x1