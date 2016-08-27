.text
.global _start
_start:
  // This boot ROM doesn't know about any boot devices, so it just spins,
  // waiting for the debugger to load a program and change the PC.
  j _start // reset vector
  .word 0 // reserved
  .word 0 // reserved
  .word 0 // pointer to config string
  .word 0 // default trap vector
  .word 0
  .word 0
  .word 0
