// See LICENSE.SiFive for license details.

#include "riscv-pk/encoding.h"

#ifndef CSR_DSCRATCH1
#define CSR_DSCRATCH1 (CSR_DSCRATCH0 + 1)
#endif
  
// These are implementation-specific addresses in the Debug Module, relative to entry
#define HALTED    -0x700  // 0x100
#define GOING     -0x6FC  // 0x104
#define RESUMING  -0x6F8  // 0x108
#define EXCEPTION -0x6F4  // 0x10C

// Region of memory where each hart has 1 byte to read.
#define FLAGS     -0x400  // 0x400
#define FLAG_GO     0
#define FLAG_RESUME 1

        .option norvc
        .global entry
        .global exception

        // Entry location on ebreak, Halt, or Breakpoint
        // It is the same for all harts. They branch when 
        // their GO or RESUME bit is set.

// 0x800
entry:
        csrw  CSR_DSCRATCH1, s1        // s1=s1, ds1=s1: Save s1 to use as base pointer
        j     _entry

// 0x808
exception:
        csrrw s1, CSR_DSCRATCH1, s1    // s1=base, ds1=s1
        sw    zero, EXCEPTION(s1)      // Let debug module know you got an exception.
  
_entry:
        // This fence is required because the execution may have written something
        // into the Abstract Data or Program Buffer registers.
        fence
        csrw  CSR_DSCRATCH0, s0        // Save s0 to allow signaling MHARTID
        la    s1, entry                // s1 = base (0x800 into debug region)

        // We continue to let the hart know that we are halted in order that
        // a DM which was reset is still made aware that a hart is halted.
        // We keep checking both whether there is something the debugger wants
        // us to do, or whether we should resume.
entry_loop:
        csrr  s0, CSR_MHARTID
        sw    s0, HALTED(s1)
        add   s0, s0, s1               // s0=base + hartid
        lbu   s0, FLAGS(s0)            // 1 byte flag per hart. Only one hart advances here.
        andi  s0, s0, (1 << FLAG_GO) | (1 << FLAG_RESUME)
        beqz  s0, entry_loop           // Loop until either GO or RESUME is set.

        andi  s0, s0, (1 << FLAG_GO)
        beqz  s0, _resume              // If GO is clear at this point, RESUME must be set.

        csrr  s0, CSR_DSCRATCH0        // Restore s0 here

//TODO: 0x48 is a hack for (icache-entry) constant
        jalr  zero, 0x48(s1)           // Rocket-Chip has a specific hack which is that jalr in
                                       // Debug Mode will flush the I-Cache. We need that so that the
                                       // remainder of the variable instructions will be what Debug Module
                                       // intends.
icache:
        sw    zero, GOING(s1)          // When debug module sees this write, the GO flag is reset.
        csrrw s1, CSR_DSCRATCH1, s1    // s1=s1, ds1=base
        j     whereto

_resume:
        csrr  s0, CSR_MHARTID
        sw    s0, RESUMING(s1)         // When Debug Module sees this write, the RESUME flag is reset.
        csrr  s0, CSR_DSCRATCH0        // Restore s0
        csrr  s1, CSR_DSCRATCH1        // Restore s1
        dret

        // END OF ACTUAL "ROM" CONTENTS. BELOW IS JUST FOR LINKER SCRIPT.

.section .whereto
whereto:
        nop
        // Variable "ROM" This is : jal x0 abstract, jal x0 program_buffer,
        //                or jal x0 resume, as desired.
        //                Debug Module state machine tracks what is 'desired'.
        //                We don't need/want to use jalr here because all of the
        //                Variable ROM contents are set by
        //                Debug Module before setting the OK_GO byte.
