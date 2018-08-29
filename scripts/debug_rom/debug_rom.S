// See LICENSE.SiFive for license details.

#include "spike/encoding.h"

// These are implementation-specific addresses in the Debug Module
#define HALTED    0x100
#define GOING     0x104
#define RESUMING  0x108
#define EXCEPTION 0x10C

// Region of memory where each hart has 1
// byte to read.
#define FLAGS 0x400
#define FLAG_GO     0
#define FLAG_RESUME 1

        .option norvc
        .global entry
        .global exception

        // Entry location on ebreak, Halt, or Breakpoint
        // It is the same for all harts. They branch when 
        // their GO or RESUME bit is set.

entry:
       jal zero, _entry
resume:
       jal zero, _resume
exception:
       jal zero, _exception

_entry:
        // This fence is required because the execution may have written something
        // into the Abstract Data or Program Buffer registers.
        fence
        csrw CSR_DSCRATCH, s0  // Save s0 to allow signaling MHARTID

        // We continue to let the hart know that we are halted in order that
        // a DM which was reset is still made aware that a hart is halted.
        // We keep checking both whether there is something the debugger wants
        // us to do, or whether we should resume.
entry_loop:
        csrr s0, CSR_MHARTID
        sw   s0, HALTED(zero)
        lbu  s0, FLAGS(s0) // 1 byte flag per hart. Only one hart advances here.
        andi s0, s0, (1 << FLAG_GO) | (1 << FLAG_RESUME)
        beqz s0, entry_loop            // Loop until either GO or RESUME is set.

        andi s0, s0, (1 << FLAG_GO)
        beqz s0, _resume               // If GO is clear at this point, RESUME must be set.

        csrr s0, CSR_DSCRATCH          // Restore s0 here
        sw zero, GOING(zero)           // When debug module sees this write, the GO flag is reset.
        jalr zero, zero, %lo(whereto)          // Rocket-Chip has a specific hack which is that jalr in
                                       // Debug Mode will flush the I-Cache. We need that so that the
                                       // remainder of the variable instructions will be what Debug Module
                                       // intends.
_resume:
        csrr s0, CSR_MHARTID
        sw   s0, RESUMING(zero) // When Debug Module sees this write, the RESUME flag is reset.
        csrr s0, CSR_DSCRATCH   // Restore s0
        dret

_exception:
        sw      zero, EXCEPTION(zero) // Let debug module know you got an exception.
        ebreak

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
