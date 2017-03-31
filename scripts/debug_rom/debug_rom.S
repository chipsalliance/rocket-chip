// See LICENSE.SiFive for license details.

#include "spike/encoding.h"

// These are implementation-specific addresses in the Debug Module
#define HALTED    0x100
#define GOING     0x104
#define RESUMING  0x108
#define EXCEPTION 0x10C

// Region of memory where each hart has 1
// byte to read.
#define OK_GO 0x400

        .option norvc
        .global entry
        .global exception

        // Entry location on ebreak, Halt, or Breakpoint
        // It is the same for all harts. They branch when 
        // their specific OK_GO bit is set.

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
        // us to do, or whether we should not be halted anymore.
entry_loop:
        csrr s0, CSR_MHARTID
        sw   s0, HALTED(zero)
        lb   s0, OK_GO(s0) // 1 byte flag per hart. Only one hart advances here.
        bne  zero, s0, going
        jal  zero, entry_loop

_exception:
        sw      zero, EXCEPTION(zero) // Let debug module know you got an exception.
        ebreak

going:
        csrr s0, CSR_DSCRATCH          // Restore s0 here
        sw zero, GOING(zero)           // When debug module sees this write, the OK_GO flag is reset.
        jalr zero, zero, %lo(whereto)          // Rocket-Chip has a specific hack which is that jalr in
                                       // Debug Mode will flush the I-Cache. We need that so that the
                                       // remainder of the variable instructions will be what Debug Module
                                       // intends.

_resume:
        csrw CSR_DSCRATCH, s0   // Save s0 to allow signaling MHARTID
        csrr s0, CSR_MHARTID
        sw   s0, RESUMING(zero) // Let the Debug Module know you're not halted anymore.
        csrr s0, CSR_DSCRATCH   // Restore s0
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
