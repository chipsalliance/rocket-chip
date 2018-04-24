/*
 * VPI register
 * -----------------------------------------------------------------------
 * Jimmy Situ (web@jimmystone.cn)
 */
#include <vpi_user.h>
#include <sv_vpi_user.h>


/*
 * This is a table of register functions. This table is the external
 * symbol that the simulator looks for when loading this .vpi module.
 */
extern void debug_tick_register();
extern void jtag_tick_register();

void (*vlog_startup_routines[])(void) = {
      debug_tick_register,
      jtag_tick_register,
      0
};
