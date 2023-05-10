import "DPI-C" function void debug_rob_push_trace(input int     hartid,
                                                  input bit     should_wb,
                                                  input bit     has_wb,
                                                  input longint wb_tag,
                                                  input bit     trace_valid,
                                                  input longint trace_iaddr,
                                                  input longint trace_insn,
                                                  input int     trace_priv,
                                                  input bit     trace_exception,
                                                  input bit     trace_interrupt,
                                                  input longint trace_cause,
                                                  input longint trace_tval,
                                                  input longint trace_wdata[8]);

import "DPI-C" function void debug_rob_push_wb(input int     hartid,
                                               input bit     valid,
                                               input longint wb_tag,
                                               input longint wb_data[8]);

import "DPI-C" function void debug_rob_pop_trace(input int      hartid,
                                                 output bit     trace_valid,
                                                 output longint trace_iaddr,
                                                 output longint trace_insn,
                                                 output int     trace_priv,
                                                 output bit     trace_exception,
                                                 output bit     trace_interrupt,
                                                 output longint trace_cause,
                                                 output longint trace_tval,
                                                 output longint trace_wdata[8]);


module DebugROBPushTrace (
                          input		clock,
                          input		reset,
                          input [31:0]	hartid,
                          input		should_wb,
                          input		has_wb,
                          input [63:0]	wb_tag,
                          input		trace_valid,
                          input [63:0]	trace_iaddr,
                          input [63:0]	trace_insn,
                          input [2:0]	trace_priv,
                          input		trace_exception,
                          input		trace_interrupt,
                          input [63:0]	trace_cause,
                          input [63:0]	trace_tval,
                          input [511:0]	trace_wdata);

   longint __trace_wdata[8];
   genvar  i;

   for (i = 0; i < 8; i = i + 1)
     assign __trace_wdata[i] = trace_wdata[(i+1)*64-1:i*64];

   always @(posedge clock) begin
      if (!reset) begin
         debug_rob_push_trace(hartid,
                              should_wb, has_wb, wb_tag,
                              trace_valid, trace_iaddr, trace_insn,
                              trace_priv, trace_exception, trace_interrupt,
                              trace_cause, trace_tval, __trace_wdata);
      end
   end
endmodule; // DebugROBPushTrace

module DebugROBPushWb (
                       input         clock,
                       input         reset,
                       input [31:0]  hartid,
                       input         valid,
                       input [63:0]  wb_tag,
                       input [511:0] wb_data);

   longint __wb_data[8];
   genvar  i;

   for (i = 0; i < 8; i = i + 1)
     assign __wb_data[i] = wb_data[(i+1)*64-1:i*64];

   always @(posedge clock) begin
      if (!reset) begin
         debug_rob_push_wb(hartid, valid, wb_tag, __wb_data);
      end
   end
endmodule; // DebugROBPushWb

module DebugROBPopTrace (
                         input		clock,
                         input		reset,
                         input [31:0]	hartid,
                         output		trace_valid,
                         output [63:0]	trace_iaddr,
                         output [63:0]	trace_insn,
                         output [2:0]	trace_priv,
                         output		trace_exception,
                         output		trace_interrupt,
                         output [63:0]	trace_cause,
                         output [63:0]	trace_tval,
                         output [511:0]	trace_wdata);

   bit					r_reset;

   bit					__trace_valid;
   longint				__trace_iaddr;
   longint				__trace_insn;
   int					__trace_priv;
   bit					__trace_exception;
   bit					__trace_interrupt;
   longint				__trace_cause;
   longint				__trace_tval;
   longint				__trace_wdata[8];

   reg					__trace_valid_reg;
   reg [63:0]				__trace_iaddr_reg;
   reg [63:0]				__trace_insn_reg;
   reg [2:0]				__trace_priv_reg;
   reg					__trace_exception_reg;
   reg					__trace_interrupt_reg;
   reg [63:0]				__trace_cause_reg;
   reg [63:0]				__trace_tval_reg;
   reg [511:0]				__trace_wdata_reg;

   always @(posedge clock) begin
      __trace_valid_reg <= __trace_valid;
      __trace_iaddr_reg <= __trace_iaddr;
      __trace_insn_reg <= __trace_insn;
      __trace_priv_reg <= __trace_priv;
      __trace_exception_reg <= __trace_exception;
      __trace_interrupt_reg <= __trace_interrupt;
      __trace_cause_reg <= __trace_cause;
      __trace_tval_reg <= __trace_tval;
      __trace_wdata_reg <= {__trace_wdata[7], __trace_wdata[6], __trace_wdata[5], __trace_wdata[4],
                            __trace_wdata[3], __trace_wdata[2], __trace_wdata[1], __trace_wdata[0]};
   end // always @ (posedge clock)

   assign trace_valid = __trace_valid_reg;
   assign trace_iaddr = __trace_iaddr_reg;
   assign trace_insn = __trace_insn_reg;
   assign trace_priv = __trace_priv_reg;
   assign trace_exception = __trace_exception_reg;
   assign trace_interrupt = __trace_interrupt_reg;
   assign trace_cause = __trace_cause_reg;
   assign trace_tval = __trace_tval_reg;
   assign trace_wdata = __trace_wdata_reg;

   always @(negedge clock) begin
      r_reset <= reset;
      if (!reset && !r_reset) begin
         debug_rob_pop_trace(hartid,
                             __trace_valid, __trace_iaddr, __trace_insn,
                             __trace_priv, __trace_exception, __trace_interrupt,
                             __trace_cause, __trace_tval, __trace_wdata);
      end
   end
endmodule; // DebugROBPopTrace

