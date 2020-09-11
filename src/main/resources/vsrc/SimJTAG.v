// See LICENSE.SiFive for license details.
//VCS coverage exclude_file
import "DPI-C" function int jtag_tick
(
 output bit jtag_TCK,
 output bit jtag_TMS,
 output bit jtag_TDI,
 output bit jtag_TRSTn,
 
 input bit  jtag_TDO
);

module SimJTAG #(
                 parameter TICK_DELAY = 50
                 )(

                   input         clock,
                   input         reset,
                   
                   input         enable,
                   input         init_done,

                   output reg    jtag_TCK,
                   output reg    jtag_TMS,
                   output reg    jtag_TDI,
                   output reg    jtag_TRSTn,
 
                   input         jtag_TDO_data,
                   input         jtag_TDO_driven,
                          
                   output reg [31:0] exit
                   );

   reg [31:0]                    tickCounterReg;
   wire [31:0]                   tickCounterNxt;
   
   assign tickCounterNxt = (tickCounterReg == 0) ? TICK_DELAY :  (tickCounterReg - 1);
   
   bit          r_reset;

   wire [31:0]  random_bits = $random;
   
   wire         #0.1 __jtag_TDO = jtag_TDO_driven ? 
                jtag_TDO_data : random_bits[0];
      
   bit          __jtag_TCK;
   bit          __jtag_TMS;
   bit          __jtag_TDI;
   bit          __jtag_TRSTn;
   int          __exit;

   reg          init_done_sticky;
   
   always @(negedge clock) begin
     jtag_TCK   <= __jtag_TCK;
     jtag_TMS   <= __jtag_TMS;
     jtag_TDI   <= __jtag_TDI;
     jtag_TRSTn <= __jtag_TRSTn;
     exit <= __exit;
   end

   always @(posedge clock) begin
      r_reset <= reset;
      if (reset || r_reset) begin
         __exit = 0;
         tickCounterReg <= TICK_DELAY;
         init_done_sticky <= 1'b0;
         __jtag_TCK = !__jtag_TCK;
      end else begin
         init_done_sticky <= init_done | init_done_sticky;
         if (enable && init_done_sticky) begin
            tickCounterReg <= tickCounterNxt;
            if (tickCounterReg == 0) begin
               __exit = jtag_tick(
                                  __jtag_TCK,
                                  __jtag_TMS,
                                  __jtag_TDI,
                                  __jtag_TRSTn,
                                  __jtag_TDO);
            end
         end // if (enable && init_done_sticky)
      end // else: !if(reset || r_reset)
   end // always @ (posedge clock)

endmodule
