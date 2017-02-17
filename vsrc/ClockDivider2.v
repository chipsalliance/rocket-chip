// See LICENSE.SiFive for license details.

/** This black-boxes a Clock Divider by 2.
  * The output clock is phase-aligned to the input clock.
  * If you use this in synthesis, make sure your sdc
  * declares that you want it to do the same.
  *
  * Because Chisel does not support
  * blocking assignments, it is impossible
  * to create a deterministic divided clock.
  *
  *  @param clk_out Divided Clock
  *  @param clk_in  Clock Input
  *
  */

module ClockDivider2 (output reg clk_out, input clk_in);

   initial clk_out = 1'b0;
   always @(posedge clk_in) begin
      clk_out = ~clk_out; // Must use =, NOT <=
   end

endmodule // ClockDivider2
