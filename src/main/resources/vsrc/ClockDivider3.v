// See LICENSE.SiFive for license details.

/** This black-boxes a Clock Divider by 3.
  * The output clock is phase-aligned to the input clock.
  * Do NOT use this in synthesis; the duty cycle is 2:1.
  *
  * Because Chisel does not support
  * blocking assignments, it is impossible
  * to create a deterministic divided clock.
  *
  *  @param clk_out Divided Clock
  *  @param clk_in  Clock Input
  *
  */

module ClockDivider3 (output reg clk_out, input clk_in);

   reg delay;

   initial begin
     clk_out = 1'b0;
     delay = 1'b0;
   end

   always @(posedge clk_in) begin
      if (clk_out == 1'b0) begin
        clk_out = 1'b1;
        delay <= 1'b0;
      end else if (delay == 1'b1) begin
        clk_out = 1'b0;
        delay <= 1'b0;
      end else begin
        delay <= 1'b1;
      end
   end

endmodule // ClockDivider3
