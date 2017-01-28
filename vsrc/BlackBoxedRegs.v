// See LICENSE.SiFive for license details.

/** This black-boxes a configurable number
  * of configurable width registers
  *  
  *  @param i Index Input
  *  @param d Data Input
  *  @param q Vector Data Output
  *  @param en Write Enable Input
  *  @param clk Clock Input
  *  
  */

module BlackBoxedRegs #(
  parameter int NREGS=1,
  parameter int WIDTH=1,
  parameter int IDXWIDTH = 1
) (
                      input  [IDXWIDTH-1:0] i,
                      input  [WIDTH-1:0] d,
                      output [WIDTH-1:0] q,
                      input  en,
                      input  clk);

    reg [WIDTH-1:0] regs [0:NREGS-1];

    always @(posedge clk) begin
        if (en) begin
            regs[i] <= d;
        end
    end

    assign q = regs[i];
   
endmodule // BlackBoxedRegs

