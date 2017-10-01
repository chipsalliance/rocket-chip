// See LICENSE.SiFive for license details.

/** This black-boxes an Async Reset
  * Reg.
  *  
  * Because Chisel doesn't support
  * parameterized black boxes, 
  * we unfortunately have to 
  * instantiate a number of these.
  *  
  * We also have to hard-code the set/reset.
  *  
  *  Do not confuse an asynchronous
  *  reset signal with an asynchronously
  *  reset reg. You should still 
  *  properly synchronize your reset 
  *  deassertion.
  *  
  *  @param d Data input
  *  @param q Data Output
  *  @param clk Clock Input
  *  @param rst Reset Input
  *  @param en Write Enable Input
  *  
  */

`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif

module AsyncResetReg (
                      input      d,
                      output reg q,
                      input      en,

                      input      clk,
                      input      rst);
   
`ifdef RANDOMIZE
   integer                       initvar;
   reg [31:0]                    _RAND;
   initial begin
`ifndef verilator
      #0.002 begin end
`endif
`ifdef RANDOMIZE_REG_INIT
      _RAND = {1{$random}};
      if (~rst) begin
         q = _RAND[0];
      end
`endif
   end
`endif //  `ifdef RANDOMIZE
   
   always @(posedge clk or posedge rst) begin
      
      if (rst) begin
         q <= 1'b0;
      end else if (en) begin
         q <= d;
      end
   end
   
endmodule // AsyncResetReg

