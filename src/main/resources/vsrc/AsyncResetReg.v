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
                      output     q,
                      input      en,

                      input      clk,
                      input      rst);

   reg                           q_reg;

   // There is a lot of initialization
   // here you don't normally find in Verilog
   // async registers because of scenarios in which reset
   // is not actually asserted cleanly at time 0,
   // and we want to make sure to properly model
   // that, yet Chisel codebase is absolutely intolerant
   // of Xs.
   
   initial begin
`ifdef RANDOMIZE
      integer                    initvar;
      reg [31:0]                 _RAND;
      _RAND = {1{$random}};
`endif // RANDOMIZE
      if (rst) begin
        q_reg = 1'b0;
      end 
`ifdef RANDOMIZE
 `ifdef RANDOMIZE_REG_INIT
      else begin
  `ifndef verilator
         #0.002 begin end
  `endif // verilator
         // We have to check for rst again
         // otherwise we initialize this
         // even though rst is asserted.
         if (~rst)
           q_reg = _RAND[0];
      end
 `endif // RANDOMIZE_REG_INIT
`endif // RANDOMIZE
   end
   
   always @(posedge clk or posedge rst) begin

      if (rst) begin
         q_reg <= 1'b0;
      end else if (en) begin
         q_reg <= d;
      end
   end

   assign q = rst ? 1'b0 :  q_reg;
 
endmodule // AsyncResetReg

