

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

module AsyncResetReg (
                      input      d,
                      output reg q,
                      input      en,

                      input      clk,
                      input      rst);
`ifdef RANDOMIZE
  reg[31:0] GEN_ASYNC;
  initial begin
    `ifndef verilator
      #0.002 begin end
    `endif
  `ifdef RANDOMIZE_REG_INIT
  GEN_ASYNC = {1{$random}};
  q = GEN_ASYNC[0];
  `endif
  end
`endif

   always @(posedge clk or posedge rst) begin

      if (rst) begin
         q <= 1'b0;
      end else if (en) begin
         q <= d;
      end
   end
   

endmodule // AsyncResetReg

