

/** This black-boxes an Async Reset
  * Reg.
  *  
  * Because Chisel doesn't support
  * parameterized black boxes, 
  * we unfortunately have to 
  * instantiate a number of these.
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
  *  
  *  @param init Value to write at Reset. 
  *  This is a constant, 
  *  but this construction
  *  will likely make backend flows
  *  and lint tools unhappy.
  * 
  */

module AsyncResetReg (
                      input      d,
                      output reg q,

                      input      clk,
                      input      rst,
                      
                      input      init);

   always @(posedge clk or posedge rst) begin

      if (rst) begin
         q <= init;
      end else begin
         q <= d;
      end
   
   end
   

endmodule // AsyncResetReg

